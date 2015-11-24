{-# LANGUAGE RecursiveDo #-}
module LazyGrid where

import Control.Monad (forM_, when, liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.List (sortBy)
import Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Vector as V

import Reflex
import Reflex.Dom

import GHCJS.DOM.Element hiding (drop)


data Column k v = Column
  { colHeader :: String
  , colValue :: (k, k) -> v -> String                           -- ^ column string value for display, can use row key and value
  , colCompare :: Maybe (v -> v -> Ordering)                    -- ^ would it be nicer to just use ord or do we need more flexibility?
  , colFilter :: Maybe (String -> Map (k, k) v -> Map (k, k) v) -- ^ filtering function
  , colVisible :: Bool
  }

instance Default (Column k v) where
  def = Column
    { colHeader = ""
    , colValue = (\_ _ -> "")
    , colCompare = Nothing
    , colFilter = Nothing
    , colVisible = True
    }

data SortOrder
  = SortNone
  | SortAsc
  | SortDesc
  deriving (Eq, Show, Enum)

instance Default SortOrder where
  def = SortNone

nextSort :: SortOrder -> SortOrder
nextSort SortDesc = SortNone
nextSort s = succ s



-- Lazy grid - based on virtualList code, styled after ui-grid.
-- 
-- Uses resizeDetector to keep track of height and renders as many rows as needed + extra.
--
-- Terminology:
-- - window - the rows to be rendered given tbody height, scroll position and (filtered, sorted) data
--
-- Why not reuse virtualList:
-- - hardocded divs break semantic markup
-- - positions each row absolute based on key which causes problems when filtering
-- - since we can assume there are no holes between rows we can instead position a container eg. rowgroup instead of each row
-- - we can keep track of the details ourselves and implement weird guarantees eg. we always start with odd row to allow :nth-child zebra
--
-- Whats with the tuple key?
-- - listWithKey does a shallow diff on keys and will not re-render rows unless the keys change
-- - with Map k v sorting does not change the set of keys in any way thus the dom would not be updated
-- - to work around the above we use a composite key (k, k) first component is used for ordering (see Ord for (a, a)), second
--   is the row id (or original position), this way the map is always ordered according to current sort but listWithKey will
--   notice changes
-- - yes, it is awkard, i wonder if using listWithKeyShallowDiff would make this go away
--
-- TODO:
-- - probably use listWithKeyShallowDiff instead of a Dynamic window, how much does it affect peformance?
-- - performance tuning
-- - column selection
-- - export to csv
grid :: (MonadWidget t m, Ord k, Default k, Enum k, Num k, Show v)
  => String                                 -- ^ css class applied to <div> container
  -> String                                 -- ^ css class applied to <table>
  -> Int                                    -- ^ row height in px
  -> Int                                    -- ^ extra rows rendered on top and bottom
  -> Dynamic t (Map k (Column k v))         -- ^ column spec
  -> Dynamic t (Map (k, k) v)               -- ^ rows
  -> m ()
grid containerClass tableClass rowHeight extra dcols drows = do
  pb <- getPostBuild
  rec (gridResizeEvent, (tbody, dcontrols)) <- resizeDetectorAttr ("class" =: containerClass) $ do
        -- grid menu stub
        -- ghcjs cannot figure out t = Spider for Reflex t
        (expE, expVisE, colToggles) <- el "div" $ do
          (menuToggle, _) <- elAttr' "div" ("class" =: "grid-menu-toggle") $ return ()
          menuOpen <- toggle False $ domEvent Click menuToggle
          menuAttrs <- mapDyn (\o -> "class" =: if o then "grid-menu grid-menu-open" else "grid-menu") menuOpen

          elDynAttr "div" menuAttrs $ do
            elClass "ul" "grid-menu-list" $ do
              exportEl <- el' "li" $ text "Export all data as csv"
              exportVisibleEl <- el' "li" $ text "Export filtered data as csv"
              toggles <- listWithKey dcols $ \k dc ->
                sample (current dc) >>= \c -> el "div" $ do
                  (toggleEl, _) <- elAttr' "li" ("class" =: "grid-menu-col grid-menu-col-visible") $ text $ colHeader c
                  return toggleEl -- $ domEvent Click toggleEl
              return (exportEl, exportVisibleEl, toggles)

        elClass "table" tableClass $ do
          dcontrols <- el "thead" $ el "tr" $ listWithKey dcols $ \k dc ->
            sample (current dc) >>= \c -> el "th" $ do

              -- header and sort controls
              let headerClass = maybe "grid-col-title" (const "grid-col-title grid-col-title-sort") (colCompare c)
              sortAttrs <- mapDyn (toSortIndicatorAttrs k) sortState
              (sortEl, _) <- elAttr' "div" ("class" =: headerClass) $ do
                text (colHeader c)
                elDynAttr "span" sortAttrs $ return ()

              let sortEvent = case colCompare c of
                                Just _ -> tag (constant k) $ domEvent Click sortEl
                                Nothing -> never

              -- filter controls
              dfilter <- case colFilter c of
                Just f -> return . _textInput_value =<< textInput (def & attributes .~ constDyn ("class" =: "grid-col-filter" ))
                Nothing -> return $ constDyn $ ""

              -- for each column we return:
              -- - filter string :: Dynamic t String
              -- - sort button event tagged with column key :: Event t Int
              return (dfilter, sortEvent)

          (tbody, _) <- el' "tbody" $
            elDynAttr "rowgroup" rowgroupAttrs $
              listWithKey window $ \k dv -> sample (current dv) >>= \v -> do
                el "tr" $ listWithKey dcols $ \_ dc ->
                  sample (current dc) >>= \c ->
                    el "td" $ text ((colValue c) k v)

          return (tbody, dcontrols)

      resizeE <- performEvent . mapElHeight tbody =<< debounce scrollDebounceDelay gridResizeEvent
      initHeightE <- performEvent . mapElHeight tbody $ pb
      tbodyHeight <- holdDyn 0 $ fmap ceiling $ leftmost [resizeE, initHeightE]
      scrollTop <- holdDyn 0 =<< debounce scrollDebounceDelay (domEvent Scroll tbody)

      -- joinDynThroughMap :: forall t k a. (Reflex t, Ord k) => Dynamic t (Map k (Dynamic t a)) -> Dynamic t (Map k a)
      -- split controls into filters and sort events
      dfs <- return . joinDynThroughMap =<< mapDyn (Map.map fst) dcontrols
      ess <- mapDyn (Map.map snd) dcontrols -- Dynamic t (Map k (Event t0 k))
      sortState <- toSortState . switchPromptlyDyn =<< mapDyn (leftmost . Map.elems) ess

      dxs <- combineDyn (,) dcols drows
          >>= combineDyn applyFilters dfs
          >>= combineDyn applySort sortState

      rowCount <- mapDyn size dxs

      window <- combineDyn (,) scrollTop tbodyHeight >>= combineDyn toWindow dxs
      rowgroupAttrs <- combineDyn toRowgroupAttrs scrollTop rowCount

  return ()

  where
    scrollDebounceDelay = 0.04
    toStyleAttr m = "style" =: (Map.foldrWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)

    mapElHeight el = fmap (const $ liftIO $ elementGetOffsetHeight $ _el_element el)

    -- always start the window with odd row not to have the zebra "flip" when using css :nth-child
    toWindow :: Ord k => Map (k, k) v -> (Int, Int) -> Map (k, k) v
    toWindow xs (scrollTop, tbodyHeight) =
      let d = scrollTop `div` rowHeight - extra
          x = fromEnum $ odd d
          skip = d - x
          wsize = tbodyHeight `div` rowHeight + 1 + x + 2*extra
      in Map.fromList . take wsize . drop skip . Map.toList $ xs

    toRowgroupAttrs :: Int -> Int -> Map String String
    toRowgroupAttrs scrollTop rowCount = 
      let total = rowCount * rowHeight
          (d, pad) = scrollTop `divMod` rowHeight
          x = fromEnum $ odd d
          woffset = capAtZero $ scrollTop - pad - (extra + x) * rowHeight
          wheight = total - woffset
          capAtZero x = if x < 0 then 0 else x
      in toStyleAttr $ "position" =: "relative"
                    <> "overflow" =: "hidden"
                    <> "top"      =: (show woffset <> "px")
                    <> "height"   =: (show wheight <> "px")

    applyFilters :: Ord k => Map k String -> (Map k (Column k v), Map (k, k) v) -> (Map k (Column k v), Map (k, k) v)
    applyFilters fs (cols, xs) = (cols, Map.foldrWithKey (applyOne cols) xs fs)
    applyOne _ _ "" xs = xs
    applyOne cols k s xs = case Map.lookup k cols of
                              Just c -> case colFilter c of
                                          Just f -> f s xs
                                          Nothing -> xs
                              Nothing -> xs

    -- takes an Event t k and returns a Dynamic t (k, SortOrder) with the current sort state
    -- - k is the column key
    -- - only one column can be sorted at a time
    -- - whenever we switch to another column SortOrder is reset to SortAsc
    toSortState :: (MonadWidget t m, Eq k, Default k) => Event t k -> m (Dynamic t (k, SortOrder))
    toSortState = foldDyn (\k (pk, v) -> if k == pk then (k, nextSort v) else (k, SortAsc)) (def, def)

    applySort :: (Num k, Ord k, Enum k) => (k, SortOrder) -> (Map k (Column k v), Map (k, k) v) -> Map (k, k) v
    applySort (k, sortOrder) (cols, xs) =
      case (maybeSortFunc k cols) of
        Nothing -> xs
        Just f -> Map.fromList $ reorder $ f $ Map.toList xs
      where
        maybeSortFunc k cols = Map.lookup k cols >>= colCompare >>= \f ->
          let f' = (\(_, v1) (_, v2) -> f v1 v2)
          in case sortOrder of
            SortNone -> Nothing
            SortAsc -> return $ sortBy f'
            SortDesc -> return $ sortBy (flip f')
        reorder = zipWith (\n ((_, k2), v) -> ((n, k2), v)) [1..]

    toSortIndicatorAttrs :: Eq k => k -> (k, SortOrder) -> Map String String
    toSortIndicatorAttrs k (ck, v) = "class" =: ("grid-col-sort-icon" <> if ck == k
      then case v of
             SortNone -> ""
             SortAsc -> " grid-col-sort-icon-asc"
             SortDesc -> " grid-col-sort-icon-desc"
      else "")


-- more general version of resizeDetectorWithStyle
-- need to specify class
-- caller is responsible for somehow setting position: relative or position: absolute
resizeDetectorAttr :: MonadWidget t m
  => Map String String -- ^ Element attributes. Warning: It must specifiy the "position" attribute with value either "absolute" or "relative".
  -> m a -- ^ The embedded widget
  -> m (Event t (), a) -- ^ An 'Event' that fires on resize, and the result of the embedded widget
resizeDetectorAttr attrs w = do
  let childStyle = "position: absolute; left: 0; top: 0;"
      containerAttrs = "style" =: "position: absolute; left: 0; top: 0; right: 0; bottom: 0; overflow: scroll; z-index: -1; visibility: hidden;"
  (parent, (expand, expandChild, shrink, w')) <- elAttr' "div" attrs $ do
    w' <- w
    elAttr "div" containerAttrs $ do
      (expand, (expandChild, _)) <- elAttr' "div" containerAttrs $ elAttr' "div" ("style" =: childStyle) $ return ()
      (shrink, _) <- elAttr' "div" containerAttrs $ elAttr "div" ("style" =: (childStyle <> "width: 200%; height: 200%;")) $ return ()
      return (expand, expandChild, shrink, w')
  let reset = do
        let e = _el_element expand
            s = _el_element shrink
        eow <- elementGetOffsetWidth e
        eoh <- elementGetOffsetHeight e
        let ecw = eow + 10
            ech = eoh + 10
        elementSetAttribute (_el_element expandChild) "style" (childStyle <> "width: " <> show ecw <> "px;" <> "height: " <> show ech <> "px;")
        esw <- elementGetScrollWidth e
        elementSetScrollLeft e esw
        esh <- elementGetScrollHeight e
        elementSetScrollTop e esh
        ssw <- elementGetScrollWidth s
        elementSetScrollLeft s ssw
        ssh <- elementGetScrollHeight s
        elementSetScrollTop s ssh
        lastWidth <- elementGetOffsetWidth (_el_element parent)
        lastHeight <- elementGetOffsetHeight (_el_element parent)
        return (Just lastWidth, Just lastHeight)
      resetIfChanged ds = do
        pow <- elementGetOffsetWidth (_el_element parent)
        poh <- elementGetOffsetHeight (_el_element parent)
        if ds == (Just pow, Just poh)
           then return Nothing
           else liftM Just reset
  pb <- getPostBuild
  expandScroll <- wrapDomEvent (_el_element expand) elementOnscroll $ return ()
  shrinkScroll <- wrapDomEvent (_el_element shrink) elementOnscroll $ return ()
  size0 <- performEvent $ fmap (const $ liftIO reset) pb
  rec resize <- performEventAsync $ fmap (\d cb -> liftIO $ cb =<< resetIfChanged d) $ tag (current dimensions) $ leftmost [expandScroll, shrinkScroll]
      dimensions <- holdDyn (Nothing, Nothing) $ leftmost [ size0, fmapMaybe id resize ]
  return (fmap (const ()) $ fmapMaybe id resize, w')
