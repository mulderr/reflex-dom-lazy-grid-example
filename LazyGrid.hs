{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyGrid where

import Control.Monad (forM_, when, liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.List (sortBy)
import Data.Map as Map
import Data.Monoid ((<>))
import Data.Time.Clock
import qualified Data.Vector as V

import Reflex
import Reflex.Dom

import GHCJS.DOM.Element hiding (drop)


data Column k v = Column
  { colHeader :: String
  , colValue :: (k, k) -> v -> String                      -- ^ column string value for display, can use row key and value
  , colCompare :: Maybe (v -> v -> Ordering)          -- ^ would it be nicer to just use ord or do we need more flexibility?
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



-- Lazy grid - based on virtualList code.
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
-- TODO:
-- - probably use virtual list style diff Event for updates instead of a Dynamic window, how much does it affect peformance?
grid :: (MonadWidget t m, Ord k, Show k, Default k, Enum k, Num k, Show v)
  => String                                 -- ^ css class applied to <div> container
  -> String                                 -- ^ css class applied to <table>
  -> Int                                    -- ^ row height in px
  -> Int                                    -- ^ extra rows rendered on top and bottom
  -> Dynamic t (Map k (Column k v))         -- ^ column spec
  -> Dynamic t (Map (k, k) v)                    -- ^ rows
  -> m ()
grid containerClass tableClass rowHeight extra dcols drows = do
  rec pb <- getPostBuild
      
      -- listWithKey :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
      (gridResizeEvent, (tbody, dcontrols)) <- resizeDetectorAttr ("class" =: containerClass) $
        elClass "table" tableClass $ do
          dcontrols <- el "thead" $ el "tr" $ listWithKey dcols $ \k dc ->
            sample (current dc) >>= \c -> el "th" $ do

              -- header and sort controls
              (sortEl, _) <- elAttr' "div" ("class" =: "col-title") $ do
                text (colHeader c)
                elClass "span" "sort-icon" $ (dynText =<< mapDyn (toSortIndicator k) sortState)

              -- filter controls
              dfilter <- case colFilter c of
                Just f -> return . _textInput_value =<< textInput (def & attributes .~ constDyn (mconcat [ "class" =: "grid-filter" ]))
                Nothing -> return $ constDyn $ ""

              let sortEvent = tag (constant k) . domEvent Click $ sortEl

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

      rowCount <- mapDyn size dxs

      scrollTop <- holdDyn 0 =<< debounce scrollDebounceDelay (domEvent Scroll tbody)
      params <- combineDyn (,) scrollTop tbodyHeight
      window <- combineDyn toWindow params dxs

      -- debug
      performEvent $ fmap (liftIO . print) $ updated window

      rowgroupAttrs <- combineDyn toRowgroupAttrs scrollTop rowCount

      resizeE <- performEvent . mapHeight tbody =<< debounce scrollDebounceDelay gridResizeEvent
      initHeightE <- performEvent . mapHeight tbody $ pb
      tbodyHeight <- holdDyn 0 $ leftmost [resizeE, initHeightE]

      -- joinDynThroughMap :: forall t k a. (Reflex t, Ord k) => Dynamic t (Map k (Dynamic t a)) -> Dynamic t (Map k a)
      -- split controls into filters and sort events
      dfs <- return . joinDynThroughMap =<< mapDyn (Map.map fst) dcontrols
      ess <- mapDyn (Map.map snd) dcontrols -- Dynamic t (Map k (Event t0 k))

      drc <- combineDyn (,) drows dcols
      filtered <- combineDyn applyFilters drc dfs

      drc' <- combineDyn (,) filtered dcols
      sortState <- toSortState . switchPromptlyDyn =<< mapDyn (leftmost . Map.elems) ess
      dxs <- combineDyn applySort drc' sortState

  return ()

  where
    scrollDebounceDelay = 0.04 -- 40ms caps it at around 25Hz
    toStyleAttr m = "style" =: (Map.foldrWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)

    mapHeight el = fmap (const $ liftIO $ elementGetOffsetHeight $ _el_element el)

    -- always start the window with odd row not to have the zebra "flip" when using css :nth-child
    toWindow (scrollTop, tbodyHeight) =
      let d = scrollTop `div` rowHeight - extra
          x = fromEnum $ odd d
          skip = d - x
          wsize = (ceiling tbodyHeight) `div` rowHeight + 1 + x + 2*extra
      in Map.fromList . take wsize . drop skip . Map.toList

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

    applyFilters (xs, cols) cs = Map.foldrWithKey (applyOne cols) xs cs
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

    -- note to self:
    -- listWithKey performs equality checks based on keys not values!
    -- If you dont update keys it will not re-render items
    applySort (xs, cols) (k, sortOrder) =
      case (maybeFunc k cols) of
        Nothing -> xs
        Just f -> let es = Map.elems xs
                      ks = Map.keys xs
                  in Map.fromList $ reorder $ f $ Map.toList xs
      where
        maybeFunc k cols = Map.lookup k cols >>= colCompare >>= \f ->
          let f' = (\(_, v1) (_, v2) -> f v1 v2)
          in case sortOrder of
            SortNone -> Nothing
            SortAsc -> return $ sortBy f'
            SortDesc -> return $ sortBy (flip f')
        reorder = zipWith (\n ((k1, k2), v) -> ((n, k2), v)) [1..]

    toSortIndicator k (ck, v) = if ck == k
      then case v of
             SortNone -> ""
             SortAsc -> "\x25be"  -- small triangle down
             SortDesc -> "\x25b4" -- small triangle up
      else ""


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
