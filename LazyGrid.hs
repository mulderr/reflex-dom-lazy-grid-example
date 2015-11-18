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
  , colValue :: k -> v -> String                      -- ^ column string value for display, can use row key and value
  , colCompare :: Maybe (v -> v -> Ordering)          -- ^ would it be nicer to just use ord or do we need more flexibility?
  , colFilter :: Maybe (String -> Map k v -> Map k v) -- ^ filtering function
  , colWidth :: Maybe Int
  , colVisible :: Bool
  }

instance Default (Column k v) where
  def = Column
    { colHeader = ""
    , colValue = (\_ _ -> "")
    , colCompare = Nothing
    , colFilter = Nothing
    , colWidth = Nothing
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



-- Lazy grid - based on virtualList code but without fixed height.
-- 
-- Uses resizeDetector to keep track of height and renders as many rows as needed.
grid :: (MonadWidget t m, Ord k, Show k, Default k, Show v)
  => String                                 -- ^ css class applied to <div> container
  -> String                                 -- ^ css class applied to <table>
  -> Int                                    -- ^ row height in px
  -> Int                                    -- ^ extra rows rendered on top and bottom
  -> Dynamic t (Map k (Column k v))         -- ^ column spec
  -> Dynamic t (Map k v)                    -- ^ rows
  -> m ()
grid containerClass tableClass rowHeight extra dcols drows = do
  rec -- circular refs:
      -- window > pos > scrollTop > tbody > window
      -- rowgroupAttrs > scrollTop > tbody > rowgroupAttrs
      -- ...

      pb <- getPostBuild
      performEvent $ fmap (\xs -> liftIO $ print "yoink dcols") $ updated dcols

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
              -- debug
              performEvent $ fmap (\_ -> do
                (k, so) <- sample $ current sortState
                liftIO $ print $ "sort column " <> show k <> " " <> show so
                ) sortEvent

              -- for each column we return:
              -- - filter string :: Dynamic t String
              -- - sort button event tagged with column key :: Event t Int
              return (dfilter, sortEvent)

          (tbody, _) <- el' "tbody" $
            elDynAttr "rowgroup" rowgroupAttrs $ do

              listWithKey window $ \k dv -> sample (current dv) >>= \v -> do
                -- debug
                performEvent $ fmap (\xs -> liftIO $ print "yoink row") $ updated dv

                -- apparently this part does not always run when window is updated... um, why?
                x <- el "tr" $ listWithKey dcols $ \_ dc ->
                  sample (current dc) >>= \c ->
                    el "td" $ text ((colValue c) k v)

                -- debug - see above, this should be printed when sorting but isnt
                performEvent $ fmap (\xs -> liftIO $ print "yoink x") $ updated x

                return ()

              return ()

          return (tbody, dcontrols)

      rowCount <- mapDyn size dxs

      scrollTop <- holdDyn 0 =<< debounce scrollDebounceDelay (domEvent Scroll tbody)
      params <- combineDyn (,) scrollTop tbodyHeight
      window <- combineDyn toWindow params dxs

      -- debug
      performEvent $ fmap (\xs -> liftIO $ print "yoink window") $ updated window

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

      --performEvent $ fmap (\xs -> liftIO $ print $ show xs) $ updated window

  return ()

  where
    scrollDebounceDelay = 0.04 -- 40ms caps it at around 25Hz
    toStyleAttr m = "style" =: (Map.foldrWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)

    mapHeight el = fmap (const $ liftIO $ getOffsetHeight $ _el_element el)

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
    toSortState = foldDyn (\k (pk, v) -> if k == pk then (k, nextSort v) else (k, SortAsc)) (def, def)

    applySort (xs, cols) (k, sortOrder) =
      case (maybeFunc k cols) of
        Nothing -> xs
        Just f -> let es = Map.elems xs
                      ks = Map.keys xs
                  in Map.fromList $ zip ks $ f es
      where maybeFunc k cols = Map.lookup k cols >>= colCompare >>= \f ->
              case sortOrder of
                SortNone -> Nothing
                SortAsc -> return $ sortBy f
                SortDesc -> return $ sortBy (flip f)

    toSortIndicator k (ck, v) = if ck == k
      then case v of
             SortNone -> ""
             SortAsc -> "\x25be"  -- small triangle down
             SortDesc -> "\x25b4" -- small triangle up
      else def




--
-- TODO: copied from current develop branch, remove after updating reflex-dom
--

-- | Block occurrences of an Event until th given number of seconds elapses without
--   the Event firing, at which point the last occurrence of the Event will fire.
debounce :: MonadWidget t m => NominalDiffTime -> Event t a -> m (Event t a)
debounce dt e = do
  n :: Dynamic t Integer <- count e
  let tagged = attachDynWith (,) n e
  delayed <- delay dt tagged
  return $ attachWithMaybe (\n' (t, v) -> if n' == t then Just v else Nothing) (current n) delayed


-- Changelog
-- * updated for ghcjs 0.2
-- * added resizeDetectorAttr as the most general version
-- * removed hardcoded position: relative - one may want to specify position: absolute
-- * replaced wrapDomEvent with domEvent - i admit i dont know why would one use one over the other
--
-- | A widget that wraps the given widget in a div and fires an event when resized.
--   Adapted from github.com/marcj/css-element-queries
resizeDetector :: MonadWidget t m => m a -> m (Event t (), a)
resizeDetector = resizeDetectorAttr Map.empty

resizeDetectorWithStyle :: MonadWidget t m
  => String -- ^ A css style string. Warning: It must contain the "position" attribute with value either "absolute" or "relative".
  -> m a
  -> m (Event t (), a)
resizeDetectorWithStyle styleString = resizeDetectorAttr ("style" =: styleString)

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
        eow <- getOffsetWidth e
        eoh <- getOffsetHeight e
        let ecw = eow + 10
            ech = eoh + 10
        setAttribute (_el_element expandChild) "style" (childStyle <> "width: " <> show ecw <> "px;" <> "height: " <> show ech <> "px;")
        esw <- getScrollWidth e
        setScrollLeft e esw
        esh <- getScrollHeight e
        setScrollTop e esh
        ssw <- getScrollWidth s
        setScrollLeft s ssw
        ssh <- getScrollHeight s
        setScrollTop s ssh
        lastWidth <- getOffsetWidth (_el_element parent)
        lastHeight <- getOffsetHeight (_el_element parent)
        return (Just lastWidth, Just lastHeight)
      resetIfChanged ds = do
        pow <- getOffsetWidth (_el_element parent)
        poh <- getOffsetHeight (_el_element parent)
        if ds == (Just pow, Just poh)
           then return Nothing
           else liftM Just reset
  pb <- getPostBuild
  let expandScroll = domEvent Scroll expand
      shrinkScroll = domEvent Scroll shrink
  size0 <- performEvent $ fmap (const $ liftIO reset) pb
  rec resize <- performEventAsync $ fmap (\d cb -> liftIO $ cb =<< resetIfChanged d) $ tag (current dimensions) $ leftmost [expandScroll, shrinkScroll]
      dimensions <- holdDyn (Nothing, Nothing) $ leftmost [ size0, fmapMaybe id resize ]
  return (fmap (const ()) $ fmapMaybe id resize, w')
