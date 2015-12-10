{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

-- Lazy grid - based on virtualList code, styled after ui-grid.
--
-- Uses resizeDetector to keep track of height and renders as many rows as needed + extra.
--
-- Terminology:
-- - xs     - filtered sorted rows
-- - window - the rows to be rendered from xs
--
-- Why not reuse virtualList:
-- - hardocded divs break semantic markup
-- - positions each row absolute based on key which causes problems when filtering
-- - since we can assume there are no holes between rows we can instead position a container eg. rowgroup instead of each row speparately
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
--
module LazyGrid where

import Control.Lens ((^.))
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as AE
import Data.Default
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Traversable (forM)
import Text.CSV

import Reflex
import Reflex.Dom

import GHCJS.Foreign
import GHCJS.Marshal

import GHCJS.DOM.Element hiding (drop)
import qualified GHCJS.DOM.HTMLElement as HE
import GHCJS.DOM.EventM (on)

import GHCJS.DOM.Blob
import qualified GHCJS.DOM.Document as D
import GHCJS.DOM.URL
import GHCJS.DOM.Types (BlobPropertyBag (..), HTMLDocument, castToHTMLAnchorElement)


import Utils


type Columns k v = Map k (Column k v)
type Rows k v = Map (k, k) v
type Filters k = Map k String


data Column k v = Column
  { colHeader :: String
  , colValue :: (k, k) -> v -> String                     -- ^ column string value for display, can use row key and value
  , colCompare :: Maybe (v -> v -> Ordering)              -- ^ would it be nicer to just use ord or do we need more flexibility?
  , colFilter :: Maybe (String -> Rows k v -> Rows k v)   -- ^ filtering function
  , colVisible :: Bool
  , colAttrs :: Map String String
  }

instance Eq (Column k v) where
  x == y = colHeader x == colHeader y

instance Default (Column k v) where
  def = Column
    { colHeader = ""
    , colValue = (\_ _ -> "")
    , colCompare = Nothing
    , colFilter = Nothing
    , colVisible = True
    , colAttrs = Map.empty
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


data GridOrdering k = GridOrdering k SortOrder

instance Default k => Default (GridOrdering k) where
  def = GridOrdering def def


-- | Handles model changes in response to filtering or sorting.
gridManager :: (MonadWidget t m, Ord k, Enum k, Num k)
  => Event t (Columns k v, Rows k v, Filters k, GridOrdering k)
  -> m (Dynamic t (Rows k v))
gridManager =
  holdDyn Map.empty . fmap f
  where
    f (cols, rows, fs, order) = gridSort cols order $ gridFilter cols fs rows

-- | Apply filters to a set of rows.
gridFilter :: Ord k => Columns k v -> Filters k -> Rows k v -> Rows k v
gridFilter cols fs xs =
  Map.foldrWithKey (applyOne cols) xs fs
  where
    applyOne _ _ "" xs = xs
    applyOne cols k s xs = case Map.lookup k cols of
                             Nothing -> xs
                             Just c -> case colFilter c of
                                         Just f -> f s xs
                                         Nothing -> xs

-- | Apply column sorting to a set of rows.
gridSort :: (Num k, Ord k, Enum k) => Columns k v -> GridOrdering k -> Rows k v -> Rows k v
gridSort cols (GridOrdering k sortOrder) xs =
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


-- | Grid view.
grid :: forall t m k v a . (MonadWidget t m, Ord k, Default k, Enum k, Num k)
  => String                                 -- ^ css class applied to <div> container
  -> String                                 -- ^ css class applied to <table>
  -> Int                                    -- ^ row height in px
  -> Int                                    -- ^ extra rows rendered on top and bottom
  -> Dynamic t (Columns k v)                -- ^ columns
  -> Dynamic t (Rows k v)                   -- ^ rows
  -> (Columns k v -> (k, k) -> Dynamic t v -> m a) -- ^ row creating action
  -> m (Dynamic t (Rows k v))
grid containerClass tableClass rowHeight extra dcols drows mkRow = do
  pb <- getPostBuild
  rec (gridResizeEvent, (tbody, dcontrols, expE, expVisE, colToggles)) <- resizeDetectorAttr ("class" =: containerClass) $ do

        -- grid menu
        (expE, expVisE, colToggles) <- el "div" $ do
          (menuToggle, _) <- elAttr' "div" ("class" =: "grid-menu-toggle") $ return ()
          menuOpen <- toggle False $ domEvent Click menuToggle
          menuAttrs <- mapDyn (\o -> "class" =: if o then "grid-menu grid-menu-open" else "grid-menu") menuOpen

          elDynAttr "div" menuAttrs $ do
            elClass "ul" "grid-menu-list" $ do
              (exportEl, _) <- el' "li" $ text "Export all data as csv"
              (exportVisibleEl, _) <- el' "li" $ text "Export visible data as csv"
              toggles <- listWithKey dcols $ \k dc ->
                sample (current dc) >>= \c -> el "div" $ do
                  rec (toggleEl, _) <- elDynAttr' "li" attrs $ text $ colHeader c
                      dt <- toggle (colVisible c) (domEvent Click toggleEl :: Event t ())
                      attrs <- mapDyn (\v -> ("class" =: ("grid-menu-col " <> if v then "grid-menu-col-visible" else "grid-menu-col-hidden"))) dt
                  return dt
              return
                ( domEvent Click exportEl :: Event t ()
                , domEvent Click exportVisibleEl :: Event t ()
                , toggles
                )

        (tbody, dcontrols) <- elClass "table" tableClass $ do
          dcontrols <- el "thead" $ el "tr" $ listWithKey dcs $ \k dc ->
            sample (current dc) >>= \c -> elAttr "th" (colAttrs c) $ do

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
              ti <- textInputClearable "grid-col-filter-clear-btn" (def & attributes .~ constDyn ("class" =: "grid-col-filter" ))
              dfilter <- case colFilter c of
                Just f -> return $ _textInput_value ti
                Nothing -> return $ constDyn $ ""

              -- for each column we return:
              -- - filter string :: Dynamic t String
              -- - sort button event tagged with column key :: Event t Int
              return (dfilter, sortEvent)

          (tbody, _) <- el' "tbody" $
            elDynAttr "rowgroup" rowgroupAttrs $ do
              as <- forDyn dcs $ \cs ->
                listWithKey window $ \k dv ->
                  mkRow cs k dv
              dyn as

          return (tbody, dcontrols)

        return (tbody, dcontrols, expE, expVisE, colToggles)

      resizeE <- performEvent . mapElHeight tbody =<< debounce scrollDebounceDelay gridResizeEvent
      initHeightE <- performEvent . mapElHeight tbody $ pb
      tbodyHeight <- holdDyn 0 $ fmap ceiling $ leftmost [resizeE, initHeightE]
      scrollTop <- holdDyn 0 =<< debounce scrollDebounceDelay (domEvent Scroll tbody)

      -- joinDynThroughMap :: forall t k a. (Reflex t, Ord k) => Dynamic t (Map k (Dynamic t a)) -> Dynamic t (Map k a)
      -- split controls into filters and sort events
      dfs <- return . joinDynThroughMap =<< mapDyn (Map.map fst) dcontrols
      ess <- mapDyn (Map.map snd) dcontrols -- Dynamic t (Map k (Event t0 k))
      sortState <- toSortState . switchPromptlyDyn =<< mapDyn (leftmost . Map.elems) ess

      -- TODO:
      -- if the new filter value contains the old one (ex. user types another letter) we want to search within
      -- currenlty filtered items - as in keep existing work and not start from scratch
      -- could that be done with a fold of some kind?
      --
      -- also if we use filters on multiple columns we don't want to have to reapply all of them from
      -- scratch whenever something is added to any one of them
      --
      -- note we cannot avoid starting from scratch when we subtract something from any of the filters
      gridState <- combineDyn4 (,,,) dcols drows dfs sortState
      dxs <- gridManager $ updated gridState
      rowCount <- mapDyn Map.size dxs

      window <- combineDyn3 toWindow dxs scrollTop tbodyHeight
      rowgroupAttrs <- combineDyn toRowgroupAttrs scrollTop rowCount

      let colVisibility = joinDynThroughMap colToggles
      dcs <- mapDyn (Map.filter (== True)) colVisibility
        >>= combineDyn (Map.intersectionWith (\c _ -> c)) dcols

      exportCsv dcols $ tag (current drows) expE
      exportCsv dcols $ tag (current dxs) expVisE

  return dxs

  where
    scrollDebounceDelay = 0.04 -- 25Hz
    toStyleAttr m = "style" =: (Map.foldrWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)

    mapElHeight el = fmap (const $ liftIO $ getOffsetHeight $ _el_element el)

    -- always start the window with odd row not to have the zebra "flip" when using css :nth-child
    toWindow :: Rows k v -> Int -> Int -> Rows k v
    toWindow xs scrollTop tbodyHeight =
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

    -- whenever we switch to another column SortOrder is reset to SortAsc
    toSortState :: Event t k -> m (Dynamic t (GridOrdering k))
    toSortState = foldDyn f def
      where f k (GridOrdering pk v) = GridOrdering k (if k == pk then (nextSort v) else SortAsc)

    toSortIndicatorAttrs :: k -> GridOrdering k -> Map String String
    toSortIndicatorAttrs k (GridOrdering ck v) = "class" =: ("grid-col-sort-icon" <> if ck == k
      then case v of
             SortNone -> ""
             SortAsc -> " grid-col-sort-icon-asc"
             SortDesc -> " grid-col-sort-icon-desc"
      else "")

toCsv :: Columns k v -> Rows k v -> String
toCsv cols rows = printCSV $ toFields <$> Map.toList rows
  where toFields (k, x) = fmap (\c -> colValue c k x) cs
        cs = Map.elems cols

exportCsv :: MonadWidget t m => Dynamic t (Columns k v) -> Event t (Rows k v) -> m ()
exportCsv dcols e = do
  doc <- askDocument
  performEvent_ $ fmap (liftIO . triggerDownload doc "text/csv" "export.csv" . uncurry toCsv) $ attachDyn dcols e

-- an HTML5 way of locally triggering a file download with arbitrary content
-- only tested on recent versions of Chrome and Firefox
triggerDownload
  :: HTMLDocument
  -> String -- ^ mime type
  -> String -- ^ file name
  -> String -- ^ content
  -> IO ()
triggerDownload doc mime filename s = do
  windowUrl <- js_windowURL
  v <- toJSVal s
  p <- toJSVal $ AE.Object $ HM.singleton (T.pack "type") (AE.String $ T.pack mime)
  blob <- newBlob' [v] $ Just $ BlobPropertyBag p
  Just (url :: String) <- createObjectURL windowUrl (Just blob)
  Just a <- D.createElement doc (Just "a")
  setAttribute a "style" "display: none;"
  setAttribute a "download" filename
  setAttribute a "href" url
  HE.click $ castToHTMLAnchorElement a
  revokeObjectURL windowUrl url

-- for triggerDownload
-- cannot use newURL; createObjectURL is only defined for window.URL?
foreign import javascript unsafe "window[\"URL\"]"
        js_windowURL :: IO URL

-- | Text input with a button to clear the value.
-- The button content ie. icon or text is to be defined through CSS using btnClass.
textInputClearable :: MonadWidget t m => String -> TextInputConfig t -> m (TextInput t)
textInputClearable btnClass tic =
  elAttr "div" ("style" =: "position: relative;") $ do
    rec (e, _) <- elDynAttr' "span" attrs $ return ()
        let clearE = domEvent Click e
        ti <- textInput $ tic & setValue .~ fmap (\_ -> "") clearE
        attrs <- holdDyn emptyAttrs $ leftmost [ fmap (\_ -> emptyAttrs) clearE, fmap f $ ti ^. textInput_input]
    return ti
  where
    emptyAttrs = ("style" =: "visibility: hidden;")
    f s = case s of
            "" -> emptyAttrs
            _  -> ("class" =: btnClass)

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
  expandScroll <- wrapDomEvent (_el_element expand) (`on` scroll) $ return ()
  shrinkScroll <- wrapDomEvent (_el_element shrink) (`on` scroll) $ return ()
  size0 <- performEvent $ fmap (const $ liftIO reset) pb
  rec resize <- performEventAsync $ fmap (\d cb -> liftIO $ cb =<< resetIfChanged d) $ tag (current dimensions) $ leftmost [expandScroll, shrinkScroll]
      dimensions <- holdDyn (Nothing, Nothing) $ leftmost [ size0, fmapMaybe id resize ]
  return (fmap (const ()) $ fmapMaybe id resize, w')
