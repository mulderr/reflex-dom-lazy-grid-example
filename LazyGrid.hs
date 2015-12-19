{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module LazyGrid 
  ( Rows
  , Columns
  , Column (..)
  , grid
  ) where

import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           Data.List (sortBy)
import           Data.Maybe (isJust)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Time.Clock (NominalDiffTime)
import           Text.CSV

import           GHCJS.DOM.Element (getOffsetHeight)

import           Reflex
import           Reflex.Dom

import           Utils
import           DomUtils


type Columns k v = Map k (Column k v)
type Rows k v = Map (k, k) v
type Filters k = Map k String

-- | Column specification.
data Column k v = Column
  { colName :: String                                     -- ^ column name
  , colHeader :: String                                   -- ^ column header
  , colValue :: (k, k) -> v -> String                     -- ^ column string value for display, can use row key and value
  , colCompare :: Maybe (v -> v -> Ordering)              -- ^ ordering function
  , colFilter :: Maybe (String -> Rows k v -> Rows k v)   -- ^ filtering function
  , colVisible :: Bool                                    -- ^ initial visibility
  , colAttrs :: Map String String                         -- ^ element attrs applied to <th> and available for use in row action
  }

instance Eq (Column k v) where
  x == y = colName x == colName y

instance Default (Column k v) where
  def = Column
    { colName = ""
    , colHeader = ""
    , colValue = (\_ _ -> "")
    , colCompare = Nothing
    , colFilter = Nothing
    , colVisible = True
    , colAttrs = Map.empty
    }

-- | Column ordering.
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


gridMenu :: forall t m k v . (MonadWidget t m, Ord k)
  => Dynamic t (Columns k v) -- ^ columns
  -> m
     ( Event t () -- ^ export data event
     , Event t () -- ^ export visible data event
     , Event t () -- ^ export selected data event
     , Dynamic t (Map k (Dynamic t Bool)) -- ^ column visibility
     )
gridMenu dcols = el "div" $ do
  (menuToggle, _) <- elAttr' "div" ("class" =: "grid-menu-toggle") $ return ()
  menuOpen <- toggle False $ domEvent Click menuToggle
  menuAttrs <- mapDyn (\o -> "class" =: if o then "grid-menu grid-menu-open" else "grid-menu") menuOpen

  elDynAttr "div" menuAttrs $ do
    elClass "ul" "grid-menu-list" $ do
      (exportEl, _) <- el' "li" $ text "Export all data as csv"
      (exportVisibleEl, _) <- el' "li" $ text "Export visible data as csv"
      (exportSelectedEl, _) <- el' "li" $ text "Export selected data as csv"
      toggles <- listWithKey dcols $ \k dc ->
        sample (current dc) >>= \c -> el "div" $ do
          rec (toggleEl, _) <- elDynAttr' "li" attrs $ text $ colHeader c
              dt <- toggle (colVisible c) (domEvent Click toggleEl :: Event t ())
              attrs <- mapDyn (\v -> ("class" =: ("grid-menu-col " <> if v then "grid-menu-col-visible" else "grid-menu-col-hidden"))) dt
          return dt
      return
        ( domEvent Click exportEl
        , domEvent Click exportVisibleEl
        , domEvent Click exportSelectedEl
        , toggles
        )


gridHead :: forall t m k v . (MonadWidget t m, Ord k)
  => Dynamic t (Columns k v)      -- ^ columns
  -> Dynamic t (GridOrdering k)   -- ^ ordering
  -> m (Dynamic t (Map k (Dynamic t String, Event t k)))  -- ^ column filters and sort events
gridHead dcs dordering = el "thead" $ el "tr" $ listWithKey dcs $ \k dc ->
  sample (current dc) >>= \c -> elAttr "th" (colAttrs c) $ do
    -- header and sort controls
    let headerClass = maybe "grid-col-title" (const "grid-col-title grid-col-title-sort") (colCompare c)
    sortAttrs <- mapDyn (toSortIndicatorAttrs k) dordering
    (sortEl, _) <- elAttr' "div" ("class" =: headerClass) $ do
      text (colHeader c)
      elDynAttr "span" sortAttrs $ return ()

    let sortEvent = case colCompare c of
                      Just _ -> tag (constant k) $ domEvent Click sortEl
                      Nothing -> never

    -- filter controls
    dfilter <- case colFilter c of
      Just f -> do
        ti <- textInputClearable "grid-col-filter-clear-btn" (def & attributes .~ constDyn ("class" =: "grid-col-filter" ))
        return $ _textInput_value ti
      Nothing -> return $ constDyn $ ""

    return (dfilter, sortEvent)

  where
    -- given column key k and GridOrdering k return sort indicator attrs for that column
    toSortIndicatorAttrs :: k -> GridOrdering k -> Map String String
    toSortIndicatorAttrs k (GridOrdering ck v) = "class" =: ("grid-col-sort-icon" <> if ck == k
      then case v of
             SortNone -> ""
             SortAsc -> " grid-col-sort-icon-asc"
             SortDesc -> " grid-col-sort-icon-desc"
      else "")


gridBody :: forall t m k v . (MonadWidget t m, Ord k)
  => Dynamic t (Columns k v)        -- ^ visible columns
  -> Dynamic t (Rows k v)           -- ^ window
  -> Dynamic t (Rows k v)           -- ^ selected rows
  -> Dynamic t (Map String String)  -- ^ x-rowgroup attrs
  -> (Columns k v -> (k, k) -> v -> Dynamic t Bool -> m (El t)) -- ^ row creating action
  -> m
     ( El t                                           -- ^ tbody element
     , Dynamic t (Map (k, k) (Event t ((k, k), v)))   -- ^ row selection events
     )
gridBody dcs window dselected dattrs mkRow = el' "tbody" $
  -- i am not sure it is legal to have a custom element directly under tbody
  -- if not then what consequences does it have?
  elDynAttr "x-rowgroup" dattrs $ do
    dsel <- widgetHold (return $ constDyn Map.empty) $ fmap (const $ do
        -- we want to sample the columns exactly once for all rows we render
        cs <- sample $ current dcs
        listWithKey window $ \k dv -> do
          v <- sample $ current dv
          r <- mkRow cs k v =<< mapDyn (isJust . Map.lookup k) dselected
          return $ (k, v) <$ domEvent Click r
      ) $ updated dcs
    return $ joinDyn dsel


-- | Grid view.
grid :: forall t m k v a . (MonadWidget t m, Ord k, Default k, Enum k, Num k)
  => String                                 -- ^ css class applied to <div> container
  -> String                                 -- ^ css class applied to <table>
  -> Int                                    -- ^ row height in px
  -> Int                                    -- ^ extra rows rendered on top and bottom
  -> NominalDiffTime                        -- ^ debounce delay, 0.01 seems reasonable
  -> Dynamic t (Columns k v)                -- ^ columns
  -> Dynamic t (Rows k v)                   -- ^ rows
  -> (Columns k v -> (k, k) -> v -> Dynamic t Bool -> m (El t)) -- ^ row creating action
  -> m
     ( Dynamic t (Columns k v)              -- ^ visible columns
     , Dynamic t (Rows k v)                 -- ^ filtred rows
     , Dynamic t (Rows k v)                 -- ^ selected rows
     )
grid containerClass tableClass rowHeight extra debounceDelay dcols drows mkRow = do
  pb <- getPostBuild
  rec (gridResizeEvent, (tbody, dcontrols, expE, expVisE, expSelE, colToggles, dsel)) <- resizeDetectorAttr ("class" =: containerClass) $ do
        (expE, expVisE, expSelE, colToggles) <- gridMenu dcols

        (tbody, dcontrols, dsel) <- elClass "table" tableClass $ do
          dcontrols <- gridHead dcs sortState
          (tbody, dsel) <- gridBody dcs window dselected rowgroupAttrs mkRow
          return (tbody, dcontrols, dsel)

        return (tbody, dcontrols, expE, expVisE, expSelE, colToggles, dsel)

      -- height and top scroll
      initHeightE <- performEvent $ mapElHeight tbody pb
      resizeE <- performEvent . mapElHeight tbody =<< debounceShield gridResizeEvent
      tbodyHeight <- holdDyn 0 $ fmap ceiling $ leftmost [resizeE, initHeightE]
      scrollTop <- holdDyn 0 =<< debounceShield (domEvent Scroll tbody)

      -- split controls into filters and sort events
      dfs <- return . joinDynThroughMap =<< mapDyn (Map.map fst) dcontrols
      sortState <- toSortState . switchPromptlyDyn =<< mapDyn (leftmost . Map.elems) =<< mapDyn (Map.map snd) dcontrols

      -- TODO:
      -- if the old set of filteres is completely contained within the new we can keep existing work and
      -- only search within current dxs
      --
      -- note we cannot avoid starting from scratch when we subtract something from any of the filters
      gridState <- combineDyn4 (,,,) dcols drows dfs sortState
      dxs <- gridManager $ updated gridState

      window <- combineDyn3 toWindow dxs scrollTop tbodyHeight
      rowgroupAttrs <- combineDyn toRowgroupAttrs scrollTop =<< mapDyn Map.size dxs

      dcs <- mapDyn (Map.filter (== True)) (joinDynThroughMap colToggles)
        >>= combineDyn (Map.intersectionWith (\c _ -> c)) dcols

      dselected <- mapDyn (leftmost . Map.elems) dsel
        >>= foldDyn foldSelectMultiple Map.empty . switchPromptlyDyn

  exportCsv dcols $ tag (current drows) expE
  exportCsv dcols $ tag (current dxs) expVisE
  exportCsv dcols $ tag (current dselected) expSelE

  return (dcs, dxs, dselected)

  where
    toStyleAttr m = "style" =: (Map.foldrWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)

    mapElHeight el = fmap (const $ liftIO $ getOffsetHeight $ _el_element el)

    -- if the delay is given to be 0 there is no point in calling debounce
    debounceShield = case debounceDelay of
                       0 -> return
                       _ -> debounce debounceDelay

    -- always start the window with odd row not to have the zebra "flip" when using css :nth-child
    toWindow :: Rows k v -> Int -> Int -> Rows k v
    toWindow xs scrollTop tbodyHeight =
      let d = scrollTop `div` rowHeight - extra
          x = fromEnum $ odd d
          skip = d - x
          wsize = tbodyHeight `div` rowHeight + 1 + x + 2*extra
      in Map.fromList . take wsize . drop skip . Map.toList $ xs

    -- the position of the rowgroup is given by two css properties:
    -- - top    - offset from the top
    -- - height - includes content height and offset from the bottom
    -- the main invariant being:
    --   rowCount * rowHeight = top + height
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

--
-- TODO: one of those should probably be supplied by the caller to allow choice of single vs multiple selection
--

-- single row selection
foldSelectSingle :: Ord k => ((k, k), v) -> Rows k v -> Rows k v
foldSelectSingle (k, v) sel =
  case Map.lookup k sel of
    Just _ -> Map.empty
    Nothing -> Map.singleton k v

-- multipe row selection
foldSelectMultiple :: Ord k => ((k, k), v) -> Rows k v -> Rows k v
foldSelectMultiple (k, v) sel =
  case Map.lookup k sel of
    Just _ -> Map.delete k sel
    Nothing -> Map.insert k v sel

toCsv :: Columns k v -> Rows k v -> String
toCsv cols rows = printCSV $ toFields <$> Map.toList rows
  where toFields (k, x) = fmap (\c -> colValue c k x) cs
        cs = Map.elems cols

exportCsv :: MonadWidget t m => Dynamic t (Columns k v) -> Event t (Rows k v) -> m ()
exportCsv dcols e = do
  doc <- askDocument
  performEvent_ $ fmap (liftIO . triggerDownload doc "text/csv" "export.csv" . uncurry toCsv) $ attachDyn dcols e
