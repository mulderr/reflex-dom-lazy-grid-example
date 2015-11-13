{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyGrid where

import Control.Monad (forM_, when, liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.Map as Map
import Data.Monoid ((<>))
import Data.Time.Clock
import qualified Data.Vector as V

import Reflex
import Reflex.Dom

import GHCJS.DOM.Element hiding (drop)


data GridColumn k v = GridColumn
  { colName :: String
  , colHeader :: String
  , colValue :: k -> v -> String              -- ^ column string value for display, can use row key and value
  , colCompare :: Maybe (v -> v -> Ordering)
  , colWidth :: Maybe Int
  , colVisible :: Bool
  }

instance Default (GridColumn k v) where
  def = GridColumn
    { colName = ""
    , colHeader = ""
    , colValue = (\_ _ -> "")
    , colCompare = Nothing
    , colWidth = Nothing
    , colVisible = True
    }

-- Lazy grid - based on virtualList.
-- 
-- Uses resizeDetector to keep track of height and renders only as many rows as needed.
grid :: (MonadWidget t m, Ord k, Show k)
  => String                                 -- ^ css class applied to <div> container
  -> String                                 -- ^ css class applied to <table>
  -> Int                                    -- ^ row height in px
  -> Int                                    -- ^ extra rows rendered on top and bottom
  -> Dynamic t (Map k (GridColumn k v))     -- ^ column spec
  -> Dynamic t (Map k v)                    -- ^ rows
  -> m ()
grid containerClass tableClass rowHeight extra dcols dxs = do
  rowCount <- mapDyn size dxs

  rec -- circular refs:
      -- window > pos > scrollTop > gridBody > window
      -- rowgroupAttrs > scrollTop > gridBody > rowgroupAttrs
      -- ...

      (gridResizeEvent, gridBody) <- resizeDetectorAttr ("class" =: containerClass) $
        elClass "table" tableClass $ do
          el "thead" $ el "tr" $ listWithKey dcols $ \k dc ->
            sample (current dc) >>= \c -> el "th" $ do
              el "div" $ text (colHeader c)
              filterInput <- textInput $ def & attributes .~ constDyn (mconcat ["class" =: "grid-filter"])
              return ()


          (tbody, _) <- el' "tbody" $
            elDynAttr "rowgroup" rowgroupAttrs $ do

              listWithKey window $ \k dv -> sample (current dv) >>= \v ->
                el "tr" $ listWithKey dcols $ \_ dc ->
                  sample (current dc) >>= \c -> el "td" $ text (colValue c k v)

              return ()

          return tbody

      scrollTop <- holdDyn 0 =<< debounce scrollDebounceDelay (domEvent Scroll gridBody)
      params <- combineDyn (,) scrollTop tbodyHeight
      window <- combineDyn toWindow params dxs

      rowgroupAttrs <- combineDyn toRowgroupAttrs scrollTop rowCount

      resizeE <- performEvent $ mapHeight gridBody gridResizeEvent
      initHeightE <- performEvent . mapHeight gridBody =<< getPostBuild
      tbodyHeight <- holdDyn 0 $ leftmost [resizeE, initHeightE]

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
          wsize = (ceiling tbodyHeight) `div` rowHeight + 1 + x + extra
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


mkCol :: String -> (k -> v -> String) -> GridColumn k v
mkCol name val = def 
  { colName = name
  , colHeader = name
  , colValue = val
  }


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
-- * replaced wrapDomEvent with domEvent
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
