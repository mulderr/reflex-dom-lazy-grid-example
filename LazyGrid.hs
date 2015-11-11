{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyGrid where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.Map as Map
import Data.Monoid ((<>))
import Data.Time.Clock
import qualified Data.Vector as V

import Reflex
import Reflex.Dom

import GHCJS.DOM.Element (getClientHeight, getScrollTop)


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

grid :: (MonadWidget t m, Ord k, Show k)
  => String                                 -- ^ css class applied to <table>
  -> Int                                    -- ^ row height in px
  -> Int                                    -- ^ grid height in px
  -> Dynamic t (Map k (GridColumn k v))     -- ^ column spec
  -> Dynamic t (Map k v)                    -- ^ rows
  -> m ()
grid klass rowHeight gridHeight dcols dxs = do
  dxsSize <- mapDyn size dxs

  rec -- TODO: make the grid 100% height
      --
      -- circular refs:
      -- window > pos > scrollTop > gridBody > window
      -- rowgroupAttrs > scrollTop > gridBody > rowgroupAttrs

      gridBody <- elAttr "div" gridAttrs $
        elClass "table" klass $ do
          el "thead" $ el "tr" $ listWithKey dcols $ \k dc ->
            sample (current dc) >>= \c -> el "th" $ text (colHeader c)

          (tbody, _) <- el' "tbody" $
            elDynAttr "rowgroup" rowgroupAttrs $ do

              listWithKey window $ \k dv -> sample (current dv) >>= \v ->
                el "tr" $ listWithKey dcols $ \_ dc ->
                  sample (current dc) >>= \c -> el "td" $ text (colValue c k v)

              return ()

          return tbody

      scrollTop <- holdDyn 0 =<< debounce debounceDelay (domEvent Scroll gridBody)
      pos <- mapDyn (`div` rowHeight) scrollTop
      window <- combineDyn toWindow dxs pos

      rowgroupAttrs <- combineDyn toRowgroupAttrs scrollTop dxsSize

      --bodyHeight <- getClientHeight $ _el_element gridBody

  text "scrollTop: "; display scrollTop
  text "; rowIndex: "; display pos
  return ()

  where
    debounceDelay = 0.04 -- 40ms caps it at around 25Hz
    windowSize = gridHeight `div` rowHeight + 1
    toWindow rs p = Map.fromList $ take windowSize $ drop p $ Map.toList rs
    toStyleAttr m = "style" =: (Map.foldrWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)
    gridAttrs = "class" =: "lazy-grid" <> toStyleAttr ("height" =: (show gridHeight <> "px"))
    toRowgroupAttrs scrollPosition rowCount = 
      let height = rowHeight * rowCount - scrollPosition
      in toStyleAttr $ "position" =: "relative"
                    <> "overflow" =: "hidden"
                    <> "height"   =: (show height <> "px")
                    <> "top"      =: (show scrollPosition <> "px")


mkCol :: String -> (k -> v -> String) -> GridColumn k v
mkCol name val = def 
  { colName = name
  , colHeader = name
  , colValue = val
  }

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
