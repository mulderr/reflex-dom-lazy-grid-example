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

import GHCJS.DOM.Element (getScrollTop)


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
  -> Dynamic t (Map k (GridColumn k v))     -- ^ column spec
  -> Dynamic t (Map k v)                    -- ^ rows
  -> Dynamic t Int                          -- ^ size
  -> m ()
grid klass dcols dxs dsize = do
  cols <- sample $ current dcols
  siz <- sample $ current dsize

  dxsSize <- mapDyn size dxs

  rec -- "thread blocked indefinitely in an MVar operation" in console and high cpu usage
      -- page does not load
      --scrollPos <- holdDyn 0 =<< debounce 0.01 (domEvent Scroll gridBody)

      -- sometimes goes out of sync, esp. when fast scrolling - only page reload restores normal operation
      -- rowgroup height somehow ends up negative
      scrollPos <- holdDyn 0 (domEvent Scroll gridBody)

      pos <- mapDyn toRow scrollPos
      rowgroupAttrs <- combineDyn toRowgroupStyle scrollPos dxsSize
      window <- combineDyn (\rs p -> Map.fromList $ take siz $ drop p $ Map.toList rs) dxs pos
      
      gridBody <- elAttr "div" ("class" =: "lazy-grid") $
        elAttr "table" ("class" =: klass) $ do
          el "thead" $ el "tr" $
            forM_ cols $ \c -> el "th" $ text (colHeader c)

          (tbody, _) <- el' "tbody" $
            elDynAttr "rowgroup" rowgroupAttrs $ do

              listWithKey window $ \k dv ->
                sample (current dv) >>= \v ->
                  el "tr" $ forM_ cols $ \col -> el "td" $ text (colValue col k v)

              return ()

          return tbody

  text "scrollTop: "; display scrollPos
  text "; rowIndex: "; display pos
  return ()

  where
    rowHeight = 30 -- in px
    toStyleAttr m = "style" =: (Map.foldrWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)
    toRow scrollTopPx = let (row, _) = scrollTopPx `divMod` rowHeight
                        in row
    toRowgroupStyle scrollPosition rowCount = 
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
{-
debounce :: MonadWidget t m => NominalDiffTime -> Event t a -> m (Event t a)
debounce dt e = do
  n :: Dynamic t Integer <- count e
  let tagged = attachDynWith (,) n e
  delayed <- delay dt tagged
  return $ attachWithMaybe (\n' (t, v) -> if n' == t then Just v else Nothing) (current n) delayed
-}
