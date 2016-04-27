{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, TemplateHaskell, DeriveGeneric #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Bool (bool)
import           Data.Char (toLower)
import           Data.List (isInfixOf)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.Generics
import           Safe (headDef)
import           Reflex
import           Reflex.Dom

import           Reflex.Dom.LazyGrid


data Employee
  = Employee { firstName :: String
             , lastName :: String
             , company :: String
             , employed :: Bool
             } deriving (Generic, Eq, Show)

instance FromJSON Employee


main :: IO ()
main = mainWidgetWithHead headWidget gridExample

headWidget :: MonadWidget t m => m ()
headWidget = do
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no") blank
  elAttr "meta" ("http-equiv" =: "x-ua-compatible" <> "content" =: "ie=edge") blank
  el "title" $ text "Lazy grid example"
  linkCss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  linkCss "grid.css"
  linkCss "style.css"
  where
    linkCss href = elAttr "link" ("href" =: href <> "rel" =: "stylesheet" <> "type" =: "text/css") blank

gridExample :: MonadWidget t m => m ()
gridExample = do
  rec g <- gridView ds
      ds <- description "500" ["50", "500", "10000"] g
  return ()

-- | Buttons with optional initial value.
buttonsMaybe :: MonadWidget t m => Maybe String -> [String] -> m (Dynamic t (Maybe String))
buttonsMaybe initItem items = divClass "btn-group" $ do
  rec evs <- forM items $ \i -> do
        attrs <- forDyn sel $ \ms -> "class" =: ("btn btn-default" <> maybe "" (bool "" " active" . (==i)) ms)
        (e, _) <- elDynAttr' "button" attrs $ text i
        return $ Just i <$ domEvent Click e
      sel <- holdDyn initItem $ leftmost evs
  return $ nubDyn sel

-- | Buttons where one is always selected.
buttons:: MonadWidget t m => String -> [String] -> m (Dynamic t String)
buttons initItem items = do
  ms <- buttonsMaybe (Just initItem) items
  mapDyn (fromMaybe initItem) ms

description :: MonadWidget t m => String -> [String] -> Grid t Int Employee -> m (Dynamic t String)
description initFile files g = elClass "div" "description" $ do
  ev <- el "div" $ do
    text "Dataset size: "
    sel <- buttons initFile files
    mapDyn (<> ".json") sel
  el "div" $ do
    text "Meta:"
    el "ul" $ do
      el "li" $ do
        text "Column count: "
        text . show . Map.size . _grid_columns $ g
      el "li" $ do
        text "Visible column count: "
        dynText =<< mapDyn (show . Map.size) (_grid_columnsVisible g)
      el "li" $ do
        text "Row count: "
        dynText =<< mapDyn (show . Map.size) (_grid_rows g)
      el "li" $ do
        text "Filtered row count: "
        dynText =<< mapDyn (show . Map.size) (_grid_rowsFiltered g)
      el "li" $ do
        text "Selected row count: "
        dynText =<< mapDyn (show . Map.size) (_grid_rowsSelected g)
  el "div" $ do
    text "Selected row:"
    el "pre" $ listWithKey (_grid_rowsSelected g) $ \_ dv -> display dv
  el "div" $ do
    text "Code on Github: "
    elAttr "a" ("href" =: "https://github.com/mulderr/reflex-dom-lazy-grid-example") $ text "example"
    text " "
    elAttr "a" ("href" =: "https://github.com/mulderr/reflex-dom-lazy-grid") $ text "component"
  return ev

gridView :: (MonadWidget t m)
  => Dynamic t String
  -> m (Grid t Int Employee)
gridView dataset = do
  pb <- getPostBuild
  let req fname = xhrRequest "GET" fname def
  asyncReq <- performRequestAsync $ req <$> leftmost [updated dataset, tagDyn dataset pb]
  xs <- holdDyn (Just []) (decodeXhrResponse <$> asyncReq)
    >>= mapDyn (Map.fromList . zip (map (\x -> (x, x)) [1..]) . fromMaybe [])
  grid $ def & attributes .~ constDyn ("class" =: "my-grid")
             & gridConfig_columns .~ columns
             & gridConfig_rows .~ xs
             & gridConfig_selectionStrategy .~ selectSingle

columns :: Map Int (Column Int Employee)
columns = Map.fromList $ zip [1..]
  [ def { _colName     = "no"
        , _colHeader   = "No."
        , _colValue    = (\(k, _) _ -> show k)
        , _colAttrs    = ("style" =: "width: 50px;")
        }
  , def { _colName     = "fname"
        , _colHeader   = "First name"
        , _colValue    = const firstName
        , _colFilter   = Just $ matchIgnoreCase firstName
        , _colCompare  = Just (\a b -> firstName a `compare` firstName b)
        }
  , def { _colName     = "lname"
        , _colHeader   = "Last name"
        , _colValue    = const lastName
        , _colFilter   = Just $ matchIgnoreCase lastName
        , _colCompare  = Just (\a b -> lastName a `compare` lastName b)
        }
  , def { _colName     = "initials"
        , _colHeader   = "Initials"
        , _colValue    = const initials
        , _colFilter   = Just $ matchIgnoreCase initials
        , _colCompare  = Just (\a b -> initials a `compare` initials b)
        , _colVisible  = False
        , _colAttrs    = ("style" =: "width: 80px;")
        }
  , def { _colName     = "company"
        , _colHeader   = "Company"
        , _colValue    = const company
        , _colFilter   = Just $ matchIgnoreCase company
        , _colCompare  = Just (\a b -> company a `compare` company b)
        }
  , def { _colName     = "employed"
        , _colHeader   = "Employed"
        , _colValue    = (\_ -> show . fromEnum . employed)
        , _colFilter   = Just (\s -> Map.filter $ (==) s . show . fromEnum . employed)
        , _colCompare  = Just (\a b -> employed a `compare` employed b)
        , _colAttrs    = ("style" =: "width: 80px;")
        }
  ]

initials :: Employee -> String
initials e = headDef ' ' (firstName e) : '.' : headDef ' ' (lastName e) : '.' : ""

matchIgnoreCase :: (Employee -> String) -> String -> Rows Int Employee -> Rows Int Employee
matchIgnoreCase f s = Map.filter $ isInfixOf (map toLower s) . (map toLower) . f
