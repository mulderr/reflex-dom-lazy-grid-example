{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module Main where

import           Data.Aeson
import           Data.Char (toLower)
import           Data.FileEmbed
import           Data.List (isInfixOf)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Traversable (forM)

import           Reflex
import           Reflex.Dom

import           LazyGrid


data Employee = Employee
  { firstName :: String
  , lastName :: String
  , company :: String
  , employed :: Bool
  }
  deriving (Eq, Show)

instance FromJSON Employee where
  parseJSON (Object v) = Employee <$> v .: "firstName"
                                  <*> v .: "lastName"
                                  <*> v .: "company"
                                  <*> v .: "employed"

main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") gridExample

gridExample :: MonadWidget t m => m ()
gridExample = do
  reloadDataE <- elClass "div" "description" $ do
    el "h2" $ text "Reflex-frp lazy grid demo"
    el "p" $ do
      text "Features:"
      el "ul" $ forM
        [ "semantic markup (although technically the row creating action is supplied by the user so there is no hard guarantee)"
        , "sorting"
        , "filtering"
        , "column selection"
        , "fixed width columns through custom column attrs"
        , "conditional formatting using the row creating action"
        , "csv export (using html5 createObjectURL, not supported on older browsers)"
        , "scales to occupy all available space"
        , "styled after ui-grid (but almost nothing is hardcoded so you are free to just supply a different stylesheet)"
        ]
        $ \s -> el "li" $ text s
    e <- el "p" $ do
      text "Grab some data (also courtesy of ui-grid):"
      el "ul" $ do
        (e1, _) <- el "li" $ elAttr' "a" ("href" =: "#") $ text "500 rows"
        (e2, _) <- el "li" $ elAttr' "a" ("href" =: "#") $ text "10k rows"
        return $ leftmost
          [ fmap (\_ -> "500.json") $ domEvent Click e1
          , fmap (\_ -> "10000.json") $ domEvent Click e2
          ]
    return e

  (xs, xsFiltered, xsSelected) <- myGridView "500.json" reloadDataE

  elClass "div" "description" $ do
    el "p" $ do
      text "Meta:"
      el "ul" $ do
        el "li" $ do
          text "Row count: "
          dynText =<< mapDyn (show . Map.size) xs
        el "li" $ do
          text "Filtered row count: "
          dynText =<< mapDyn (show . Map.size) xsFiltered
        el "li" $ do
          text "Selected row count: "
          dynText =<< mapDyn (show . Map.size) xsSelected

    el "p" $ do
      text "Selected rows:"
      el "ul" $ listWithKey xsSelected $ \k dv -> do
        v <- sample $ current dv
        el "li" $ text $ show v

    el "p" $ do
      text "Bugs:"
      el "ul" $ do
        el "li" $ text "if you load a different dataset the view will not automatically reload (scroll away and back to recreate the rows)"
        el "li" $ text "arrow key scroll is broken, it locks when the currently visible rows go out of the DOM"

    el "p" $ do
      elAttr "a" ("href" =: "https://github.com/mulderr/reflex-dom-lazy-grid-example") $ text "Code on Github"

  return ()


myGridView :: (MonadWidget t m)
  => String           -- ^ JSON file to display by default
  -> Event t String   -- ^ Event with path to JSON data
  -> m (Dynamic t (Rows Int Employee), Dynamic t (Rows Int Employee), Dynamic t (Rows Int Employee))
myGridView defFile reloadE = do
  pb <- getPostBuild

  let toReq filename = xhrRequest "GET" filename def
  asyncReq <- performRequestAsync $ fmap toReq $ leftmost [reloadE, fmap (const defFile) pb]

  xs <- holdDyn (Just []) (fmap decodeXhrResponse asyncReq)
    >>= mapDyn (Map.fromList . zip (map (\x -> (x, x)) [1..]) . fromJust)

  (xsFiltered, xsSelected) <- grid "my-grid" "table" 30 2 (constDyn columns) xs $ \cs k v dsel -> do
    attrs <- forDyn dsel $ \s -> if s then ("class" =: "grid-row-selected") else Map.empty
    (e, _) <- elDynAttr' "tr" attrs $ forM (Map.toList cs) $ \(ck, c) -> do
      case ck of
        5 -> do let t = (colValue c) k v
                    attrs = (colAttrs c) <> ("class" =: if t == "0" then "red" else "")
                elAttr "td" attrs $ text t
        _ -> elAttr "td" (colAttrs c) $ text ((colValue c) k v)
    return e

  return (xs, xsFiltered, xsSelected)


columns :: Map Int (Column Int Employee)
columns = Map.fromList $ zip [1..]
  [ def { colHeader = "No."
        , colValue = (\(k, _) _ -> show k)
        , colAttrs = ("style" =: "width: 50px;")
        }
  , def { colHeader = "First name"
        , colValue = const firstName
        , colFilter = Just $ matchIgnoreCase firstName
        , colCompare = Just $ (\a b -> firstName a `compare` firstName b)
        }
  , def { colHeader = "Last name"
        , colValue = const lastName
        , colFilter = Just $ matchIgnoreCase lastName
        , colCompare = Just $ (\a b -> lastName a `compare` lastName b)
        }
  , def { colHeader = "Company"
        , colValue = const company
        , colFilter = Just $ matchIgnoreCase company
        , colCompare = Just $ (\a b -> company a `compare` company b)
        }
  , def { colHeader = "Employed"
        , colValue = (\_ -> show . fromEnum . employed)
        , colFilter = Just $ (\s -> Map.filter $ (==) s . show . fromEnum . employed)
        , colCompare = Just $ (\a b -> employed a `compare` employed b)
        , colAttrs = ("style" =: "width: 80px;")
        }
  ]

matchIgnoreCase :: (Employee -> String) -> String -> Map (Int, Int) Employee -> Map (Int, Int) Employee
matchIgnoreCase f s = Map.filter $ isInfixOf (map toLower s) . (map toLower) . f
