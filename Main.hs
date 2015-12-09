{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Aeson
import Data.Char (toLower)
import Data.FileEmbed
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (forM)

import Reflex
import Reflex.Dom

import LazyGrid


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
  reloadE <- elClass "div" "description" $ do
    el "h2" $ text "Reflex-frp lazy grid demo"
    el "p" $ do
      text "Features:"
      el "ul" $ forM
        [ "semantic markup"
        , "sorting"
        , "filtering"
        , "column selection"
        , "csv export"
        , "scales to occupy all available space"
        , "styled after ui-grid"
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


  pb <- getPostBuild

  -- fetch event occurs on page load and every time we click the refresh button
  let toReq filename = xhrRequest "GET" filename def
  asyncReq <- performRequestAsync $ fmap toReq $ leftmost [reloadE, fmap (const "500.json") pb]

  let mresp = fmap decodeXhrResponse asyncReq
  xs <- holdDyn (Just []) mresp >>= mapDyn fromJust

  xs' <- forDyn xs $ Map.fromList . zip (map (\x -> (x, x)) [1..])

  dfs <- grid "my-grid" "table" 30 2 (constDyn columns) xs' $ \cs k dv -> do
    v <- sample $ current dv
    el "tr" $ forM (Map.toList cs) $ \(ck, c) ->
      -- Could switch on key instead:
      -- case ck of
      --   5 ->
      --   _ ->
      case colHeader c of
        "Employed" -> do let t = (colValue c) k v
                             attrs = ("class" =: if t == "0" then "red" else "")
                         elAttr "td" attrs $ text t
        _ -> el "td" $ text ((colValue c) k v)

  elClass "div" "description" $ do
    el "p" $ do
      text "Statistics:"
      el "ul" $ do
        el "li" $ do
          text "Row count: "
          dynText =<< mapDyn (show . Map.size) xs'
        el "li" $ do
          text "Filtered row count: "
          dynText =<< mapDyn (show . Map.size) dfs

    el "p" $ do
      text "Bugs:"
      el "ul" $ do
        el "li" $ text "if you load a different dataset the view will not automatically reload (scroll away and back to recreate the rows)"
        el "li" $ text "arrow key scroll is broken, it locks when the currently visible rows go out of the DOM"

    el "p" $ do
      text "Todo:"
      el "ul" $ do
        el "li" $ text "fixed width columns (currently only with css but that breaks when coupled with column selection)"
        el "li" $ text "single row selection"

  return ()


columns :: Map Int (Column Int Employee)
columns = Map.fromList $ zip [1..]
  [ def { colHeader = "No."
        , colValue = (\(k, _) _ -> show k) 
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
        }
  ]

matchIgnoreCase :: (Employee -> String) -> String -> Map (Int, Int) Employee -> Map (Int, Int) Employee
matchIgnoreCase f s = Map.filter $ isInfixOf (map toLower s) . (map toLower) . f
