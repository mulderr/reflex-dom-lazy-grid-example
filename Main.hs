{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
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
  deriving (Show)

instance FromJSON Employee where
  parseJSON (Object v) = Employee <$> v .: "firstName"
                                  <*> v .: "lastName"
                                  <*> v .: "company"
                                  <*> v .: "employed"


main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") gridExample

gridExample :: MonadWidget t m => m ()
gridExample = do
  clickEvent <- el "div" $ do
    button "Refresh"

  pb <- getPostBuild

  -- fetch event occurs on page load and every time we click the refresh button
  let req = xhrRequest "GET" "500.json" def
  asyncReq <- performRequestAsync $ tag (constant req) $ leftmost [clickEvent, pb]

  let mresp = fmap decodeXhrResponse asyncReq
  xs <- holdDyn (Just []) mresp >>= mapDyn fromJust

  xs' <- forDyn xs $ Map.fromList . zip (map (\x -> (x, x)) [1..])

  grid "my-grid" "table" 30 2 (constDyn columns) xs' $ \cs k dv -> do
    v <- sample $ current dv
    el "tr" $ forM (Map.elems cs) $ \c ->
      el "td" $ text ((colValue c) k v)


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
