{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Aeson
import Data.Char (toLower)
import Data.FileEmbed
import qualified Data.HashMap as HM
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom

import LazyGrid


data Employee = Employee
  { firstName :: String
  , lastName :: String
  , company :: String
  , employed :: Bool
  }

instance FromJSON Employee where
  parseJSON (Object v) = Employee <$> v .: "firstName"
                                  <*> v .: "lastName"
                                  <*> v .: "company"
                                  <*> v .: "employed"


main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ do
  clickEvent <- el "div" $ do
    button "Refresh"

  pb <- getPostBuild

  -- fetch event occurs on page load and every time we click the refresh button
  let req = xhrRequest "GET" "500.json" def
      fetchEvent = appendEvents clickEvent pb
  asyncReq <- performRequestAsync (tag (constant req) fetchEvent)

  let mresp = fmap decodeXhrResponse asyncReq
  xs <- holdDyn (Just []) mresp >>= mapDyn fromJust

  xs' <- forDyn (xs :: Dynamic Spider [Employee]) $ \xs ->
           Map.fromList $ zip [1..] xs

  grid "my-grid" "table" 30 10 (constDyn columns) xs'


columns :: Map Int (GridColumn Int Employee)
columns = Map.fromList $ zip [1..]
  [ def { colHeader = "No."
        , colValue = (\k _ -> show k) 
        }
  , def { colHeader = "First name"
        , colValue = const firstName
        , colFilter = Just $ matchIgnoreCase firstName
        }
  , def { colHeader = "Last name"
        , colValue = const lastName
        , colFilter = Just $ matchIgnoreCase lastName
        }
  , def { colHeader = "Company"
        , colValue = const company
        , colFilter = Just $ matchIgnoreCase company
        }
  , def { colHeader = "Employed"
        , colValue = (\_ -> show . fromEnum . employed)
        , colFilter = Just $ (\s -> Map.filter $ (==) s . show . fromEnum . employed)
        }
  ]

matchIgnoreCase :: (Employee -> String) -> String -> Map Int Employee -> Map Int Employee
matchIgnoreCase f s = Map.filter $ isInfixOf (map toLower s) . (map toLower) . f
