{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Aeson
import Data.FileEmbed
import qualified Data.HashMap as HM
import Data.Maybe (fromJust)
import Data.Map as Map

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
  [ mkCol "ID"          (\k _ -> show k)
  , mkCol "First name"  (\_ -> firstName)
  , mkCol "Last name"   (\_ -> lastName)
  , mkCol "Company"     (\_ -> company)
  , mkCol "Employed"    (\_ -> show . employed)
  ]
