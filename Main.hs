{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Aeson
import Data.Default
import Data.FileEmbed
import qualified Data.HashMap as HM
import Data.Maybe
import Data.Map as Map
import Data.Monoid (Sum (..), (<>))
import Data.Text (Text)
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics

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

  postBuildEvent <- getPostBuild

  -- fetch event occurs on page load and every time we click the refresh button
  let req = xhrRequest "GET" "500.json" def
      fetchEvent = appendEvents clickEvent postBuildEvent
  asyncReq <- performRequestAsync (tag (constant req) fetchEvent)

  let mresp = fmap decodeXhrResponse asyncReq
  xs <- holdDyn (Just []) mresp >>= mapDyn fromJust

  xs' <- forDyn (xs :: Dynamic Spider [Employee]) $ \xs ->
           Map.fromList $ zip [1..] xs

  grid "table" (constDyn columns) xs' (constDyn 25)
  
  return ()


columns :: Map Int (GridColumn Int Employee)
columns = Map.fromList $ zip [1..]
  [ mkCol "ID"          (\k _ -> show k)
  , mkCol "First name"  (\_ -> firstName)
  , mkCol "Last name"   (\_ -> lastName)
  , mkCol "Company"     (\_ -> company)
  , mkCol "Employed"    (\_ -> show . employed)
  ]
