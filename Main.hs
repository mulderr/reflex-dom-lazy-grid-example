{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, TemplateHaskell, DeriveGeneric #-}
module Main where

import           Control.Lens
import           Data.Aeson
import           Data.Char (toLower)
import           Data.FileEmbed
import           Data.List (isInfixOf)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Safe
import           Data.Foldable (forM_)

import           GHC.Generics

import           Reflex
import           Reflex.Dom

import           LazyGrid


data Employee = Employee
  { firstName :: String
  , lastName :: String
  , company :: String
  , employed :: Bool
  }
  deriving (Generic, Eq, Show)

instance FromJSON Employee


main :: IO ()
main = mainWidgetWithCss ($(embedFile "grid.css") <> $(embedFile "style.css")) gridExample

gridExample :: MonadWidget t m => m ()
gridExample = do
  pb <- getPostBuild
  rec metaData <- myGridView $ leftmost [reloadE, "500.json" <$ pb]
      reloadE <- myDescription metaData
  return ()

myDescription :: MonadWidget t m => Grid t Int Employee -> m (Event t String)
myDescription g = do
  elClass "div" "description" $ do
    el "h2" $ text "Reflex-dom lazy grid demo"
    el "p" $ do
      text "Features:"
      el "ul" $ forM_
        [ "allows semantic markup, although by default a non standard <x-rowgroup> tag is used for positioning of visible rows (for perfomance)"
        , "single column sorting"
        , "multiple column filtering"
        , "column selection"
        , "fixed width columns through custom column attrs"
        , "conditional formatting using the row creating action"
        , "csv export (using html5 createObjectURL, not supported on older browsers)"
        , "scales to occupy all available space"
        , "styled after ui-grid (but almost nothing is hardcoded so it should be straightforward to supply a different stylesheet)"
        ]
        $ \s -> el "li" $ text s
    e <- el "p" $ do
      text "Grab some data (also courtesy of ui-grid):"
      el "ul" $ do
        (e1, _) <- el "li" $ elAttr' "a" ("href" =: "#") $ text "50 rows"
        (e2, _) <- el "li" $ elAttr' "a" ("href" =: "#") $ text "500 rows"
        (e3, _) <- el "li" $ elAttr' "a" ("href" =: "#") $ text "10k rows"
        return $ leftmost
          [ "50.json" <$ domEvent Click e1
          , "500.json" <$ domEvent Click e2
          , "10000.json" <$ domEvent Click e3
          ]

    el "p" $ do
      text "Meta:"
      el "ul" $ do
        el "li" $ do
          text "Column count: "
          dynText =<< mapDyn (show . Map.size) (_grid_columns g)
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

    el "p" $ do
      text "Selected row:"
      el "ul" $ listWithKey (_grid_rowsSelected g) $ \k dv -> do
        v <- sample $ current dv
        el "li" $ text $ show v

    el "p" $ do
      text "More:"
      el "ul" $ do
        el "li" $ do
          text "apparently a scrollable tbody can be a challenge in and of itself; a bare bones example of how it's currently done can be found "
          elAttr "a" ("href" =: "https://jsfiddle.net/bakgx0yz/1/") $ text "here"
        el "li" $ text $ "zebra is done using css :nth-child without any attrs generated in code;"
                      <> " this is possible due to the privision that the window always starts with an odd row"

    el "p" $ do
      elAttr "a" ("href" =: "https://github.com/mulderr/reflex-dom-lazy-grid-example") $ text "Code on Github"

    return e

myGridView :: (MonadWidget t m)
  => Event t String   -- ^ Event with path to JSON data
  -> m (Grid t Int Employee)
myGridView reloadE = do
  let toReq filename = xhrRequest "GET" filename def
  asyncReq <- performRequestAsync $ fmap toReq reloadE

  xs <- holdDyn (Just []) (fmap decodeXhrResponse asyncReq)
    >>= mapDyn (Map.fromList . zip (map (\x -> (x, x)) [1..]) . fromJust)

  grid $ def & attributes .~ constDyn ("class" =: "my-grid")
             & gridConfig_columns .~ constDyn columns
             & gridConfig_rows .~ xs
             & gridConfig_selectionStrategy .~ selectSingle
             -- we want to show off conditional formatting, normally it's fine to stick with the default
             & gridConfig_rowAction .~ \cs k v dsel -> do
                attrs <- forDyn dsel $ \s -> if s then ("class" =: "grid-row-selected") else mempty
                (e, _) <- elDynAttr' "tr" attrs $ forM_ cs $ \c ->
                  let t = (_colValue c) k v
                      attrs = _colAttrs c <> case _colName c of
                                               "employed" -> if t == "0" then "class" =: "red" else mempty
                                               _ -> mempty
                  in elAttr "td" attrs $ text t
                return e

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
  -- example of a column computed from two fields
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
