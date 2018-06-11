{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Components.Head (
  conduitHead
) where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom ((=:), MonadWidget, blank, el, elAttr, text)

import qualified Data.Map as Map
import qualified Data.Text as Text

conduitHead :: (MonadWidget t m) => m ()
conduitHead =
  let
  stylesheetAttrs url = Map.fromList [
      ("href", url)
    , ("rel", "stylesheet")
    , ("type", "text/css")
    ]
  in do
    elAttr "meta" ("charset" =: "utf-8") blank
    elAttr "link" (stylesheetAttrs iconsStylesheet) blank
    elAttr "link" (stylesheetAttrs fontsStylesheet) blank
    elAttr "link" (stylesheetAttrs conduitStylesheet) blank
    el "title" $ text "Conduit"

conduitStylesheet :: Text
conduitStylesheet =
  "https://demo.productionready.io/main.css"

iconsStylesheet :: Text
iconsStylesheet =
  "https://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"

fontsStylesheet :: Text
fontsStylesheet =
  let baseUrl = "https://fonts.googleapis.com/css?family="
      fonts   = Text.intercalate "|" [
          titilliumWeb
        , sourceSerifPro
        , merriweather
        , sourceSansPro
        ]
  in  baseUrl <> fonts

merriweather :: Text
merriweather =
  "Merriweather+Sans:400,700"

sourceSansPro :: Text
sourceSansPro =
  "Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic"

sourceSerifPro :: Text
sourceSerifPro =
  "Source+Serif+Pro:400,700"

titilliumWeb :: Text
titilliumWeb =
  "Titillium+Web:700"
