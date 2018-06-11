{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Components.Footer (
  conduitFooter
) where

import Conduit.Frontend.Routes (Route(Home), printRoute)
import Conduit.Frontend.Widgets (aAttr)
import Reflex.Dom (
    (=:)
  , Event
  , MonadWidget
  , divClass
  , el
  , elAttr
  , elClass
  , text
  )

import qualified Data.Map as Map

conduitFooter :: (MonadWidget t m) => m (Event t Route)
conduitFooter =
  el "footer" $
    divClass "container" $ do
      logoClicked <- logo
      attribution
      return logoClicked

logo :: (MonadWidget t m) => m (Event t Route)
logo =
  let attrs = Map.fromList [("href", printRoute Home), ("class", "logo-font")]
  in  do
    clicked <- aAttr attrs $ text "conduit"
    return $ Home <$ clicked

attribution :: (MonadWidget t m) => m ()
attribution =
  elClass "span" "attribution" $ do
    text "An interactive learning project from "
    elAttr "a" ("href" =: "https://thinkster.io") $ text "Thinkster"
    text ". Code & design licensed under MIT."
