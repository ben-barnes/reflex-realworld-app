{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Components.Form (
  form
, formSubmitButton
, formTextInput
) where

import Data.Text (Text)
import GHCJS.DOM.Element (Element(Element))
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (input)
import GHCJS.DOM.HTMLInputElement (HTMLInputElement(HTMLInputElement), getValue)
import GHCJS.DOM.Types (uncheckedCastTo)
import Reflex.Dom (
    Dynamic
  , Event
  , EventName(Click)
  , MonadWidget
  , _element_raw
  , blank
  , domEvent
  , el
  , elAttr'
  , elClass
  , elClass'
  , holdDyn
  , tagPromptlyDyn
  , text
  , wrapDomEvent
  )

import qualified Data.Map as Map

form :: (MonadWidget t m) => m (Dynamic t a) -> m (Event t ()) -> m (Event t a)
form inputs submit = el "form" $ do
  inputValue    <- inputs
  submitClicked <- submit
  return $ tagPromptlyDyn inputValue submitClicked

formSubmitButton :: (MonadWidget t m) => Text -> m (Event t ())
formSubmitButton label =
  let buttonClasses = "btn btn-lg btn-primary pull-xs-right"
  in  do
    (e, _) <- elClass' "button" buttonClasses $ text label
    return $ domEvent Click e

formTextInput
  :: (MonadWidget t m)
  => Text
  -> Text
  -> m (Dynamic t Text)
formTextInput tpe placeholder = 
  let classAttr = ("class", "form-control form-control-lg")
      typeAttr = ("type", tpe)
      placeholderAttr = ("placeholder", placeholder)
      attrs = Map.fromList [classAttr, typeAttr, placeholderAttr]
  in  do
    elClass "fieldset" "form-group" $ do
      (e, _) <- elAttr' "input" attrs $ blank
      let inputElement = uncheckedCastTo HTMLInputElement (_element_raw e)
      onInput <- wrapDomEvent inputElement (`on` input) (getValue inputElement)
      holdDyn "" onInput
