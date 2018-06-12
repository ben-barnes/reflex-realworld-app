{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Components.Form (
  form
, formSubmitButton
, formTextAreaClass
, formTextAreaLarge
, formTextInputClass
, formTextInputLarge
, formTextInputSmall
) where

import Data.Text (Text)
import GHCJS.DOM.Element (Element(Element))
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (input)
import GHCJS.DOM.HTMLInputElement (HTMLInputElement(HTMLInputElement))
import GHCJS.DOM.HTMLTextAreaElement (HTMLTextAreaElement(HTMLTextAreaElement))
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
import qualified Data.Text as Text
import qualified GHCJS.DOM.HTMLInputElement as InputElement (getValue)
import qualified GHCJS.DOM.HTMLTextAreaElement as TextAreaElement (getValue)

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

formTextAreaClass
  :: (MonadWidget t m)
  => Text
  -> Int
  -> Text
  -> m (Dynamic t Text)
formTextAreaClass placeholder rows cls =
  let attr = Map.fromList [
          ("class", cls)
        , ("rows", rowsText)
        , ("placeholder", placeholder)
        ]
      rowsText = Text.pack . show $ rows
  in  elClass "fieldset" "form-group" $ do
        (e, _) <- elAttr' "textarea" attr $ blank
        let textAreaElement = uncheckedCastTo
              HTMLTextAreaElement
              (_element_raw e)
        onInput <- wrapDomEvent
                     textAreaElement
                     (`on` input)
                     (TextAreaElement.getValue textAreaElement)
        holdDyn "" onInput

formTextAreaLarge
  :: (MonadWidget t m)
  => Text
  -> Int
  -> m (Dynamic t Text)
formTextAreaLarge placeholder rows =
  formTextAreaClass placeholder rows "form-control form-control-lg"

formTextInputClass
  :: (MonadWidget t m)
  => Text
  -> Text
  -> Text
  -> m (Dynamic t Text)
formTextInputClass tpe placeholder cls = 
  let classAttr = ("class", cls)
      typeAttr  = ("type", tpe)
      placeholderAttr = ("placeholder", placeholder)
      attrs = Map.fromList [classAttr, typeAttr, placeholderAttr]
  in  elClass "fieldset" "form-group" $ do
        (e, _) <- elAttr' "input" attrs $ blank
        let inputElement = uncheckedCastTo HTMLInputElement (_element_raw e)
        onInput <- wrapDomEvent
                     inputElement
                     (`on` input)
                     (InputElement.getValue inputElement)
        holdDyn "" onInput

formTextInputLarge
  :: (MonadWidget t m)
  => Text
  -> Text
  -> m (Dynamic t Text)
formTextInputLarge tpe placeholder =
  formTextInputClass tpe placeholder "form-control form-control-lg"

formTextInputSmall
  :: (MonadWidget t m)
  => Text
  -> Text
  -> m (Dynamic t Text)
formTextInputSmall tpe placeholder =
  formTextInputClass tpe placeholder "form-control"

