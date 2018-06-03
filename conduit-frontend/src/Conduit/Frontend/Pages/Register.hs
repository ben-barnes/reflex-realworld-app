{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

-- TODO
-- [ ] Hook up register response, render errors and return authed profile

module Conduit.Frontend.Pages.Register (
  registerPage
) where

import Conduit.Common.Data (
    Email(Email)
  , Password(Password)
  , Username(Username)
  )
import Conduit.Frontend.API (
    AuthProfile
  , NewUser(NewUser)
  , RegisterError
  , RegisterFailure
  , RegisterRequest(RegisterRequest)
  , RegisterResponse (RegisterResponseSuccess, RegisterResponseFailure)
  , getRegisterError
  , register
  , registerErrorsEmail
  , registerErrorsPassword
  , registerErrorsUsername
  , registerFailureErrors
  )
import Conduit.Frontend.Components (aAttr)
import Conduit.Frontend.Routing (Route(Login), renderRoute)
import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (concat)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHCJS.DOM.Element (Element(Element))
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (input)
import GHCJS.DOM.HTMLInputElement (HTMLInputElement(HTMLInputElement), getValue)
import GHCJS.DOM.Types (uncheckedCastTo)
import Reflex.Dom (
    (=:)
  , Dynamic
  , Event
  , EventName(Click)
  , MonadWidget
  , _element_raw
  , blank
  , decodeXhrResponse
  , divClass
  , domEvent
  , el
  , elAttr'
  , elClass
  , elClass'
  , fmapMaybe
  , holdDyn
  , performRequestAsync
  , tagPromptlyDyn
  , text
  , widgetHold
  , wrapDomEvent
  )

import qualified Data.Map as Map

registerPage :: (MonadWidget t m) => m (Event t AuthProfile)
registerPage =
  divClass "auth-page" $
    divClass "container page" $
      divClass "row" $
        divClass "col-md-6 offset-md-3 col-xs-12" $ do
          elClass "h1" "text-xs-center" $ text "Sign up"
          elClass "p" "text-xs-center" $
            void $ aAttr ("href" =: renderRoute Login) $ text "Have an account?"
          rec _ <- widgetHold blank (errorList <$> failure)
              (success, failure) <- el "form" $ do
                nameValue <- formTextInput "text" "Your Name"
                emailValue <- formTextInput "text" "Email"
                passwordValue <- formTextInput "password" "Password"
                buttonClick <- signUpButton
                let name = Username <$> nameValue
                    email = Email <$> emailValue
                    password = Password <$> passwordValue
                    newUser = NewUser <$> name <*> email <*> password
                    registerRequest = RegisterRequest <$> newUser
                    registerXhr = register <$> registerRequest
                    sendRegisterRequest = tagPromptlyDyn registerXhr buttonClick
                res <- performRequestAsync sendRegisterRequest
                let getSuccess (RegisterResponseSuccess s) = Just s
                    getSuccess _ = Nothing
                    getFailure (RegisterResponseFailure f) = Just f
                    getFailure _ = Nothing
                    resSuccess = fmapMaybe (decodeXhrResponse >=> getSuccess) res
                    resFailure = fmapMaybe (decodeXhrResponse >=> getFailure) res
                return (resSuccess, resFailure)
          return success

errorList :: (MonadWidget t m) => RegisterFailure -> m ()
errorList f =
  let errors = registerFailureErrors f
      allErrors = concat . catMaybes $ [
          prefixErrors "Email" <$> registerErrorsEmail errors
        , prefixErrors "Password" <$> registerErrorsPassword errors
        , prefixErrors "Username" <$> registerErrorsUsername errors
        ]
      renderError e = el "li" $ text e
  in  elClass "ul" "error-messages" $
        traverse_ renderError allErrors

prefixErrors :: Text -> [RegisterError] -> [Text]
prefixErrors prefix es =
  let addPrefix e = prefix <> " " <> e
  in  addPrefix . getRegisterError <$> es

formTextInput :: (MonadWidget t m) => Text -> Text -> m (Dynamic t Text)
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

signUpButton :: (MonadWidget t m) => m (Event t ())
signUpButton =
  let buttonClasses = "btn btn-lg btn-primary pull-xs-right"
  in  do
    (e, _) <- elClass' "button" buttonClasses $ text "Sign up"
    return $ domEvent Click e
