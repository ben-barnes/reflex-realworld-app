{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Conduit.Frontend.Pages.Login (
  loginPage
) where

import Conduit.Common.Data (Email(Email), Password(Password))
import Conduit.Frontend.API (
    AuthProfile
  , Credentials(Credentials)
  , LoginRequest(LoginRequest)
  , LoginResponse(LoginResponseSuccess, LoginResponseFailure)
  , login
  )
import Conduit.Frontend.API.Errors (Errors)
import Conduit.Frontend.Components.ErrorList (errorList)
import Conduit.Frontend.Components.Form (
    form
  , formSubmitButton
  , formTextInputLarge
  )
import Conduit.Frontend.Data.SessionEvent (
    SessionEvent(SessionAuthEvent, SessionRouteEvent)
  )
import Conduit.Frontend.Routes (Route(Register), printRoute)
import Conduit.Frontend.Widgets (aAttr)
import Reflex.Dom (
    (=:)
  , Dynamic
  , Event
  , MonadWidget
  , blank
  , decodeXhrResponse
  , divClass
  , elClass
  , fanEither
  , fmapMaybe
  , leftmost
  , performRequestAsync
  , text
  , widgetHold
  )

loginPage :: (MonadWidget t m) => m (Event t SessionEvent)
loginPage =
  divClass "auth-page" $
    divClass "container page" $
      divClass "row" $
        divClass "col-md-6 offset-md-3 col-xs-12" $ do
          elClass "h1" "text-xs-center" $ text "Login"
          registerClicked <- registerLink
          authEvents <- loginFormWithErrors
          return $ leftmost [
              SessionAuthEvent . Just <$> authEvents
            , SessionRouteEvent Register <$ registerClicked
            ]

registerLink :: (MonadWidget t m) => m (Event t Route)
registerLink =
  elClass "p" "text-xs-center" $ do
    clicked <- aAttr ("href" =: printRoute Register) $ text "Need an account?"
    return $ Register <$ clicked

loginFormWithErrors :: (MonadWidget t m) => m (Event t AuthProfile)
loginFormWithErrors = do
  rec _ <- widgetHold blank (errorList <$> failure)
      (failure, success) <- loginForm
  return success

loginForm :: (MonadWidget t m) => m (Event t Errors, Event t AuthProfile)
loginForm = do
  req <- form loginInputs $ formSubmitButton "Sign in"
  res <- performRequestAsync (login <$> req)
  let resToEither (LoginResponseSuccess p) = Right p
      resToEither (LoginResponseFailure f) = Left f
      validResponses = fmapMaybe decodeXhrResponse res
  return $ fanEither (resToEither <$> validResponses)

loginInputs :: (MonadWidget t m) => m (Dynamic t LoginRequest)
loginInputs = do
  email    <- formTextInputLarge "text" "Email"
  password <- formTextInputLarge "password" "Password"
  let credentials = Credentials
        <$> (Email <$> email)
        <*> (Password <$> password)
  return $ LoginRequest <$> credentials

