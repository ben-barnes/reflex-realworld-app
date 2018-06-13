{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

-- TODO
-- [x] Hook up register response, render errors and return authed profile

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
  , RegisterRequest(RegisterRequest)
  , RegisterResponse (RegisterResponseSuccess, RegisterResponseFailure)
  , register
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
import Conduit.Frontend.Routes (Route(Login), printRoute)
import Conduit.Frontend.Widgets (aAttr)
import Reflex.Dom (
    (=:)
  , Dynamic
  , Event
  , MonadWidget
  , _xhrResponse_responseText
  , blank
  , decodeXhrResponse
  , divClass
  , elClass
  , fanEither
  , fmapMaybe
  , leftmost
  , performRequestAsync
  , text
  , traceEventWith
  , widgetHold
  )

registerPage :: (MonadWidget t m) => m (Event t SessionEvent)
registerPage =
  divClass "auth-page" $
    divClass "container page" $
      divClass "row" $
        divClass "col-md-6 offset-md-3 col-xs-12" $ do
          elClass "h1" "text-xs-center" $ text "Sign up"
          loginClicked <- loginLink
          authEvents <- registrationFormWithErrors
          return $ leftmost [
              SessionAuthEvent . Just <$> authEvents
            , SessionRouteEvent Login <$ loginClicked
            ]

loginLink :: (MonadWidget t m) => m (Event t Route)
loginLink =
  elClass "p" "text-xs-center" $ do
    clicked <- aAttr ("href" =: printRoute Login) $ text "Have an account?"
    return $ Login <$ clicked

registrationFormWithErrors :: (MonadWidget t m) => m (Event t AuthProfile)
registrationFormWithErrors = do
  rec _ <- widgetHold blank (errorList <$> failure)
      (failure, success) <- registrationForm
  return success

registrationForm :: (MonadWidget t m) => m (Event t Errors, Event t AuthProfile)
registrationForm = do
  req <- form registrationInputs $ formSubmitButton "Sign up"
  res <- performRequestAsync (register <$> req)
  let resToEither (RegisterResponseSuccess p) = Right p
      resToEither (RegisterResponseFailure f) = Left f
      validResponses = fmapMaybe decodeXhrResponse (traceEventWith (show . _xhrResponse_responseText ) res)
  return $ fanEither (resToEither <$> (traceEventWith (const "Valid response") validResponses))

registrationInputs :: (MonadWidget t m) => m (Dynamic t RegisterRequest)
registrationInputs = do
  name     <- formTextInputLarge Nothing "text" "Your Name"
  email    <- formTextInputLarge Nothing "text" "Email"
  password <- formTextInputLarge Nothing "password" "Password"
  let newUser = NewUser
        <$> (Username <$> name)
        <*> (Email <$> email)
        <*> (Password <$> password)
  return $ RegisterRequest <$> newUser
