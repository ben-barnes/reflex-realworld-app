{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Pages.Settings (
  settingsPage
) where

import Conduit.Common.Data (getBio, getEmail, getImage, getUsername)
import Conduit.Frontend.API (
    AuthProfile
  , authProfileBio
  , authProfileEmail
  , authProfileImage
  , authProfileUsername
  )
import Conduit.Frontend.Components.Form (
    formSubmitButton
  , formTextAreaLarge
  , formTextInputSmall
  , formTextInputLarge
  )
import Conduit.Frontend.Data.SessionEvent (SessionEvent)
import Reflex.Dom (
    Event
  , MonadWidget
  , divClass
  , el
  , elClass
  , never
  , text
  )

settingsPage :: (MonadWidget t m) => AuthProfile -> m (Event t SessionEvent)
settingsPage auth =
  divClass "settings-page" $
    divClass "container page" $
      divClass "row" $
        divClass "col-md-6 offset-md-3 col-xs-12" $ do
          elClass "h1" "text-xs-center" $ text "Your Settings"
          settingsForm auth

settingsForm :: (MonadWidget t m) => AuthProfile -> m (Event t SessionEvent)
settingsForm auth =
  el "form" $
    el "fieldset" $ do
      settingsInputs auth
      formSubmitButton "Update Settings"
      return never

settingsInputs :: (MonadWidget t m) => AuthProfile -> m ()
settingsInputs auth =
  let image = getImage <$> authProfileImage auth
      name  = Just . getUsername . authProfileUsername $ auth
      bio   = getBio <$> authProfileBio auth
      email = Just . getEmail . authProfileEmail $ auth
  in  do 
    formTextInputSmall image "text" "URL of profile picture"
    formTextInputLarge name "text" "Your Name"
    formTextAreaLarge bio "Short bio about you" 8
    formTextInputLarge email "text" "Email"
    formTextInputLarge Nothing "password" "Password"
    return ()
