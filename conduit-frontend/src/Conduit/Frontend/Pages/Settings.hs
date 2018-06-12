{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Pages.Settings (
  settingsPage
) where

import Conduit.Frontend.API (AuthProfile)
import Conduit.Frontend.Components.Form (
    formTextAreaLarge
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
      formTextInputSmall "text" "URL of profile picture"
      formTextInputLarge "text" "Your name"
      formTextAreaLarge "Short bio about you" 8
      return never


-- <div class="settings-page">
--   <div class="container page">
--     <div class="row">
-- 
--       <div class="col-md-6 offset-md-3 col-xs-12">
--         <h1 class="text-xs-center">Your Settings</h1>
-- 
--         <form>
--           <fieldset>
--               <fieldset class="form-group">
--                 <input class="form-control" type="text" placeholder="URL of profile picture">
--               </fieldset>
--               <fieldset class="form-group">
--                 <input class="form-control form-control-lg" type="text" placeholder="Your Name">
--               </fieldset>
--               <fieldset class="form-group">
--                 <textarea class="form-control form-control-lg" rows="8" placeholder="Short bio about you"></textarea>
--               </fieldset>
--               <fieldset class="form-group">
--                 <input class="form-control form-control-lg" type="text" placeholder="Email">
--               </fieldset>
--               <fieldset class="form-group">
--                 <input class="form-control form-control-lg" type="password" placeholder="Password">
--               </fieldset>
--               <button class="btn btn-lg btn-primary pull-xs-right">
--                 Update Settings
--               </button>
--           </fieldset>
--         </form>
--       </div>
-- 
--     </div>
--   </div>
-- </div>
