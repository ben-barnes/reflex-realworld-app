{-# LANGUAGE OverloadedStrings #-}

module RealWorld.Components (
  conduitNav
) where

import Reflex.Dom

import qualified RealWorld.Routing as Route

conduitNav :: (MonadWidget t m) => Dynamic t Route -> m (Event t Route)
conduitNav route =
  let routeClass p =
        (\currentRoute -> if p currentRoute then active else inactive) <$> route
      active = "nav-link active"
      inactive = "nav-link"
  in  elClass "nav" "navbar navbar-light" $
        divClass "container" $ do
          anchorClass "/" "navbar-brand" $ text "conduit"
          elClass "ul" "nav navbar-nav pull-xs-right" $ do
            homeClicks <- elClass "li" "nav-item" $
              anchorDynClassClickable "/" (routeClass Route.isHome) $
                text "Home"
            editorClicks <- elClass "li" "nav-item" $
              anchorDynClassClickable "/editor" (routeClass Route.isEditor) $ do
                elClass "i" "ion-compose" blank
                text " New Post"
            settingsClicks <- elClass "li" "nav-item" $
              anchorDynClassClickable "/settings" (routeClass Route.isSettings) $ do
                elClass "i" "ion-gear-a" blank
                text " Settings"
            registerClicks <- elClass "li" "nav-item" $
              anchorDynClassClickable "/register" (routeClass Route.isRegister) $
                text "Sign up"
            return $
              leftmost [
                  fmap (const Route.Home) homeClicks
                , fmap (const $ Route.Editor Nothing) editorClicks
                , fmap (const Route.Settings) settingsClicks
                , fmap (const Route.Register) registerClicks
                ]
