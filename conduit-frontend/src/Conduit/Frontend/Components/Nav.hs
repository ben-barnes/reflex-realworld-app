{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Components.Nav (
  conduitNav
) where

import Conduit.Common.Data (getUsername)
import Conduit.Frontend.API (AuthProfile, authProfileUsername)
import Conduit.Frontend.Routes (
    Route(Editor, Home, Login, Profile, Register, Settings)
  , isEditor
  , isHome
  , isLogin
  , isProfile
  , isRegister
  , isSettings
  , printRoute
  )
import Conduit.Frontend.Widgets (aAttr, aDynAttr)
import Reflex.Dom (
    Dynamic
  , Event
  , MonadWidget
  , blank
  , divClass
  , dyn
  , elClass
  , ffor
  , leftmost
  , never
  , switchHoldPromptly
  , text
  )

import qualified Data.Map as Map

conduitNav
  :: (MonadWidget t m)
  => Dynamic t (Maybe AuthProfile)
  -> Dynamic t Route
  -> m (Event t Route)
conduitNav auth route =
  elClass "nav" "navbar navbar-light" $
    divClass "container" $ do
      bannerClicked <- banner
      navbarClicked <- navbar auth route
      return $ leftmost [bannerClicked, navbarClicked]

banner :: (MonadWidget t m) => m (Event t Route)
banner =
  let attrs = Map.fromList [("href", printRoute Home), ("class", "navbar-brand")]
  in  do
    clicked <- aAttr attrs $ text "conduit"
    return $ Home <$ clicked

navbar
  :: (MonadWidget t m)
  => Dynamic t (Maybe AuthProfile)
  -> Dynamic t Route
  -> m (Event t Route)
navbar auth route =
  let chooseNav = ffor auth $ \a -> case a of
        Just a' -> authedNavItems a' route
        Nothing -> unauthedNavItems route
  in  elClass "ul" "nav navbar-nav pull-xs-right" $ do
        es <- dyn chooseNav
        switchHoldPromptly never es

authedNavItems
  :: (MonadWidget t m)
  => AuthProfile
  -> Dynamic t Route
  -> m (Event t Route)
authedNavItems auth route =
  let username = authProfileUsername auth
  in  do
    homeRoutes <- navItem route Home isHome $
      text "Home"
    editorRoutes <- navItem route (Editor Nothing) isEditor $ do
      elClass "i" "ion-compose" blank
      text " New Post"
    settingsRoutes <- navItem route Settings isSettings $ do
      elClass "i" "ion-gear-a" blank
      text " Settings"
    profileRoutes <- navItem route (Profile username) isProfile $
      text (getUsername username)
    return $ leftmost [homeRoutes, editorRoutes, settingsRoutes, profileRoutes]

unauthedNavItems :: (MonadWidget t m) => Dynamic t Route -> m (Event t Route)
unauthedNavItems route = do
  homeRoutes <- navItem route Home isHome $
    text "Home"
  loginRoutes <- navItem route Login isLogin $
    text "Sign in"
  registerRoutes <- navItem route Register isRegister $
    text "Sign up"
  return $ leftmost [homeRoutes, loginRoutes, registerRoutes]

navItem
  :: (MonadWidget t m)
  => Dynamic t Route
  -> Route
  -> (Route -> Bool)
  -> m ()
  -> m (Event t Route)
navItem route linkRoute matches inner =
  let routeClass currentRoute =
        if matches currentRoute
          then ("class", "nav-link active")
          else ("class", "nav-link")
      attr currentRoute =
        Map.fromList [("href", printRoute linkRoute), routeClass currentRoute]
      dynAttr = attr <$> route
  in  do
    clicks <- elClass "li" "nav-item" $ aDynAttr dynAttr inner
    return $ linkRoute <$ clicks
