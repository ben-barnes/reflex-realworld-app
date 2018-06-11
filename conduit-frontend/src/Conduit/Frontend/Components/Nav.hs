{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Components.Nav (
  conduitNav
) where

import Conduit.Frontend.Routes (
    Route(Editor, Home, Register, Settings)
  , isEditor
  , isHome
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
  , elClass
  , leftmost
  , text
  )

import qualified Data.Map as Map

conduitNav :: (MonadWidget t m) => Dynamic t Route -> m (Event t Route)
conduitNav route =
  elClass "nav" "navbar navbar-light" $
    divClass "container" $ do
      bannerClicked <- banner
      navbarClicked <- navbar route
      return $ leftmost [bannerClicked, navbarClicked]

banner :: (MonadWidget t m) => m (Event t Route)
banner =
  let attrs = Map.fromList [("href", printRoute Home), ("class", "navbar-brand")]
  in  do
    clicked <- aAttr attrs $ text "conduit"
    return $ Home <$ clicked

-- Not authed: Home SignIn SignUp
-- Authed: Home NewArticle Settings (Profile Username)

navbar :: (MonadWidget t m) => Dynamic t Route -> m (Event t Route)
navbar route =
  elClass "ul" "nav navbar-nav pull-xs-right" $ do
    homeRoutes <- navItem route Home isHome $
      text "Home"
    editorRoutes <- navItem route (Editor Nothing) isEditor $ do
      elClass "i" "ion-compose" blank
      text " New Post"
    settingsRoutes <- navItem route Settings isSettings $ do
      elClass "i" "ion-gear-a" blank
      text " Settings"
    registerRoutes <- navItem route Register isRegister $ 
      text "Sign up"
    return $
      leftmost [homeRoutes, editorRoutes, settingsRoutes, registerRoutes]

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
