{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main (
  main
) where

import Conduit.Frontend.API (AuthProfile, authProfileToken, user)
import Conduit.Frontend.Components.Footer (conduitFooter)
import Conduit.Frontend.Components.Head (conduitHead)
import Conduit.Frontend.Components.Nav (conduitNav)
import Conduit.Frontend.Data.SessionEvent (
    SessionEvent
  , chooseAuthEvent
  , chooseRouteEvent
  )
import Conduit.Frontend.Data.WebData (apiRequest, toMaybe)
import Conduit.Frontend.Pages.Home (homePage, provisionHomePage)
import Conduit.Frontend.Pages.Login (loginPage)
import Conduit.Frontend.Pages.Register (registerPage)
import Conduit.Frontend.Pages.Settings (settingsPage)
import Conduit.Frontend.Router (routeLoop)
import Conduit.Frontend.Routes (
    Route(Article, Editor, Favorites, Home, Login, Profile, Register, Settings)
  , isHome
  , parseRoute
  , printRoute
  )
import Conduit.Frontend.Storage (retrieveToken, storeToken)
import Data.Functor (void)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom (
    Dynamic
  , Event
  , MonadWidget
  , blank
  , constDyn
  , dyn
  , ffilter
  , fmapMaybe
  , getPostBuild
  , holdDyn
  , leftmost
  , never
  , performEvent_
  , switchDyn
  , switchHoldPromptly
  , text
  , updated
  , widgetHold
  )
import Reflex.Dom.Core (mainWidgetWithHead)

main :: IO ()
main = run 3911 $ mainWidgetWithHead conduitHead $ do
  initialAuth  <- retrieveAuthEvent
  _ <- widgetHold blank (conduit <$> initialAuth)
  return ()

conduit
  :: (MonadWidget t m)
  => Maybe AuthProfile
  -> m ()
conduit initialAuth = do
  rec auth         <- holdDyn initialAuth internalAuth
      allRoutes    <- routeLoop Home printRoute parseRoute allInternalRoutes
      currentRoute <- holdDyn Home allRoutes
      navRoutes    <- conduitNav auth currentRoute 
      bodyEvents   <- choosePageDyn auth currentRoute
      footerRoutes <- conduitFooter
      let internalAuth      = fmapMaybe chooseAuthEvent bodyEvents
          bodyRoutes        = fmapMaybe chooseRouteEvent bodyEvents
          allInternalRoutes = leftmost [navRoutes, bodyRoutes, footerRoutes]
      storeAuthEvent internalAuth
  return ()

choosePageDyn
  :: (MonadWidget t m)
  => Dynamic t (Maybe AuthProfile)
  -> Dynamic t Route
  -> m (Event t SessionEvent)
choosePageDyn auth route = do
  es <- dyn $ choosePage <$> auth <*> route
  switchHoldPromptly never es
  
choosePage
  :: (MonadWidget t m)
  => Maybe AuthProfile
  -> Route
  -> m (Event t SessionEvent)
choosePage auth route = case route of
  Home          -> never <$ text "Not yet implemented"
  Login         -> loginPage
  Register      -> registerPage
  Settings      -> settingsPage undefined
  (Editor _)    -> never <$ text "Not yet implemented"
  (Article _)   -> never <$ text "Not yet implemented"
  (Profile _)   -> never <$ text "Not yet implemented"
  (Favorites _) -> never <$ text "Not yet implemented"

retrieveAuthEvent :: (MonadWidget t m) => m (Event t (Maybe AuthProfile))
retrieveAuthEvent = do
  onLoad <- getPostBuild
  maybeToken <- retrieveToken
  case maybeToken of
    Nothing    -> return $ Nothing <$ onLoad
    Just token -> do
      res <- apiRequest $ user token <$ onLoad
      return $ toMaybe <$> updated res

storeAuthEvent :: (MonadWidget t m) => Event t (Maybe AuthProfile) -> m ()
storeAuthEvent auth =
  performEvent_ $ storeToken . fmap authProfileToken <$> auth
