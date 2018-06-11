{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

-- TODO:
-- [ ] Non-breaking space in New Post link
-- [ ] Format date in article preview

module Main (
  main
) where

import Conduit.Frontend.Components.Footer (conduitFooter)
import Conduit.Frontend.Components.Head (conduitHead)
import Conduit.Frontend.Components.Nav (conduitNav)
import Conduit.Frontend.Data.SessionEvent (chooseAuthEvent, chooseRouteEvent)
import Conduit.Frontend.Pages.Home (homePage, provisionHomePage)
import Conduit.Frontend.Pages.Login (loginPage)
import Conduit.Frontend.Pages.Register (registerPage)
import Conduit.Frontend.Router (routeLoop)
import Conduit.Frontend.Routes (
    Route(Home, Login, Register)
  , isHome
  , parseRoute
  , printRoute
  )
import Data.Functor (void)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom (
    Event
  , MonadWidget
  , blank
  , ffilter
  , fmapMaybe
  , holdDyn
  , leftmost
  , never
  , switchDyn
  , text
  , updated
  , widgetHold
  )
import Reflex.Dom.Core (mainWidgetWithHead)

main :: IO ()
main = run 3911 $ mainWidgetWithHead conduitHead $ do
  rec allRoutes    <- routeLoop Home printRoute parseRoute allInternalRoutes
      currentRoute <- holdDyn Home allRoutes
      navRoutes    <- conduitNav currentRoute
      bodyRoutes   <- conduitBody (updated currentRoute)
      footerRoutes <- conduitFooter
      let allInternalRoutes = leftmost [navRoutes, bodyRoutes, footerRoutes]
  return ()

conduitBody :: (MonadWidget t m) => Event t Route -> m (Event t Route)
conduitBody routes = do
  rec homePageAuth <- holdDyn Nothing (Just <$> authEvents)
      homePageData <- provisionHomePage (void $ ffilter isHome routes)
      let homePage'     = homePage homePageData homePageAuth
          loginPage'    = loginPage
          registerPage' = registerPage
          choosePage Home     = homePage'
          choosePage Login    = loginPage'
          choosePage Register = registerPage'
          choosePage _        = never <$ text "Not yet implemented."
      sessionEvents <- widgetHold (never <$ blank) (choosePage <$> routes)
      let sessionEventsFlat = switchDyn sessionEvents
      let authEvents = fmapMaybe chooseAuthEvent sessionEventsFlat
          routeEvents = fmapMaybe chooseRouteEvent sessionEventsFlat
  return routeEvents
