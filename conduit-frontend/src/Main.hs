{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

-- TODO:
-- [ ] Non-breaking space in New Post link
-- [x] Dynamic active link in nav
-- [ ] Format date in article preview

module Main (
  main
) where

import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.Javascript.JSaddle.Warp (run)
import Conduit.Common.Data (Articles(Articles), ArticlesCount(ArticlesCount))
import Conduit.Frontend.API (ArticlesRequest(ArticlesRequest), articles)
import Conduit.Frontend.Components
import Conduit.Frontend.Pages.Home (homePage)
import Conduit.Frontend.Pages.Register (registerPage)
import Conduit.Frontend.Routing (Route, parseRoute)
import Reflex.Dom hiding (mainWidgetWithHead, run)
import Reflex.Dom.Core (mainWidgetWithHead)

import qualified Data.Map as Map
import qualified Conduit.Frontend.Routing as Route

main :: IO ()
main = run 3911 $ mainWidgetWithHead conduitHead $ do
  onLoad <- getPostBuild
  windowHash <- currentHash
  windowHashChange <- hashChangeEvent
  let redirect404 (Right r) = r
      redirect404 (Left _) = Route.Home
      parseRouteRedirect = redirect404 . parseRoute
      windowRoute = parseRouteRedirect <$> windowHash
      initialRoute = fromMaybe Route.Home windowRoute
      initialRouteEvent = const initialRoute <$> onLoad
      allWindowRouteEvents = leftmost [initialRouteEvent, parseRouteRedirect <$> windowHashChange]
  rec navRouteEvent <- conduitNav currentRoute
      let routeChange = leftmost [allWindowRouteEvents, navRouteEvent]
      currentRoute <- holdDyn Route.Home routeChange
  conduitBody currentRoute
  conduitFooter

conduitHead :: (MonadWidget t m) => m ()
conduitHead = do
  meta [("charset", "utf-8")]
  stylesheet "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
  stylesheet "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic"
  stylesheet "//demo.productionready.io/main.css"
  el "title" $ text "Conduit"

conduitBody :: (MonadWidget t m) => Dynamic t Route -> m ()
conduitBody route =
  void . dyn $ choosePage <$> route

choosePage :: (MonadWidget t m) => Route -> m ()
choosePage Route.Register = void $ registerPage
choosePage _ = void $ homePage (Articles [] (ArticlesCount 0))

conduitNav :: (MonadWidget t m) => Dynamic t Route -> m (Event t Route)
conduitNav route =
  let routeClass p =
        fmap (\currentRoute -> if p currentRoute then active else inactive) route
      active = "nav-link active"
      inactive = "nav-link"
      aAttrs href p = (\rc -> Map.fromList [("class", rc), ("href", href)]) <$> routeClass p
  in  elClass "nav" "navbar navbar-light" $
        divClass "container" $ do
          anchorClass "/" "navbar-brand" $ text "conduit"
          elClass "ul" "nav navbar-nav pull-xs-right" $ do
            homeClicks <- elClass "li" "nav-item" $
              aDynAttr (aAttrs "#/" Route.isHome) $
                text "Home"
            editorClicks <- elClass "li" "nav-item" $
              aDynAttr (aAttrs "#/compose" Route.isEditor) $ do
                elClass "i" "ion-compose" blank
                text " New Post"
            settingsClicks <- elClass "li" "nav-item" $
              aDynAttr (aAttrs "#/settings" Route.isSettings) $ do
                elClass "i" "ion-gear-a" blank
                text " Settings"
            registerClicks <- elClass "li" "nav-item" $
              aDynAttr (aAttrs "#/register" Route.isRegister) $
                text "Sign up"
            return $
              leftmost [
                  fmap (const Route.Home) homeClicks
                , fmap (const $ Route.Editor Nothing) editorClicks
                , fmap (const Route.Settings) settingsClicks
                , fmap (const Route.Register) registerClicks
                ]

conduitFooter :: (MonadWidget t m) => m ()
conduitFooter =
  el "footer" $
    divClass "container" $ do
      anchorClass "/" "logo-font" $ text "conduit"
      elClass "span" "attribution" $ do
        text "An interactive learning project from "
        anchor "https://thinkster.io" $ text "Thinkster"
        text ". Code & design licensed under MIT."

anchor :: (MonadWidget t m) => Text -> m a -> m a
anchor href =
  elAttr "a" (Map.fromList [("href", href)])

anchorClass :: (MonadWidget t m) => Text -> Text -> m a -> m a
anchorClass href cls =
  elAttr "a" (Map.fromList [("class", cls), ("href", href)])

meta :: (MonadWidget t m) => [(Text, Text)] -> m ()
meta =
  singleTag "meta"

stylesheet :: (MonadWidget t m) => Text -> m ()
stylesheet url =
  singleTag "link" [("href", url), ("rel", "stylesheet"), ("type", "text/css")]

singleTag :: (MonadWidget t m) => Text -> [(Text, Text)] -> m ()
singleTag name contents =
  elAttr name (Map.fromList contents) blank
