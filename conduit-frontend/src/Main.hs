{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

-- TODO:
-- [ ] Non-breaking space in New Post link
-- [x] Dynamic active link in nav
-- [ ] Format date in article preview
-- [ ] Set hash to match chosen route

module Main (
  main
) where

import Data.Functor (void)
import Data.Text (Text)
import Language.Javascript.JSaddle.Warp (run)
import Conduit.Frontend.Components.Head (conduitHead)
import Conduit.Frontend.Components.Nav (conduitNav)
import Conduit.Frontend.Pages.Home (homePage)
import Conduit.Frontend.Pages.Register (registerPage)
import Conduit.Frontend.Router (routeLoop)
import Conduit.Frontend.Routes (Route, parseRoute, printRoute)
import Reflex.Dom hiding (mainWidgetWithHead, run)
import Reflex.Dom.Core (mainWidgetWithHead)

import qualified Data.Map as Map
import qualified Conduit.Frontend.Routes as Route

main :: IO ()
main = run 3911 $ mainWidgetWithHead conduitHead $ do
  rec routeEvents <- routeLoop Route.Home printRoute parseRoute navRoutes
      currentRoute <- holdDyn Route.Home routeEvents
      navRoutes <- conduitNav currentRoute
  dynText (printRoute <$> currentRoute)
  conduitFooter

conduitBody :: (MonadWidget t m) => Dynamic t Route -> m ()
conduitBody route =
  void . dyn $ choosePage (void $ updated route) <$> route

choosePage :: (MonadWidget t m) => Event t () -> Route -> m ()
choosePage _      Route.Register = void $ registerPage
choosePage onLoad _              = void $ homePage onLoad

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
