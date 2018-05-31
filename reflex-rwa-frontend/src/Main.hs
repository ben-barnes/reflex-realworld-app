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

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.Javascript.JSaddle.Warp (run)
import RealWorld.Common.Data
import RealWorld.Routing (Route)
import Reflex.Dom hiding (mainWidgetWithHead, run)
import Reflex.Dom.Core (mainWidgetWithHead)

import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.MouseEvent as DOM
import qualified RealWorld.Routing as Route

main :: IO ()
main = run 3911 $ mainWidgetWithHead conduitHead $ do
  onLoad <- getPostBuild
  let initRouteChange = fmap (const Route.Home) onLoad
  rec navRouteChange <- conduitNav currentRoute
      let routeChange = leftmost [initRouteChange, navRouteChange]
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
  let articlesUrl = "https://conduit.productionready.io/api/articles"
  in  do
    response <- getAndDecode $ fmap (const articlesUrl) (updated route)
    let success = fmapMaybe id response
    widgetHold (text "No response received.") (fmap conduitHomePage success)
    return ()

conduitHomePage :: (MonadWidget t m) => Articles -> m ()
conduitHomePage as =
  divClass "home-page" $ do
    divClass "banner" $
      divClass "container" $ do
        elClass "h1" "logo-font" $ text "conduit"
        el "p" $ text "A place to share your knowledge."
    divClass "container page" $
      divClass "row" $ do
        divClass "col-md-9" $ do
          divClass "feed-toggle" $
            elClass "ul" "nav nav-pills outline-active" $ do
              elClass "li" "nav-item" $ anchorClass "" "nav-link disabled" $ text "Your Feed"
              elClass "li" "nav-item" $ anchorClass "" "nav-link active" $ text "Global Feed"
          traverse_ articlePreview (articlesArticles as)
        divClass "col-md-3" $
          divClass "sidebar" $ do
            el "p" $ text "Popular Tags"
            divClass "tag-list" $ do
              anchorClass "" "tag-pill tag-default" $ text "programming"
              anchorClass "" "tag-pill tag-default" $ text "javascript"
              anchorClass "" "tag-pill tag-default" $ text "emberjs"
              anchorClass "" "tag-pill tag-default" $ text "angularjs"
              anchorClass "" "tag-pill tag-default" $ text "react"
              anchorClass "" "tag-pill tag-default" $ text "mean"
              anchorClass "" "tag-pill tag-default" $ text "node"
              anchorClass "" "tag-pill tag-default" $ text "rails"

articlePreview:: (MonadWidget t m) => Article -> m ()
articlePreview a =
  divClass "article-preview" $ do
    divClass "article-meta" $ do
      anchor "/profile" $ elAttr "img" (Map.singleton "src" (getArticleAuthorImage . articleAuthorImage . articleAuthor $ a)) blank
      divClass "info" $ do
        anchorClass "" "author" $ text . getUsername . articleAuthorUsername . articleAuthor $ a
        elClass "span" "date" $ text . T.pack . show . getArticleCreatedAt . articleCreatedAt $ a
      buttonClass "btn btn-outline-primary btn-sm pull-xs-right" $ do
        elClass "i" "ion-heart" blank
        text . T.pack . show . getArticleFavoritesCount . articleFavoritesCount $ a
      return ()
    anchorClass "" "preview-link" $ do
      el "h1" $ text . getArticleTitle . articleTitle $ a
      el "p" $ text . getArticleDescription . articleDescription $ a
      el "span" $ text "Read more..."

conduitNav :: (MonadWidget t m) => Dynamic t Route -> m (Event t Route)
conduitNav route =
  let routeClass p =
        fmap (\currentRoute -> if p currentRoute then active else inactive) route
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
              anchorDynClassClickable "/" (routeClass Route.isEditor) $ do
                elClass "i" "ion-compose" blank
                text " New Post"
            settingsClicks <- elClass "li" "nav-item" $
              anchorDynClassClickable "/" (routeClass Route.isSettings) $ do
                elClass "i" "ion-gear-a" blank
                text " Settings"
            registerClicks <- elClass "li" "nav-item" $
              anchorDynClassClickable "/" (routeClass Route.isRegister) $
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

anchorDynClass :: (MonadWidget t m) => Text -> Dynamic t Text -> m a -> m a
anchorDynClass href cls =
  let attrs = fmap (\cls' -> Map.fromList [("class", cls'), ("href", href)]) cls
  in  elDynAttr "a" attrs

anchorDynClassClickable
  :: (DOM.IsGlobalEventHandlers (m a), MonadWidget t m)
  => Text
  -> Dynamic t Text
  -> m a
  -> m (Event t ())
anchorDynClassClickable href cls inner =
  let attrs = fmap (\cls' -> Map.fromList [("class", cls'), ("href", href)]) cls
  in  do
    (e, _) <- elDynAttr' "a" attrs inner
    wrapDomEvent (_element_raw e) (`DOM.on` DOM.click) DOM.preventDefault

buttonClass :: (MonadWidget t m) => Text -> m () -> m (Event t ())
buttonClass cls inner = do
  (e, _) <- elClass' "button" cls inner
  return $ domEvent Click e

meta :: (MonadWidget t m) => [(Text, Text)] -> m ()
meta =
  singleTag "meta"

stylesheet :: (MonadWidget t m) => Text -> m ()
stylesheet url =
  singleTag "link" [("href", url), ("rel", "stylesheet"), ("type", "text/css")]

singleTag :: (MonadWidget t m) => Text -> [(Text, Text)] -> m ()
singleTag name contents =
  elAttr name (Map.fromList contents) blank
