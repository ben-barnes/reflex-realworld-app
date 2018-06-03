{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Conduit.Frontend.Pages.Home (
  homePage
) where

import Conduit.Common.Data (
    Article
  , ArticleSlug
  , Articles
  , Profile
  , Tag(Tag)
  , Tags(Tags)
  , articleAuthor
  , articleCreatedAt
  , articleDescription
  , articleFavorited
  , articleFavoritesCount
  , articleSlug
  , articleTitle
  , articlesArticles
  , profileImage
  , profileUsername
  , getArticleCreatedAt
  , getArticleDescription
  , getArticleFavorited
  , getArticleFavoritesCount
  , getArticleTitle
  , getImage
  , getTag
  , getTags
  , getUsername
  )
import Conduit.Frontend.Components (aAttr, aDynAttr)
import Conduit.Frontend.Routing (Route(Article, Profile), renderRoute)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom (
    (=:)
  , Dynamic
  , Event
  , EventName(Click)
  , MonadWidget
  , blank
  , divClass
  , domEvent
  , el
  , elAttr
  , elClass
  , elClass'
  , holdDyn
  , leftmost
  , text
  )

import qualified Data.Map as Map
import qualified Data.Text as Text

data ArticlePreviewEvent
  = ArticlePreviewEventRoute Route
  | ArticlePreviewEventFavorite FavoriteEvent
    deriving (Eq, Ord)

data FavoriteEvent
  = FavoriteArticle ArticleSlug
  | UnfavoriteArticle ArticleSlug
    deriving (Eq, Ord)

data Feed
  = YourFeed
  | GlobalFeed
    deriving (Eq, Ord)

newtype TagClicked = TagClicked {
  clickedTag :: Tag
} deriving (Eq, Ord)

homePage :: (MonadWidget t m) => Articles -> m ()
homePage as =
  divClass "home-page" $ do
    banner
    divClass "container page" $
      divClass "row" $ do
        divClass "col-md-9" $ do
          rec feedClicked <- feedToggle feedDyn
              feedDyn <- holdDyn YourFeed feedClicked
          traverse_ articlePreview (articlesArticles as)
        divClass "col-md-3" $
          void $ sidebar $ Tags [
              Tag "programming"
            , Tag "javascript"
            , Tag "emberjs"
            , Tag "angularjs"
            , Tag "react"
            , Tag "mean"
            , Tag "node"
            , Tag "rails"
            ]

banner :: (MonadWidget t m) => m ()
banner =
  divClass "banner" $
    divClass "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge."

feedToggle :: (MonadWidget t m) => Dynamic t Feed -> m (Event t Feed)
feedToggle feedDyn =
  divClass "feed-toggle" $
    elClass "ul" "nav nav-pills outline-active" $ do
      elClass "li" "nav-item" $ do
        yourFeedClicked <- feedToggleItem "Your Feed" YourFeed feedDyn
        globalFeedClicked <- feedToggleItem "Global Feed" GlobalFeed feedDyn
        return $ leftmost [yourFeedClicked, globalFeedClicked]

feedToggleItem :: (MonadWidget t m) => Text -> Feed -> Dynamic t Feed -> m (Event t Feed)
feedToggleItem label feed feedDyn = 
  let feedAttr currentFeed = if currentFeed == feed then active else inactive
      active = Map.fromList [("class", "nav-link active"), ("href", "")]
      inactive = Map.fromList [("class", "nav-link disabled"), ("href", "")]
      feedAttrDyn = feedAttr <$> feedDyn
  in  elClass "li" "nav-item" $ do
        clicked <- aDynAttr feedAttrDyn $ text label
        return $ const feed <$> clicked

articlePreview:: (MonadWidget t m) => Article -> m (Event t ArticlePreviewEvent)
articlePreview article =
  divClass "article-preview" $ do
    articleMetaEvent <- divClass "article-meta" $ do
      authorImageEvent <- authorImage $ articleAuthor article
      articleInfoEvent <- articleInfo article
      favoritesEvent <- articleFavorites article
      return $ leftmost [
          ArticlePreviewEventRoute <$> authorImageEvent
        , ArticlePreviewEventRoute <$> articleInfoEvent
        , ArticlePreviewEventFavorite <$> favoritesEvent
        ]
    previewLinkEvent <- previewLink article
    return $ leftmost [
        articleMetaEvent
      , ArticlePreviewEventRoute <$> previewLinkEvent
      ]

authorImage :: (MonadWidget t m) => Profile -> m (Event t Route)
authorImage profile =
  let route = Profile . profileUsername $ profile
      imageSrc = getImage . profileImage $ profile
      inner = elAttr "img" ("src" =: imageSrc) blank
  in  do
    imageClicked <- aAttr ("href" =: renderRoute route) inner
    return $ const route <$> imageClicked

articleInfo :: (MonadWidget t m) => Article -> m (Event t Route)
articleInfo article =
  let author = profileUsername . articleAuthor $ article
      createdAt = getArticleCreatedAt . articleCreatedAt $ article
      route = Profile author
      attrs = Map.fromList [("class", "author"), ("href", renderRoute route)]
  in  divClass "info" $ do
        authorClicked <- aAttr attrs $ text . getUsername $ author
        elClass "span" "date" $ text . Text.pack . show $ createdAt
        return $ const route <$> authorClicked

articleFavorites
  :: (MonadWidget t m)
  => Article
  -> m (Event t FavoriteEvent)
articleFavorites article = 
  let favorited = getArticleFavorited . articleFavorited $ article
      slug = articleSlug article
      highlight = if favorited then "btn-primary" else "btn-outline-primary"
      classes = "btn btn-sm pull-xs-right " <> highlight
      clickEvent = if favorited then UnfavoriteArticle slug else FavoriteArticle slug
      inner = do
        elClass "i" "ion-heart" blank
        text . Text.pack . show . getArticleFavoritesCount . articleFavoritesCount $ article
  in  do
    favoritesButtonClicked <- buttonClass classes inner
    return $ const clickEvent <$> favoritesButtonClicked

previewLink :: (MonadWidget t m) => Article -> m (Event t Route)
previewLink article =
  let attrs = Map.fromList [("class", "preview-link"), ("href", href)]
      href = renderRoute route
      route = Article . articleSlug $ article
      inner = do
        el "h1" $ text . getArticleTitle . articleTitle $ article
        el "p" $ text . getArticleDescription . articleDescription $ article
        el "span" $ text "Read more..."
  in  do
    previewLinkClicked <- aAttr attrs inner
    return $ const route <$> previewLinkClicked

buttonClass :: (MonadWidget t m) => Text -> m () -> m (Event t ())
buttonClass cls inner = do
  (e, _) <- elClass' "button" cls inner
  return $ domEvent Click e

sidebar :: (MonadWidget t m) => Tags -> m (Event t TagClicked)
sidebar tags =
  divClass "sidebar" $ do
    el "p" $ text "Popular Tags"
    tagList tags

tagList :: (MonadWidget t m) => Tags -> m (Event t TagClicked)
tagList ts =
  divClass "tag-list" $
    leftmost <$> traverse tagPill (getTags ts)

tagPill :: (MonadWidget t m) => Tag -> m (Event t TagClicked)
tagPill t =
  let attrs = Map.fromList [("class", "tag-pill tag-default"), ("href", "")]
      tagClicked e = const (TagClicked t) <$> e
  in  tagClicked <$> (aAttr attrs $ text . getTag $ t)
