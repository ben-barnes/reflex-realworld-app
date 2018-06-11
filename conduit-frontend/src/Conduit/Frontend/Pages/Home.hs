{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Conduit.Frontend.Pages.Home (
  HomePageData(..)
, homePage
, provisionHomePage
) where

import Conduit.Common.Data (
    Article
  , ArticleSlug
  , Articles
  , ArticlesCount(ArticlesCount)
  , Profile
  , Tag
  , Tags
  , articleAuthor
  , articleCreatedAt
  , articleDescription
  , articleFavorited
  , articleFavoritesCount
  , articleSlug
  , articleTitle
  , articlesArticles
  , articlesArticlesCount
  , profileImage
  , profileUsername
  , getArticleCreatedAt
  , getArticleDescription
  , getArticleFavorited
  , getArticleFavoritesCount
  , getArticleTitle
  , getArticlesCount
  , getImage
  , getTag
  , getTags
  , getUsername
  )
import Conduit.Frontend.API (
    ArticlesLimit(ArticlesLimit)
  , ArticlesOffset(ArticlesOffset)
  , ArticlesRequest(ArticlesRequest)
  , AuthProfile
  , articles
  , getArticlesLimit
  , getArticlesOffset
  , tags
  , tagsResponseTags
  )
import Conduit.Frontend.Data.SessionEvent (SessionEvent(SessionRouteEvent))
import Conduit.Frontend.Data.WebData (
    ApiData
  , WebData(Loading, Failure, Success)
  , apiRequest
  , toMaybe
  )
import Conduit.Frontend.Routes (Route(Article, Profile), printRoute)
import Conduit.Frontend.Widgets (aAttr, buttonClass)
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom (
    (=:)
  , Dynamic
  , Event
  , MonadWidget
  , blank
  , constDyn
  , divClass
  , dyn
  , el
  , elAttr
  , elClass
  , elDynAttr
  , holdDyn
  , fanEither
  , fmapMaybe
  , leftmost
  , never
  , switchHoldPromptly
  , text
  , updated
  )

import qualified Data.Map as Map
import qualified Data.Text as Text

data HomePageData t = HomePageData {
  homePageDataArticles :: Dynamic t (ApiData Articles)
, homePageDataTags :: Dynamic t (ApiData Tags)
}

data FavoriteEvent
  = FavoriteArticle ArticleSlug
  | UnfavoriteArticle ArticleSlug
    deriving (Eq, Ord)

data Feed
  = YourFeed
  | GlobalFeed
    deriving (Eq, Ord)

provisionHomePage
  :: (MonadWidget t m)
  => Event t ()
  -> m (HomePageData t)
provisionHomePage trigger = do
  let articlesReq = ArticlesRequest
        Nothing
        Nothing
        Nothing
        (Just $ ArticlesLimit 10)
        Nothing
  articlesRes     <- apiRequest $ (articles articlesReq) <$ trigger
  tagsResponseRes <- apiRequest $ tags <$ trigger
  let tagsRes = fmap tagsResponseTags <$> tagsResponseRes
  return $ HomePageData articlesRes tagsRes

homePage
  :: (MonadWidget t m)
  => HomePageData t
  -> Dynamic t (Maybe AuthProfile)
  -> m (Event t SessionEvent)
homePage initialData authProfile =
  divClass "home-page" $ do
    banner
    divClass "container page" $
      divClass "row" $ do
        mainEvent <- divClass "col-md-9" $ do
          feedEvent    <- feedToggle
          dyn $ articleListPaginated <$> homePageDataArticles initialData
        sidebarEvent   <- divClass "col-md-3" $
          dyn $ sidebar <$> homePageDataTags initialData
        mainEventFlattened <- switchHoldPromptly never mainEvent
        return $ SessionRouteEvent <$> mainEventFlattened

banner :: (MonadWidget t m) => m ()
banner =
  divClass "banner" $
    divClass "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge."

feedToggle :: (MonadWidget t m) => m (Event t Feed)
feedToggle =
  divClass "feed-toggle" $
    elClass "ul" "nav nav-pills outline-active" $ do
      elClass "li" "nav-item" $ do
        feedToggleItem "Global Feed" GlobalFeed

feedToggleItem :: (MonadWidget t m) => Text -> Feed -> m (Event t Feed)
feedToggleItem label feed = 
  let attr = Map.fromList [("class", "nav-link active"), ("href", "")]
  in  elClass "li" "nav-item" $ do
        clicked <- aAttr attr $ text label
        return $ feed <$ clicked

articleListPaginated 
  :: (MonadWidget t m)
  => ApiData Articles
  -> m (Event t Route)
articleListPaginated as = do
  rec limit <- holdDyn (ArticlesLimit 10) limitEvent
      offset <- holdDyn (ArticlesOffset 0) offsetEvent
      articleListEventNested <- dyn $ articleList <$> limit <*> offset <*> allArticles
      articleListEvent <- switchHoldPromptly never articleListEventNested
      let (pageEvent, articleRoute) = fanEither articleListEvent
          limitEvent = fst <$> pageEvent
          offsetEvent = snd <$> pageEvent
          articlesRequest (limit, offset) =
            ArticlesRequest Nothing Nothing Nothing (Just limit) (Just offset)
          requestArticles = articles . articlesRequest <$> pageEvent
      subsequentArticles <- apiRequest requestArticles
      allArticles <- holdDyn as (updated subsequentArticles)
  return articleRoute

articleList
  :: (MonadWidget t m)
  => ArticlesLimit
  -> ArticlesOffset
  -> ApiData Articles
  -> m (Event t (Either (ArticlesLimit, ArticlesOffset) Route))
articleList limit offset as = case as of
  Loading     ->
    never <$ divClass "article-preview" (text "Loading articles...")
  Failure e   ->
    never <$ divClass "article-preview" (text "An error occured.")
  Success as' -> do
    articleRoute <- leftmost <$> traverse articlePreview (articlesArticles as')
    pageClicked  <- pagination (articlesArticlesCount as') limit offset
    return $ leftmost [Left <$> pageClicked, Right <$> articleRoute]

pagination
  :: (MonadWidget t m)
  => ArticlesCount
  -> ArticlesLimit
  -> ArticlesOffset
  -> m (Event t (ArticlesLimit, ArticlesOffset))
pagination count limit offset =
  let ac = getArticlesCount count
      al = getArticlesLimit limit
      ao = getArticlesOffset offset
      divCeil n d =
        let floored   = n `div` d
            remainder = n `mod` d
        in  if remainder > 0
              then floored + 1
              else floored
      pageCount   = ac `divCeil` al
      currentPage = 1 + (ao `divCeil` al)
      pageItemByNumber = \n -> pageItem (n == currentPage) n
      outputByPageNumber n = (ArticlesLimit al, ArticlesOffset (al * (n - 1)))
  in  el "nav" $ do
       elClass "ul" "pagination" $ do
        events <- traverse pageItemByNumber [1..pageCount]
        return $ outputByPageNumber <$> leftmost events

pageItem :: (MonadWidget t m) => Bool -> Int -> m (Event t Int)
pageItem active number = 
  let attr = if active
               then "class" =: "page-item active"
               else "class" =: "page-item"
      label = Text.pack . show $ number
  in  elAttr "li" attr $ do
        clicked <- aAttr ("class" =: "page-link") $ text label
        return $ number <$ clicked
  
articlePreview:: (MonadWidget t m) => Article -> m (Event t Route)
articlePreview article =
  divClass "article-preview" $ do
    articleMetaEvent <- divClass "article-meta" $ do
      authorImageEvent <- authorImage $ articleAuthor article
      articleInfoEvent <- articleInfo article
      favoritesEvent <- articleFavorites article
      return $ leftmost [
          authorImageEvent
        , articleInfoEvent
        ]
    previewLinkEvent <- previewLink article
    return $ leftmost [
        articleMetaEvent
      , previewLinkEvent
      ]

authorImage :: (MonadWidget t m) => Profile -> m (Event t Route)
authorImage profile =
  let route = Profile . profileUsername $ profile
      imageSrc = getImage . profileImage $ profile
      inner = elAttr "img" ("src" =: imageSrc) blank
  in  do
    imageClicked <- aAttr ("href" =: printRoute route) inner
    return $ route <$ imageClicked

articleInfo :: (MonadWidget t m) => Article -> m (Event t Route)
articleInfo article =
  let author = profileUsername . articleAuthor $ article
      createdAt = getArticleCreatedAt . articleCreatedAt $ article
      route = Profile author
      attrs = Map.fromList [("class", "author"), ("href", printRoute route)]
  in  divClass "info" $ do
        authorClicked <- aAttr attrs $ text . getUsername $ author
        elClass "span" "date" $ text . Text.pack . show $ createdAt
        return $ route <$ authorClicked

articleFavorites
  :: (MonadWidget t m)
  => Article
  -> m (Event t FavoriteEvent)
articleFavorites article = 
  let favorited = getArticleFavorited . articleFavorited $ article
      slug = articleSlug article
      highlight = if favorited then "btn-primary" else "btn-outline-primary"
      classes = "btn btn-sm pull-xs-right " <> highlight
      clickEvent = if favorited 
                     then UnfavoriteArticle slug
                     else FavoriteArticle slug
      count = getArticleFavoritesCount . articleFavoritesCount $ article
      inner = do
        elClass "i" "ion-heart" blank
        text . Text.pack . show $ count
  in  do
    favoritesButtonClicked <- buttonClass classes inner
    return $ clickEvent <$ favoritesButtonClicked

previewLink :: (MonadWidget t m) => Article -> m (Event t Route)
previewLink article =
  let attrs = Map.fromList [("class", "preview-link"), ("href", href)]
      href = printRoute route
      route = Article . articleSlug $ article
      inner = do
        el "h1" $ text . getArticleTitle . articleTitle $ article
        el "p" $ text . getArticleDescription . articleDescription $ article
        el "span" $ text "Read more..."
  in  do
    previewLinkClicked <- aAttr attrs inner
    return $ route <$ previewLinkClicked

sidebar :: (MonadWidget t m) => ApiData Tags -> m (Event t Tag)
sidebar ts =
  divClass "sidebar" $ do
    el "p" $ text "Popular Tags"
    case ts of
      Loading     -> never <$ text "Loading tags..."
      Failure e   -> never <$ text "An error occured."
      Success ts' -> tagList ts'

tagList :: (MonadWidget t m) => Tags -> m (Event t Tag)
tagList ts =
  divClass "tag-list" $
    leftmost <$> traverse tagPill (getTags ts)

tagPill :: (MonadWidget t m) => Tag -> m (Event t Tag)
tagPill t =
  let attrs = Map.fromList [("class", "tag-pill tag-default"), ("href", "")]
  in  do
    clicked <- aAttr attrs (text . getTag $ t)
    return $ t <$ clicked
