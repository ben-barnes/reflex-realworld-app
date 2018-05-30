{-# LANGUAGE OverloadedStrings #-}

module RealWorld.Common.Data (
  Article(..)
, ArticleAuthor(..)
, ArticleAuthorBio(..)
, ArticleAuthorImage(..)
, ArticleAuthorFollowing(..)
, ArticleBody(..)
, ArticleCreatedAt(..)
, ArticleDescription(..)
, ArticleFavorited(..)
, ArticleFavoritesCount(..)
, ArticleSlug(..)
, ArticleTag(..)
, ArticleTitle(..)
, ArticleUpdatedAt(..)
, Articles(..)
, Username(..)
) where

import Data.Aeson ((.:), FromJSON, parseJSON, withObject)
import Data.Text (Text)
import Data.Time (UTCTime)

data Article = Article {
  articleSlug :: ArticleSlug
, articleTitle :: ArticleTitle
, articleDescription :: ArticleDescription
, articleBody :: ArticleBody
, articleTagList :: [ArticleTag]
, articleCreatedAt :: ArticleCreatedAt
, articleUpdatedAt :: ArticleUpdatedAt
, articleFavorited :: ArticleFavorited
, articleFavoritesCount :: ArticleFavoritesCount
, articleAuthor :: ArticleAuthor
} deriving (Eq)

instance FromJSON Article where
  parseJSON = withObject "Article" $ \v -> Article
    <$> v .: "slug"
    <*> v .: "title"
    <*> v .: "description"
    <*> v .: "body"
    <*> v .: "tagList"
    <*> v .: "createdAt"
    <*> v .: "updatedAt"
    <*> v .: "favorited"
    <*> v .: "favoritesCount"
    <*> v .: "author"

data ArticleAuthor = ArticleAuthor {
  articleAuthorUsername :: Username
, articleAuthorBio :: Maybe ArticleAuthorBio
, articleAuthorImage :: ArticleAuthorImage
, articleAuthorFollowing :: ArticleAuthorFollowing
} deriving (Eq)

instance FromJSON ArticleAuthor where
  parseJSON = withObject "ArticleAuthor" $ \v -> ArticleAuthor
    <$> v .: "username"
    <*> v .: "bio"
    <*> v .: "image"
    <*> v .: "following"

newtype ArticleAuthorBio = ArticleAuthorBio {
  getArticleAuthorBio :: Text
} deriving (Eq)

instance FromJSON ArticleAuthorBio where
  parseJSON = fmap ArticleAuthorBio . parseJSON

newtype ArticleAuthorFollowing = ArticleAuthorFollowing {
  getArticleAuthorFollowing :: Bool
} deriving (Eq)

instance FromJSON ArticleAuthorFollowing where
  parseJSON = fmap ArticleAuthorFollowing . parseJSON

newtype ArticleAuthorImage = ArticleAuthorImage {
  getArticleAuthorImage :: Text
} deriving (Eq)

instance FromJSON ArticleAuthorImage where
  parseJSON = fmap ArticleAuthorImage . parseJSON

newtype ArticleBody = ArticleBody {
  getArticleBody :: Text
} deriving (Eq)

instance FromJSON ArticleBody where
  parseJSON = fmap ArticleBody . parseJSON

newtype ArticleCreatedAt = ArticleCreatedAt {
  getArticleCreatedAt :: UTCTime
} deriving (Eq)

instance FromJSON ArticleCreatedAt where
  parseJSON = fmap ArticleCreatedAt . parseJSON

newtype ArticleDescription = ArticleDescription {
  getArticleDescription :: Text
} deriving (Eq)

instance FromJSON ArticleDescription where
  parseJSON = fmap ArticleDescription . parseJSON

newtype ArticleFavorited = ArticleFavorited {
  getArticleFavorited :: Bool
} deriving (Eq)

instance FromJSON ArticleFavorited where
  parseJSON = fmap ArticleFavorited . parseJSON

newtype ArticleFavoritesCount = ArticleFavoritesCount {
  getArticleFavoritesCount :: Int
} deriving (Eq)

instance FromJSON ArticleFavoritesCount where
  parseJSON = fmap ArticleFavoritesCount . parseJSON

newtype ArticleSlug = ArticleSlug {
  getArticleSlug :: Text
} deriving (Eq)

instance FromJSON ArticleSlug where
  parseJSON = fmap ArticleSlug . parseJSON

newtype ArticleTag = ArticleTag {
  getArticleTag :: Text
} deriving (Eq)

instance FromJSON ArticleTag where
  parseJSON = fmap ArticleTag . parseJSON

newtype ArticleTitle = ArticleTitle {
  getArticleTitle :: Text
} deriving (Eq)

instance FromJSON ArticleTitle where
  parseJSON = fmap ArticleTitle . parseJSON

newtype ArticleUpdatedAt = ArticleUpdatedAt {
  getArticleUpdatedAt :: UTCTime
} deriving (Eq)

instance FromJSON ArticleUpdatedAt where
  parseJSON = fmap ArticleUpdatedAt . parseJSON

data Articles = Articles {
  articlesArticles :: [Article]
, articlesArticlesCount :: ArticlesCount
} deriving (Eq)

instance FromJSON Articles where
  parseJSON = withObject "Articles" $ \v -> Articles
    <$> v .: "articles"
    <*> v .: "articlesCount"

newtype ArticlesCount = ArticlesCount {
  getArticlesCount :: Int
} deriving (Eq)

instance FromJSON ArticlesCount where
  parseJSON = fmap ArticlesCount . parseJSON

newtype Username = Username {
  getUsername :: Text
} deriving (Eq)

instance FromJSON Username where
  parseJSON = fmap Username . parseJSON
