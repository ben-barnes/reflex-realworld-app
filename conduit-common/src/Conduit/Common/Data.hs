{-# LANGUAGE OverloadedStrings #-}

module Conduit.Common.Data (
  Article(..)
, ArticleBody(..)
, ArticleCreatedAt(..)
, ArticleDescription(..)
, ArticleFavorited(..)
, ArticleFavoritesCount(..)
, ArticleSlug(..)
, ArticleTitle(..)
, ArticleUpdatedAt(..)
, Articles(..)
, ArticlesCount(..)
, Bio(..)
, Email(..)
, Image(..)
, Password(..)
, Profile(..)
, ProfileFollowing(..)
, Tag(..)
, Tags(..)
, Token(..)
, Username(..)
) where

import Data.Aeson ((.:), FromJSON, ToJSON, parseJSON, toJSON, withObject)
import Data.Text (Text)
import Data.Time (UTCTime)

data Article = Article {
  articleSlug :: ArticleSlug
, articleTitle :: ArticleTitle
, articleDescription :: ArticleDescription
, articleBody :: ArticleBody
, articleTagList :: Tags
, articleCreatedAt :: ArticleCreatedAt
, articleUpdatedAt :: ArticleUpdatedAt
, articleFavorited :: ArticleFavorited
, articleFavoritesCount :: ArticleFavoritesCount
, articleAuthor :: Profile
} deriving (Eq, Ord)

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

newtype ArticleBody = ArticleBody {
  getArticleBody :: Text
} deriving (Eq, Ord)

instance FromJSON ArticleBody where
  parseJSON = fmap ArticleBody . parseJSON

newtype ArticleCreatedAt = ArticleCreatedAt {
  getArticleCreatedAt :: UTCTime
} deriving (Eq, Ord)

instance FromJSON ArticleCreatedAt where
  parseJSON = fmap ArticleCreatedAt . parseJSON

newtype ArticleDescription = ArticleDescription {
  getArticleDescription :: Text
} deriving (Eq, Ord)

instance FromJSON ArticleDescription where
  parseJSON = fmap ArticleDescription . parseJSON

newtype ArticleFavorited = ArticleFavorited {
  getArticleFavorited :: Bool
} deriving (Eq, Ord)

instance FromJSON ArticleFavorited where
  parseJSON = fmap ArticleFavorited . parseJSON

newtype ArticleFavoritesCount = ArticleFavoritesCount {
  getArticleFavoritesCount :: Int
} deriving (Eq, Ord)

instance FromJSON ArticleFavoritesCount where
  parseJSON = fmap ArticleFavoritesCount . parseJSON

newtype ArticleSlug = ArticleSlug {
  getArticleSlug :: Text
} deriving (Eq, Ord)

instance FromJSON ArticleSlug where
  parseJSON = fmap ArticleSlug . parseJSON

newtype ArticleTitle = ArticleTitle {
  getArticleTitle :: Text
} deriving (Eq, Ord)

instance FromJSON ArticleTitle where
  parseJSON = fmap ArticleTitle . parseJSON

newtype ArticleUpdatedAt = ArticleUpdatedAt {
  getArticleUpdatedAt :: UTCTime
} deriving (Eq, Ord)

instance FromJSON ArticleUpdatedAt where
  parseJSON = fmap ArticleUpdatedAt . parseJSON

data Articles = Articles {
  articlesArticles :: [Article]
, articlesArticlesCount :: ArticlesCount
} deriving (Eq, Ord)

instance FromJSON Articles where
  parseJSON = withObject "Articles" $ \v -> Articles
    <$> v .: "articles"
    <*> v .: "articlesCount"

newtype ArticlesCount = ArticlesCount {
  getArticlesCount :: Int
} deriving (Eq, Ord)

instance FromJSON ArticlesCount where
  parseJSON = fmap ArticlesCount . parseJSON

newtype Email = Email {
  getEmail :: Text
} deriving (Eq, Ord)

instance FromJSON Email where
  parseJSON = fmap Email . parseJSON

instance ToJSON Email where
  toJSON = toJSON . getEmail

newtype Password = Password {
  getPassword :: Text
} deriving (Eq, Ord)

instance FromJSON Password where
  parseJSON = fmap Password . parseJSON

instance ToJSON Password where
  toJSON = toJSON . getPassword

data Profile = Profile {
  profileUsername :: Username
, profileBio :: Maybe Bio
, profileImage :: Image
, profileFollowing :: ProfileFollowing
} deriving (Eq, Ord)

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \v -> Profile
    <$> v .: "username"
    <*> v .: "bio"
    <*> v .: "image"
    <*> v .: "following"

newtype Bio = Bio {
  getBio :: Text
} deriving (Eq, Ord)

instance FromJSON Bio where
  parseJSON = fmap Bio . parseJSON

newtype ProfileFollowing = ProfileFollowing {
  getProfileFollowing :: Bool
} deriving (Eq, Ord)

instance FromJSON ProfileFollowing where
  parseJSON = fmap ProfileFollowing . parseJSON

newtype Image = Image {
  getImage :: Text
} deriving (Eq, Ord)

instance FromJSON Image where
  parseJSON = fmap Image . parseJSON

newtype Tag = Tag {
  getTag :: Text
} deriving (Eq, Ord)

instance FromJSON Tag where
  parseJSON = fmap Tag . parseJSON

newtype Tags = Tags {
  getTags :: [Tag]
} deriving (Eq, Ord)

instance FromJSON Tags where
  parseJSON = fmap Tags . parseJSON

newtype Token = Token {
  getToken :: Text
} deriving (Eq, Ord)

instance FromJSON Token where
  parseJSON = fmap Token . parseJSON

newtype Username = Username {
  getUsername :: Text
} deriving (Eq, Ord)

instance FromJSON Username where
  parseJSON = fmap Username . parseJSON

instance ToJSON Username where
  toJSON = toJSON . getUsername
