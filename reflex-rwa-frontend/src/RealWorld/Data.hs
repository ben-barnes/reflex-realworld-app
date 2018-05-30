module RealWorld.Data (
  ArticleSlug(..)
, Username(..)
) where

import Data.Text (Text)
  
newtype ArticleSlug = ArticleSlug {
  getArticleSlug :: Text
} deriving (Eq, Ord)

newtype Username = Username {
  getUsername :: Text
} deriving (Eq, Ord)

