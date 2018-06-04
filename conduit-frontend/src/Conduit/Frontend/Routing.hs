{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Routing (
  Route(..)
, UnknownRoute(..)
, isArticle
, isHome
, isEditor
, isFavorites
, isLogin
, isProfile
, isSettings
, isRegister
, parseRoute
, renderRoute
) where

import Conduit.Common.Data (ArticleSlug, Username, getArticleSlug, getUsername)
import Data.Semigroup ((<>))
import Data.Text (Text)

data Route
  = Home
  | Login
  | Register
  | Settings
  | Editor (Maybe ArticleSlug)
  | Article ArticleSlug
  | Profile Username
  | Favorites Username
    deriving (Eq, Ord)

newtype UnknownRoute = UnknownRoute {
  getUnknownRoute :: Text
} deriving (Eq, Ord)

isHome :: Route -> Bool
isHome Home = True
isHome _ = False

isLogin :: Route -> Bool
isLogin Login = True
isLogin _ = False

isRegister :: Route -> Bool
isRegister Register = True
isRegister _ = False

isSettings :: Route -> Bool
isSettings Settings = True
isSettings _ = False

isEditor :: Route -> Bool
isEditor (Editor _) = True
isEditor _ = False

isArticle :: Route -> Bool
isArticle (Article _) = True
isArticle _ = False

isProfile :: Route -> Bool
isProfile (Profile _) = True
isProfile _ = False

isFavorites :: Route -> Bool
isFavorites (Favorites _) = True
isFavorites _ = False

parseRoute :: Text -> Either UnknownRoute Route
parseRoute "#/home"     = Right Home
parseRoute "#/register" = Right Register
parseRoute r            = Left (UnknownRoute r)

renderRoute :: Route -> Text
renderRoute r =
  "#/" <> case r of
    Home        -> "home"
    Login       -> "login"
    Register    -> "register"
    Settings    -> "settings"
    Editor a    -> "editor" <> maybe "" (\a' -> "/" <> getArticleSlug a') a
    Article a   -> "article/" <> getArticleSlug a
    Profile u   -> "profile/" <> getUsername u
    Favorites u -> "profile/" <> getUsername u <> "/favorites"
