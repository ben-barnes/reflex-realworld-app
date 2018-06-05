{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Routes (
  Route(..)
, isArticle
, isHome
, isEditor
, isFavorites
, isLogin
, isProfile
, isSettings
, isRegister
, parseRoute
, printRoute
) where

import Conduit.Common.Data (
    ArticleSlug(ArticleSlug)
  , Username(Username)
  , getArticleSlug
  , getUsername
  )
import Data.Semigroup ((<>))
import Data.Text (Text)

import qualified Data.Text as Text

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

parseRoute :: Text -> Either Route Route
parseRoute r =
  let tokens = filter (not . Text.null) $ Text.splitOn "/" r
  in  case tokens of
    ["#home"]                    -> Right Home
    ["#login"]                   -> Right Login
    ["#register"]                -> Right Register
    ["#settings"]                -> Right Settings
    ["#editor"]                  -> Right (Editor Nothing)
    ["#editor", s]               -> Right (Editor (Just (ArticleSlug s)))
    ["#article", s]              -> Right (Article (ArticleSlug s))
    ["#profile", u]              -> Right (Profile (Username u))
    ["#profile", u, "favorites"] -> Right (Favorites (Username u))
    _                           -> Left Home

printRoute :: Route -> Text
printRoute Home          = "#home"
printRoute Login         = "#login"
printRoute Register      = "#register"
printRoute Settings      = "#settings"
printRoute (Editor a)    = "#editor" <> maybe "" (("/" <>) . getArticleSlug) a
printRoute (Article a)   = "#article/" <> getArticleSlug a
printRoute (Profile u)   = "#profile/" <> getUsername u
printRoute (Favorites u) = "#profile/" <> getUsername u <> "/favorites"
