{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.API (
  ArticlesLimit(..)
, ArticlesOffset(..)
, ArticlesRequest(..)
, AuthProfile(..)
, Credentials(Credentials)
, LoginRequest(..)
, LoginResponse(..)
, NewUser(..)
, RegisterRequest(..)
, RegisterResponse(..)
, TagsResponse(..)
, articles
, login
, register
, tags
, user
) where

import Conduit.Common.Data (
    Bio
  , Email
  , Image
  , Password
  , Tag
  , Tags
  , Token
  , Username
  , getTag
  , getToken
  , getUsername
  )
import Conduit.Frontend.API.Errors (Errors)
import Control.Applicative ((<|>))
import Data.Aeson (
    (.:)
  , (.:?)
  , (.=)
  , FromJSON
  , ToJSON
  , toJSON
  , object
  , parseJSON
  , withObject
  )
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom (
    (=:)
  , XhrRequest
  , _xhrRequestConfig_headers
  , def
  , postJson
  , xhrRequest
  )

import qualified Data.Text as Text

prefix :: Text
prefix = "https://conduit.productionready.io/api"

login :: LoginRequest -> XhrRequest Text
login req = postJson (prefix <> "/users/login") req

articles :: ArticlesRequest -> XhrRequest ()
articles r =
  let qs = getQueryString . articlesQuery $ r
  in  xhrRequest "GET" (prefix <> "/articles" <> qs) def

register :: RegisterRequest -> XhrRequest Text
register req = postJson (prefix <> "/users") req

tags :: XhrRequest ()
tags = xhrRequest "GET" (prefix <> "/tags") def

user :: Token -> XhrRequest ()
user token = 
  let headers = "Authorization" =: ("Token " <> (getToken token))
      config  = def { _xhrRequestConfig_headers = headers }
  in  xhrRequest "GET" (prefix <> "/user") config

articlesQuery :: ArticlesRequest -> QueryString
articlesQuery r = queryString . catMaybes $ [
    (,) "tag" . getTag <$> articlesRequestTag r
  , (,) "author" . getUsername <$> articlesRequestAuthor r
  , (,) "favorited" . getUsername <$> articlesRequestFavorited r
  , (,) "limit" . showText . getArticlesLimit <$> articlesRequestLimit r
  , (,) "offset" . showText . getArticlesOffset <$> articlesRequestOffset r
  ]

queryString :: [(Text, Text)] -> QueryString
queryString [] = QueryString ""
queryString qs =
  let fmt (k, v) = k <> "=" <> v
  in  QueryString ("?" <> Text.intercalate "&" (map fmt qs))

showText :: (Show a) => a -> Text
showText = Text.pack . show

newtype ArticlesLimit = ArticlesLimit {
  getArticlesLimit :: Int
} deriving (Eq, Ord)

newtype ArticlesOffset = ArticlesOffset {
  getArticlesOffset :: Int
} deriving (Eq, Ord)

data ArticlesRequest = ArticlesRequest {
  articlesRequestTag :: Maybe Tag
, articlesRequestAuthor :: Maybe Username
, articlesRequestFavorited :: Maybe Username
, articlesRequestLimit :: Maybe ArticlesLimit
, articlesRequestOffset :: Maybe ArticlesOffset
} deriving (Eq, Ord)

data AuthProfile = AuthProfile {
  authProfileEmail :: Email
, authProfileToken :: Token
, authProfileUsername :: Username
, authProfileBio :: Maybe Bio
, authProfileImage :: Maybe Image
} deriving (Eq, Ord)

instance FromJSON AuthProfile where
  parseJSON = withObject "AuthProfile" $ \v ->
    v .: "user" >>= \u -> AuthProfile
      <$> u .: "email"
      <*> u .: "token"
      <*> u .: "username"
      <*> u .:? "bio"
      <*> u .:? "image"

data Credentials = Credentials {
  credentialsEmail :: Email
, credentialsPassword :: Password
} deriving (Eq, Ord)

instance ToJSON Credentials where
  toJSON c = object [
      "email" .= credentialsEmail c
    , "password" .= credentialsPassword c
    ]

data LoginRequest = LoginRequest {
  loginRequestUser :: Credentials
} deriving (Eq, Ord)

instance ToJSON LoginRequest where
  toJSON r = object ["user" .= loginRequestUser r]

data LoginResponse
  = LoginResponseFailure Errors
  | LoginResponseSuccess AuthProfile

instance FromJSON LoginResponse where
  parseJSON v =
        LoginResponseSuccess <$> parseJSON v
    <|> LoginResponseFailure <$> parseJSON v

data NewUser = NewUser {
  newUserUsername :: Username
, newUserEmail :: Email
, newUserPassword :: Password
} deriving (Eq, Ord)

instance ToJSON NewUser where
  toJSON n = object [
      "username" .= newUserUsername n
    , "email" .= newUserEmail n
    , "password" .= newUserPassword n
    ]

newtype QueryString = QueryString {
  getQueryString :: Text
} deriving (Eq, Ord)

newtype RegisterRequest = RegisterRequest {
  registerRequestUser :: NewUser
} deriving (Eq, Ord)

instance ToJSON RegisterRequest where
  toJSON r = object ["user" .= registerRequestUser r]

data RegisterResponse
  = RegisterResponseFailure Errors
  | RegisterResponseSuccess AuthProfile
    deriving (Eq, Ord)

instance FromJSON RegisterResponse where
  parseJSON v =
        RegisterResponseSuccess <$> parseJSON v
    <|> RegisterResponseFailure <$> parseJSON v

newtype TagsResponse = TagsResponse {
  tagsResponseTags :: Tags
} deriving (Eq, Ord)

instance FromJSON TagsResponse where
  parseJSON = withObject "TagsResponse" $ \v -> TagsResponse
    <$> v .: "tags"
