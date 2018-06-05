{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.API (
  ArticlesLimit(..)
, ArticlesRequest(..)
, AuthProfile(..)
, LoginRequest(..)
, NewUser(..)
, RegisterError(..)
, RegisterErrors(..)
, RegisterFailure(..)
, RegisterRequest(..)
, RegisterResponse(..)
, articles
, login
, register
) where

import Conduit.Common.Data (
    Bio
  , Email
  , Image
  , Password
  , Tag
  , Token
  , Username
  , getTag
  , getUsername
  )
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
import Reflex.Dom (XhrRequest, def, postJson, xhrRequest)

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
, authProfileBio :: Bio
, authProfileImage :: Image
} deriving (Eq, Ord)

instance FromJSON AuthProfile where
  parseJSON = withObject "AuthProfile" $ \v -> AuthProfile
    <$> v .: "email"
    <*> v .: "token"
    <*> v .: "username"
    <*> v .: "bio"
    <*> v .: "image"

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

newtype RegisterError = RegisterError {
  getRegisterError :: Text
} deriving (Eq, Ord)

instance FromJSON RegisterError where
  parseJSON = fmap RegisterError . parseJSON

data RegisterErrors = RegisterErrors {
  registerErrorsEmail :: Maybe [RegisterError]
, registerErrorsPassword :: Maybe [RegisterError]
, registerErrorsUsername :: Maybe [RegisterError]
} deriving (Eq, Ord)

instance FromJSON RegisterErrors where
  parseJSON = withObject "RegisterErrors" $ \v -> RegisterErrors
    <$> v .:? "email"
    <*> v .:? "password"
    <*> v .:? "username"

newtype RegisterFailure = RegisterFailure {
  registerFailureErrors :: RegisterErrors
} deriving (Eq, Ord)

instance FromJSON RegisterFailure where
  parseJSON = withObject "RegisterFailure" $ \v -> RegisterFailure
    <$> v .: "errors"

newtype RegisterRequest = RegisterRequest {
  registerRequestUser :: NewUser
} deriving (Eq, Ord)

instance ToJSON RegisterRequest where
  toJSON r = object ["user" .= registerRequestUser r]

data RegisterResponse
  = RegisterResponseFailure RegisterFailure
  | RegisterResponseSuccess AuthProfile
    deriving (Eq, Ord)

instance FromJSON RegisterResponse where
  parseJSON v =
        RegisterResponseSuccess <$> parseJSON v
    <|> RegisterResponseFailure <$> parseJSON v
