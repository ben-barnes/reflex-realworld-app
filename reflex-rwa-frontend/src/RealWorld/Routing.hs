module RealWorld.Routing (
  Route(..)
, isHome
, isEditor
, isSettings
, isRegister
) where

import RealWorld.Common.Data (ArticleSlug, Username)

data Route
  = Home
  | Login
  | Register
  | Settings
  | Editor (Maybe ArticleSlug)
  | Article ArticleSlug
  | Profile Username
  | Favorites Username
    deriving (Eq)

isHome :: Route -> Bool
isHome Home = True
isHome _ = False

isEditor :: Route -> Bool
isEditor (Editor _) = True
isEditor _ = False

isSettings :: Route -> Bool
isSettings Settings = True
isSettings _ = False

isRegister :: Route -> Bool
isRegister Register = True
isRegister _ = False
