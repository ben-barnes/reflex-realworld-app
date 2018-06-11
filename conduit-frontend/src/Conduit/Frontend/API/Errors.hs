{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.API.Errors (
  ErrorCondition(..)
, ErrorMessage(..)
, ErrorTarget(..)
, Errors(..)
, renderErrors
) where

import Data.Aeson (
    (.:)
  , FromJSON
  , FromJSONKey
  , parseJSON
  , withObject
  )
import Data.Map (Map)
import Data.Semigroup ((<>))
import Data.Text (Text)

import qualified Data.Map as Map

newtype ErrorCondition = ErrorCondition {
  getErrorCondition :: Text
} deriving (Eq, Ord)

instance FromJSON ErrorCondition where
  parseJSON = fmap ErrorCondition . parseJSON

newtype ErrorMessage = ErrorMessage {
  getErrorMessage :: Text
} deriving (Eq, Ord)

instance FromJSON ErrorMessage where
  parseJSON = fmap ErrorMessage . parseJSON

newtype ErrorTarget = ErrorTarget {
  getErrorTarget :: Text
} deriving (Eq, FromJSONKey, Ord)

instance FromJSON ErrorTarget where
  parseJSON = fmap ErrorTarget . parseJSON

newtype Errors = Errors {
  errorsErrors :: Map ErrorTarget [ErrorCondition]
} deriving (Eq, Ord)

instance FromJSON Errors where
  parseJSON = withObject "Errors" $ \v -> Errors
    <$> v .: "errors"

renderErrors :: Errors -> [ErrorMessage]
renderErrors errors =
  let errorMap = errorsErrors errors
  in  Map.foldrWithKey (\k v es -> renderErrorList k v ++ es) [] errorMap

renderErrorList :: ErrorTarget -> [ErrorCondition] -> [ErrorMessage]
renderErrorList target conditions = renderError target <$> conditions

renderError :: ErrorTarget -> ErrorCondition -> ErrorMessage
renderError target condition =
  ErrorMessage $ (getErrorTarget target) <> " " <> (getErrorCondition condition)
