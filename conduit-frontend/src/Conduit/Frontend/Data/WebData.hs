{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Data.WebData (
  ApiData
, ApiError(..)
, WebData(..)
, apiRequest
, toMaybe
) where

import Control.Monad (ap, liftM)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Text (Text)
import Data.Bifunctor (Bifunctor, first, second)
import Reflex.Dom (
    Dynamic
  , Event
  , IsXhrPayload
  , MonadWidget
  , XhrException
  , XhrRequest
  , XhrResponse
  , _xhrResponse_responseText
  , holdDyn
  , performRequestAsyncWithError
  )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data ApiError
  = ApiDecodingError Text
  | ApiXhrError XhrException

data WebData e a
  = Loading
  | Failure e
  | Success a

type ApiData = WebData ApiError

instance Bifunctor WebData where
  first  _ Loading     = Loading
  first  f (Failure e) = Failure (f e)
  first  _ (Success a) = Success a
  second _ Loading     = Loading
  second _ (Failure e) = Failure e
  second f (Success a) = Success (f a)

instance Functor (WebData e) where
  fmap = liftM

instance Applicative (WebData e) where
  pure  = return
  (<*>) = ap

instance Monad (WebData e) where
  return = Success
  Loading     >>= _ = Loading
  (Failure e) >>= _ = Failure e
  (Success a) >>= f = f a

apiRequest
  :: (FromJSON a, IsXhrPayload p, MonadWidget t m)
  => Event t (XhrRequest p)
  -> m (Dynamic t (WebData ApiError a))
apiRequest request = do
  res <- performRequestAsyncWithError request
  holdDyn Loading $ fromResponse <$> res

fromResponse
  :: (FromJSON a)
  => Either XhrException XhrResponse
  -> WebData ApiError a
fromResponse r = first ApiXhrError (fromEither r) >>= jsonWebData

fromEither :: Either e a -> WebData e a
fromEither (Left e)  = Failure e
fromEither (Right a) = Success a

fromMaybe :: e -> Maybe a -> WebData e a
fromMaybe e Nothing  = Failure e
fromMaybe _ (Just a) = Success a

toMaybe :: WebData e a -> Maybe a
toMaybe (Success a) = Just a
toMaybe _           = Nothing

jsonWebData :: (FromJSON a) => XhrResponse -> WebData ApiError a
jsonWebData res = do
  let bodyNotPresent = ApiDecodingError "Body not present."
  bodyText <- fromMaybe bodyNotPresent . _xhrResponse_responseText $ res
  let bodyBL = BL.fromStrict . Text.encodeUtf8 $ bodyText
  first (ApiDecodingError . Text.pack) . fromEither . eitherDecode $ bodyBL
