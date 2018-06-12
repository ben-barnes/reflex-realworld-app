{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Storage (
  retrieveToken
, storeToken
) where

import Conduit.Common.Data (Token(Token), getToken)
import Data.Text (Text)
import GHCJS.DOM (currentWindow)
import GHCJS.DOM.Types (MonadDOM)
import GHCJS.DOM.Storage (getItem, removeItem, setItem)
import GHCJS.DOM.Window (getLocalStorage)

tokenKey :: Text
tokenKey = "conduit-auth-token"

retrieveToken :: (MonadDOM m) => m (Maybe Token)
retrieveToken = do
  w <- currentWindow
  case w of
    Nothing -> return Nothing
    Just w' -> do
      localStorage <- getLocalStorage w'
      tokenText    <- getItem localStorage tokenKey
      return $ Token <$> tokenText

storeToken :: (MonadDOM m) => Maybe Token -> m ()
storeToken token = do
  w <- currentWindow
  case w of
    Nothing -> return ()
    Just w' -> do
      localStorage <- getLocalStorage w'
      case token of
        Nothing -> removeItem localStorage tokenKey
        Just t' -> setItem localStorage tokenKey (getToken t')
