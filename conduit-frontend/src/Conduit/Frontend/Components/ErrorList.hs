{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Components.ErrorList (
  errorList
) where

import Conduit.Frontend.API.Errors (Errors, getErrorMessage, renderErrors)
import Data.Foldable (traverse_)
import Reflex.Dom (MonadWidget, el, elClass, text)

errorList :: (MonadWidget t m) => Errors -> m ()
errorList es =
  let renderError e = el "li" $ text (getErrorMessage e)
  in  elClass "ul" "error-messages" $
        traverse_ renderError (renderErrors es)
