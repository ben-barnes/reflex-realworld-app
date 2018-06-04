{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Components (
  aAttr
, aDynAttr
, currentHash
, hashChangeEvent
) where

import Data.Map (Map)
import Data.Text (Text)
import GHCJS.DOM (currentWindow)
import GHCJS.DOM.Element (Element(Element))
import GHCJS.DOM.EventM (on, preventDefault)
import GHCJS.DOM.GlobalEventHandlers (click)
import GHCJS.DOM.HTMLAnchorElement (HTMLAnchorElement(HTMLAnchorElement))
import GHCJS.DOM.Location (getHash)
import GHCJS.DOM.Types (MonadDOM, uncheckedCastTo)
import GHCJS.DOM.Window (getLocation)
import GHCJS.DOM.WindowEventHandlers (hashChange)
import Reflex.Dom (
    Dynamic
  , Event
  , MonadWidget
  , _element_raw
  , elAttr'
  , elDynAttr'
  , never
  , wrapDomEvent
  )

aAttr :: (MonadWidget t m) => Map Text Text -> m a -> m (Event t ())
aAttr attrs inner = do
  (e, _) <- elAttr' "a" attrs inner
  let aElement = uncheckedCastTo HTMLAnchorElement (_element_raw e)
  wrapDomEvent aElement (`on` click) preventDefault

aDynAttr
  :: (MonadWidget t m)
  => Dynamic t (Map Text Text)
  -> m a
  -> m (Event t ())
aDynAttr attrs inner = do
  (e, _) <- elDynAttr' "a" attrs inner
  let aElement = uncheckedCastTo HTMLAnchorElement (_element_raw e)
  wrapDomEvent aElement (`on` click) preventDefault

currentHash :: (MonadDOM m) => m (Maybe Text)
currentHash = do
  w <- currentWindow
  case w of
    Nothing -> return Nothing
    Just w' -> Just <$> (getLocation w' >>= getHash)

hashChangeEvent :: (MonadWidget t m) => m (Event t Text)
hashChangeEvent = do
  w <- currentWindow
  case w of
    Nothing -> return never
    Just w' -> wrapDomEvent w' (`on` hashChange) (getLocation w' >>= getHash)
