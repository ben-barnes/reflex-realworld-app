{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Widgets (
  aAttr
, aDynAttr
, buttonClass
) where

import Data.Map (Map)
import Data.Text (Text)
import GHCJS.DOM.Element (Element(Element))
import GHCJS.DOM.EventM (on, preventDefault, stopPropagation)
import GHCJS.DOM.GlobalEventHandlers (click)
import GHCJS.DOM.HTMLAnchorElement (HTMLAnchorElement(HTMLAnchorElement))
import GHCJS.DOM.Types (uncheckedCastTo)
import Reflex.Dom (
    Dynamic
  , Event
  , EventName(Click)
  , MonadWidget
  , domEvent
  , _element_raw
  , elAttr'
  , elClass'
  , elDynAttr'
  , wrapDomEvent
  )

aAttr :: (MonadWidget t m) => Map Text Text -> m a -> m (Event t ())
aAttr attrs inner = do
  (e, _) <- elAttr' "a" attrs inner
  let aElement = uncheckedCastTo HTMLAnchorElement (_element_raw e)
  wrapDomEvent aElement (`on` click) $ do
    preventDefault
    stopPropagation

aDynAttr
  :: (MonadWidget t m)
  => Dynamic t (Map Text Text)
  -> m a
  -> m (Event t ())
aDynAttr attrs inner = do
  (e, _) <- elDynAttr' "a" attrs inner
  let aElement = uncheckedCastTo HTMLAnchorElement (_element_raw e)
  wrapDomEvent aElement (`on` click) $ do
    preventDefault
    stopPropagation

buttonClass :: (MonadWidget t m) => Text -> m () -> m (Event t ())
buttonClass cls inner = do
  (e, _) <- elClass' "button" cls inner
  return $ domEvent Click e

