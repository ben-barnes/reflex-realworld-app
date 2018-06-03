{-# LANGUAGE OverloadedStrings #-}

module Conduit.Frontend.Components (
  aAttr
, aDynAttr
) where

import Data.Map (Map)
import Data.Text (Text)
import GHCJS.DOM.Element (Element(Element))
import GHCJS.DOM.EventM (on, preventDefault)
import GHCJS.DOM.GlobalEventHandlers (click)
import GHCJS.DOM.HTMLAnchorElement (HTMLAnchorElement(HTMLAnchorElement))
import GHCJS.DOM.Types (uncheckedCastTo)
import Reflex.Dom (
    Dynamic
  , Event
  , MonadWidget
  , _element_raw
  , elAttr'
  , elDynAttr'
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
