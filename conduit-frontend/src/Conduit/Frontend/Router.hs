{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Conduit.Frontend.Router (
  routeLoop
) where

import Data.Text (Text)
import GHCJS.DOM (currentWindow)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Location (getHash, setHash)
import GHCJS.DOM.Types (MonadDOM)
import GHCJS.DOM.Window (Window, getLocation)
import GHCJS.DOM.WindowEventHandlers (hashChange)
import Reflex.Dom (
    MonadWidget
  , Event
  , fanEither
  , getPostBuild
  , leftmost
  , performEvent_
  , wrapDomEvent
  )

routeLoop
  :: (MonadWidget t m)
  => a
  -> (a -> Text)
  -> (Text -> Either a a)
  -> Event t a
  -> m (Event t a)
routeLoop defaultRoute printRoute parseRoute internalRoute = do
  w <- currentWindow
  onLoad <- getPostBuild
  case w of
    Nothing -> return $ leftmost [const defaultRoute <$> onLoad, internalRoute]
    Just w' -> do
      windowHash <- getWindowHash w'
      let routeOnLoad = const (parseRoute windowHash) <$> onLoad
          (initRedir, initRoute) = fanEither routeOnLoad
      rec let allInternalRoutes = leftmost [initRedir, internalRoute, subsRedir]
          allHashChanges <- hashLoop w' (printRoute <$> allInternalRoutes)
          let (subsRedir, subsRoute) = fanEither $ parseRoute <$> allHashChanges
      return $ leftmost [initRoute, subsRoute]

hashLoop :: (MonadWidget t m) => Window -> Event t Text -> m (Event t Text)
hashLoop w internalHash = do
  performEvent_ $ setWindowHash w <$> internalHash
  wrapDomEvent w (`on` hashChange) (getWindowHash w)

getWindowHash :: (MonadDOM m) => Window -> m (Text)
getWindowHash w = getLocation w >>= getHash

setWindowHash :: (MonadDOM m) => Window -> Text -> m ()
setWindowHash w hash = getLocation w >>= (\l -> setHash l hash)
