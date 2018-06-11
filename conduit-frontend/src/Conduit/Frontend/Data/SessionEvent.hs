module Conduit.Frontend.Data.SessionEvent (
  SessionEvent(..)
, chooseAuthEvent
, chooseRouteEvent
) where

import Conduit.Frontend.API (AuthProfile)
import Conduit.Frontend.Routes (Route)

data SessionEvent
  = SessionAuthEvent AuthProfile
  | SessionRouteEvent Route
    deriving (Eq, Ord)

chooseAuthEvent :: SessionEvent -> Maybe AuthProfile
chooseAuthEvent (SessionAuthEvent p) = Just p
chooseAuthEvent _                    = Nothing

chooseRouteEvent :: SessionEvent -> Maybe Route
chooseRouteEvent (SessionRouteEvent r) = Just r
chooseRouteEvent _                     = Nothing
