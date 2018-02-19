module WebSocket.Ws
  (
    WebSocketServerOptions
  , WS
  , WebSocketServer
  , WebSocketConnection
  , WebSocketRequest
  , WebSocketMessage
  , createWebSocketServer
  , onConnection
  , onMessage
  , sendMessage
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.Newtype (class Newtype)
import Node.HTTP (Server)

-- | The type of a WebSocket server object
foreign import data WebSocketServer :: Type
foreign import data WebSocketConnection :: Type
foreign import data WebSocketRequest :: Type
newtype WebSocketMessage = WebSocketMessage String

derive newtype instance showWSM :: Show WebSocketMessage
derive instance newtypeWSM :: Newtype WebSocketMessage _

-- | The effect associated with using the WebSocket module
foreign import data WS :: Effect

type WebSocketServerOptions =
  { server :: Server }

foreign import createWebSocketServer :: forall e. WebSocketServerOptions -> Eff (ws :: WS | e) WebSocketServer

foreign import onConnection :: forall e. WebSocketServer -> (WebSocketConnection -> WebSocketRequest -> Eff (ws :: WS | e) Unit) -> Eff (ws :: WS | e) Unit
foreign import onMessage :: forall e. WebSocketConnection -> (WebSocketMessage -> Eff (ws :: WS | e) Unit) -> Eff (ws :: WS | e) Unit
foreign import sendMessage :: forall e. WebSocketConnection -> WebSocketMessage -> Eff (ws :: WS | e) Unit
