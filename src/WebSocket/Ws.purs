module WebSocket.Ws where
import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1)
import Data.Newtype (class Newtype)
import Data.Record (insert)
import Data.Symbol (SProxy(..))
import Node.HTTP (Request, Server)
import Type.Row (class RowLacks)

-- | The type of a WebSocket server object
foreign import data WebSocketServer :: Type
foreign import data WebSocketConnection :: Type

newtype WebSocketMessage = WebSocketMessage String
derive newtype instance showWSM :: Show WebSocketMessage
derive instance newtypeWSM :: Newtype WebSocketMessage _

-- | The effect associated with using the WebSocket module
foreign import data WS :: Effect

-- TODO: more options from:
-- https://github.com/websockets/ws/blob/master/doc/ws.md
type WebSocketServerOptions =
  ( host :: String
  , backlog :: Int
  )

-- | The port to listen on if calling createWebSocketServerWithPort
newtype Port = Port Int

foreign import createWebSocketServer_
  :: forall e options
   . options
  -> (EffFn1 (ws :: WS | e) Unit Unit)
  -> Eff (ws :: WS | e) WebSocketServer

-- | Creates a WebSocket.Server and internally a HTTP server
-- | which binds to a given port
-- |
-- | The supplied callback is called when the created HTTP server
-- | starts listening.
createWebSocketServerWithPort
  :: forall e options options' trash
   . Union options options' WebSocketServerOptions
  => RowLacks "port" options
  => RowCons "port" Port options trash
  => Port
  -> { | options }
  -> (Unit -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) WebSocketServer
createWebSocketServerWithPort (Port port) options callback =
  createWebSocketServer_ options' callback'
    where
      options' = insert (SProxy :: SProxy "port") port options
      callback' = mkEffFn1 callback

-- | Creates a WebSocket.Server from a pre-existing Node.Server
createWebSocketServerWithServer
  :: forall e options options' trash
   . Union options options' WebSocketServerOptions
  => RowLacks "server" options
  => RowCons "server" Server options trash
  => Port
  -> { | options }
  -> Eff (ws :: WS | e) WebSocketServer
createWebSocketServerWithServer server options =
  createWebSocketServer_ options' callback'
    where
      options' = insert (SProxy :: SProxy "server") server options
      callback' = mkEffFn1 $ const (pure unit)

foreign import onConnection_
  :: forall e
   . WebSocketServer
  -> (WebSocketConnection -> Request -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit

-- | Attaches a connection event handler to a WebSocketServer
onConnection
  :: forall e
   . WebSocketServer
  -> (WebSocketConnection -> Request -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit
onConnection server callback =
  onConnection_ server callback

foreign import onServerError_
  :: forall e
   . WebSocketServer
  -> (Error -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit

-- | Attaches an error event handler to a WebSocketServer
onServerError
  :: forall e
   . WebSocketServer
  -> (Error -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit
onServerError server callback =
  onServerError_ server callback

foreign import onMessage_
  :: forall e
   . WebSocketConnection
  -> (WebSocketMessage -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit

-- | Attaches a message event handler to a WebSocketConnection
onMessage
  :: forall e
   . WebSocketConnection
  -> (WebSocketMessage -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit
onMessage ws callback =
  onMessage_ ws callback

foreign import sendMessage_
  :: forall e
   . WebSocketConnection
  -> WebSocketMessage
  -> Eff (ws :: WS | e) Unit

-- | Send a message over a WebSocketConnection
sendMessage
  :: forall e
   . WebSocketConnection
  -> WebSocketMessage
  -> Eff (ws :: WS | e) Unit
sendMessage ws message =
  sendMessage_ ws message

foreign import onError_
  :: forall e
   . WebSocketConnection
  -> (Error -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit

-- | Attaches an error event handler to a WebSocketConnection
onError
  :: forall e
   . WebSocketConnection
  -> (Error -> Eff (ws :: WS | e) Unit)
  -> Eff (ws :: WS | e) Unit
onError ws callback =
  onError_ ws callback
