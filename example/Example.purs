module Example where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Node.HTTP (HTTP, Request, Response, createServer, listen, responseAsStream, setStatusCode)
import Node.Stream (end)
import WebSocket.Ws (WS, WebSocketConnection, WebSocketMessage, WebSocketRequest, createWebSocketServer, onConnection, onMessage, sendMessage)

handleMessage :: forall e. WebSocketMessage -> Eff (ws :: WS, console :: CONSOLE | e) Unit
handleMessage msg = do
  log $ show msg

handleConnection :: forall e. WebSocketConnection -> WebSocketRequest -> Eff (ws :: WS, console :: CONSOLE | e) Unit
handleConnection ws req = do
  log "Connected!"
  onMessage ws handleMessage
  sendMessage ws $ wrap "Hello, world!"

-- | this is some extra crap you need for Node.HTTP createServer
respond :: forall e. Request -> Response -> Eff (http :: HTTP | e) Unit
respond req res = do
  setStatusCode res 200
  let outputStream = responseAsStream res
  end outputStream (pure unit)

main :: forall e. Eff (ws :: WS, http :: HTTP, console :: CONSOLE | e) Unit
main = do
  server <- createServer respond
  wss <- createWebSocketServer { server: server }
  ws <- onConnection wss handleConnection
  listen server { hostname: "localhost", port: 8080, backlog: Nothing } $ void do
    log "Listening on port 8080."
