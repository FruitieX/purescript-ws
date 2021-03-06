module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP, Request, Response, createServer, responseAsStream, setStatusCode, listen)
import Node.Stream (end)
import WebSocket.Ws (WS, WebSocketConnection, WebSocketMessage(WebSocketMessage), close, createWebSocketServerWithServer, onConnection, onError, onMessage, onServerError, sendMessage)

handleMessage :: forall e. WebSocketConnection -> WebSocketMessage -> Eff (ws :: WS, console :: CONSOLE | e) Unit
handleMessage ws msg = do
  log $ show msg
  close ws

handleError :: forall e. Error -> Eff (ws :: WS, console :: CONSOLE | e) Unit
handleError err = do
  log $ show err

handleConnection :: forall e. WebSocketConnection -> Request -> Eff (ws :: WS, console :: CONSOLE | e) Unit
handleConnection ws req = do
  log "Connected!"
  onMessage ws $ handleMessage ws
  onError ws handleError
  sendMessage ws $ WebSocketMessage "Hello, world!"

-- this is some extra crap you need for Node.HTTP createServer
respond :: forall e. Request -> Response -> Eff (http :: HTTP | e) Unit
respond req res = do
  setStatusCode res 200
  let outputStream = responseAsStream res
  end outputStream (pure unit)

main
  :: forall e
   . Eff
     (ws :: WS , console :: CONSOLE, http :: HTTP | e) Unit
main = do
  server <- createServer respond
  wss <- createWebSocketServerWithServer server {}
  onConnection wss handleConnection
  onServerError wss handleError
  listen server { hostname: "localhost", port: 8080, backlog: Nothing } $ void do
    log "Listening on port 8080."
