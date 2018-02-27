module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Node.HTTP (Request)
import WebSocket.Ws (Port(Port), WS, WebSocketConnection, WebSocketMessage(WebSocketMessage), close, createWebSocketServerWithPort, onConnection, onError, onMessage, onServerError, sendMessage)

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

main
  :: forall e
   . Eff
     (ws :: WS , console :: CONSOLE | e) Unit
main = do
  wss <- createWebSocketServerWithPort (Port 8080) {} $ const do
    log "Listening on port 8080."
  onConnection wss handleConnection
  onServerError wss handleError
