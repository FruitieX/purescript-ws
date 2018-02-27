# purescript-ws

Low-level PureScript bindings for 'ws' library. Currently only contains
WebSocket server functionality.

Sample usage in [example/](/example/) directory.

NOTE: You **MUST** attach an error handler for each `WebSocketConnection` by
using `onError`, otherwise your program will crash and burn on any error that
occurs with the `WebSocketConnection` (such as ECONNRESET from a client that
abruptly closed its end of the connection). This behavior is caused by the `ws`
library.

## Setup

1. Install `ws` from npm.
2. Have a look at the documentation/example

## TODO

- Force user to supply `WebSocketConnection` event handlers in
  `createWebSocketServer*` functions
- Implement WebSocketClient bindings to make this usable also as a client library
