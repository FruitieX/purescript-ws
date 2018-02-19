"use strict";

const WebSocket = require('ws');

exports.createWebSocketServer = function (options) {
  return function () {
    return new WebSocket.Server(options);
  };
}

exports.onConnection = function(wss) {
  return function (handleConnection) {
    return function() {
      wss.on('connection', function(ws, req) {
        handleConnection(ws)(req)();
      });
    }
  }
}

exports.onMessage = function(ws) {
  return function (handleMessage) {
    return function() {
      ws.on('message', function(message) {
        handleMessage(message)();
      });
    }
  }
}

exports.sendMessage = function(ws) {
  return function (message) {
    return function() {
      ws.send(message);
    }
  }
}
