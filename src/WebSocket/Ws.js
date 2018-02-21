"use strict";

const WebSocket = require('ws');

/* Server methods */

exports.createWebSocketServer_ = function (options) {
  return function (callback) {
    return function () {
      var wss = new WebSocket.Server(options, callback);
      return wss;
    };
  }
}

exports.onConnection_ = function(wss) {
  return function (handleConnection) {
    return function() {
      wss.on('connection', function(ws, req) {
        handleConnection(ws)(req)();
      });
    }
  }
}

exports.onServerError_ = function(wss) {
  return function (handleError) {
    return function() {
      wss.on('error', function(error) {
        handleError(error)();
      });
    }
  }
}

/* WebSocket methods */

exports.onMessage_ = function(ws) {
  return function (handleMessage) {
    return function() {
      ws.on('message', function(message) {
        handleMessage(message)();
      });
    }
  }
}

exports.sendMessage_ = function(ws) {
  return function (message) {
    return function() {
      ws.send(message);
    }
  }
}

exports.onError_ = function(ws) {
  return function (handleError) {
    return function() {
      ws.on('error', function(error) {
        handleError(error)();
      });
    }
  }
}
