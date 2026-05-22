;(function () {
  'use strict'

  var EMACS_WS_URL = 'ws://localhost:35903'
  var DATA_URL = '/OreStudio/graph/graphdata.json'

  function FakeWebSocket(url) {
    var self = this
    self.url = url
    self.readyState = 0 // CONNECTING
    self._listeners = {}

    function emit(type, event) {
      var handlers = self._listeners[type] || []
      handlers.forEach(function (h) { h(event) })
      if (typeof self['on' + type] === 'function') {
        self['on' + type](event)
      }
    }

    setTimeout(function () {
      self.readyState = 1 // OPEN
      emit('open', { type: 'open' })

      fetch(DATA_URL)
        .then(function (r) { return r.json() })
        .then(function (data) {
          var evt = { type: 'message', data: JSON.stringify({ type: 'graphdata', data: data }) }
          emit('message', evt)
        })
        .catch(function (err) {
          console.error('[ws-shim] failed to load ' + DATA_URL, err)
        })
    }, 0)
  }

  FakeWebSocket.prototype.addEventListener = function (type, handler) {
    if (!this._listeners[type]) this._listeners[type] = []
    this._listeners[type].push(handler)
  }

  FakeWebSocket.prototype.removeEventListener = function (type, handler) {
    if (!this._listeners[type]) return
    this._listeners[type] = this._listeners[type].filter(function (h) { return h !== handler })
  }

  FakeWebSocket.prototype.send = function () {}
  FakeWebSocket.prototype.close = function () { this.readyState = 3 }

  FakeWebSocket.CONNECTING = 0
  FakeWebSocket.OPEN = 1
  FakeWebSocket.CLOSING = 2
  FakeWebSocket.CLOSED = 3

  var NativeWebSocket = window.WebSocket

  function PatchedWebSocket(url, protocols) {
    if (url === EMACS_WS_URL) {
      console.info('[ws-shim] static mode: serving graph from ' + DATA_URL)
      return new FakeWebSocket(url)
    }
    return protocols ? new NativeWebSocket(url, protocols) : new NativeWebSocket(url)
  }

  PatchedWebSocket.prototype = NativeWebSocket.prototype
  PatchedWebSocket.CONNECTING = 0
  PatchedWebSocket.OPEN = 1
  PatchedWebSocket.CLOSING = 2
  PatchedWebSocket.CLOSED = 3

  window.WebSocket = PatchedWebSocket
})()
