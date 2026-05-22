;(function () {
  'use strict'

  var EMACS_WS_URL = 'ws://localhost:35903'
  var NODE_API_BASE = 'http://localhost:35901/node/'
  var DATA_URL = '/OreStudio/graph/graphdata.json'

  // id -> node object, populated when graphdata loads
  var nodeById = {}

  function buildNodeIndex(data) {
    var nodes = data.nodes || []
    for (var i = 0; i < nodes.length; i++) {
      var n = nodes[i]
      if (n && n.id) nodeById[n.id] = n
    }
  }

  // Intercept fetch() calls to localhost:35901/node/ — fetch the published HTML
  // page, extract #content, and return it wrapped in a #+begin_export html block
  // so uniorg renders it as real HTML rather than escaped markup.
  var NativeFetch = window.fetch
  window.fetch = function (url, opts) {
    var urlStr = typeof url === 'string' ? url : (url && typeof url.url === 'string' ? url.url : '')
    if (urlStr.indexOf(NODE_API_BASE) === 0) {
      var encoded = urlStr.slice(NODE_API_BASE.length).split('?')[0]
      var id
      try { id = decodeURIComponent(decodeURIComponent(encoded)) } catch (e) { id = encoded }
      var node = nodeById[id]
      if (!node || !node.file) {
        return Promise.resolve(new Response('', { status: 200,
          headers: { 'Content-Type': 'text/plain; charset=utf-8' } }))
      }
      var body = (node.content && node.content !== true) ? node.content : ''
      return Promise.resolve(new Response(body, { status: 200,
        headers: { 'Content-Type': 'text/plain; charset=utf-8' } }))
    }
    return NativeFetch.apply(this, arguments)
  }

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
          buildNodeIndex(data)
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
