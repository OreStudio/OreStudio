// ores-dsh-environment host half: two routes over the session's own work
// tree. One reads the environment through compass, and one starts an action.
// See CONTRACT.md for the frozen interface.

import { jobFor, readEnvironment, resolveWorktree, startAction } from './compass.js'
import { buildModel, failure } from './env.js'

export const name = 'ores-dsh-environment'

export const inject = ['webServer', 'sessions']

const STATE_ROUTE = '/plugins/ores-dsh-environment/state'
const ACTION_ROUTE = '/plugins/ores-dsh-environment/action'

/* One read is one compass process, about a second and a half, and a session
 * refetches on mount, on Refresh, and while an action runs. An entry expires
 * on the clock alone, so a hit older than the TTL is a miss. */
const CACHE_TTL_MS = 2000

/* The action body carries a session, a directory, an action name, a service
 * and a confirmation phrase. Anything larger is not this route's caller. */
const MAX_HTTP_BODY = 64 * 1024

export function apply(ctx) {
  const log = (message) => console.log('ores-dsh-environment: ' + message)

  const cache = new Map()
  /* The model the browser was last shown, kept past the read cache's expiry
   * because the restore guard compares against it. */
  const shown = new Map()

  const json = (res, value, status = 200) => {
    const body = JSON.stringify(value)
    res.writeHead(status, {
      'content-type': 'application/json; charset=utf-8',
      'cache-control': 'no-store',
      'content-length': Buffer.byteLength(body),
    })
    res.end(body)
  }

  const sweep = (now) => {
    for (const [key, entry] of cache) {
      if (now - entry.at >= CACHE_TTL_MS) cache.delete(key)
    }
  }

  /* The ladder: the session's own directory first, then the directory the
   * client sends, each accepted only when git resolves it to an ORE Studio
   * checkout. The server's working directory is never a rung, because
   * dsh-web.service starts from the user's home. */
  const resolveRoot = async (sessionId, cwd) => {
    const sessionCwd = sessionId ? ctx.sessions?.get(sessionId)?.header?.cwd : undefined
    const fromSession = await resolveWorktree(sessionCwd)
    if (fromSession) return { root: fromSession, source: 'session' }
    const fromCwd = await resolveWorktree(cwd)
    if (fromCwd) return { root: fromCwd, source: 'cwd' }
    return null
  }

  const describeMiss = (sessionId) => sessionId
    ? failure('unknown-session', `session ${sessionId} is not in a work tree of an ORE Studio checkout`)
    : failure('not-an-environment', 'no session and no cwd resolved to a work tree of an ORE Studio checkout')

  /* The harness fences `/api` with a Host rule and an authority-bound cookie;
   * a plugin route is registered outside that fence, so it applies the same
   * cookie check itself. Without it, a page that rebinds its own hostname to
   * loopback reaches this route as a same-origin caller, and this plugin can
   * stop a fleet. The browser already holds the cookie `dsh web` issued for
   * this authority and a same-origin fetch sends it, so the client is asked
   * for nothing extra. See dsh-client-connection's isAuthenticated. */
  const authenticated = (req) => {
    const auth = ctx.get('connection')?.browserAuth
    if (!auth || typeof auth.isAuthenticated !== 'function') return false
    return auth.isAuthenticated(req) === true
  }

  const unauthenticated = (res) => json(res, failure('unauthenticated',
    'this route needs the browser session that dsh web issued'), 401)

  const readModel = async (resolved, now) => {
    const read = await readEnvironment(resolved.root)
    if (!read.ok) return failure(read.reason, read.message)
    const model = buildModel(read.payload)
    cache.set(resolved.root, { at: Date.now(), model })
    shown.set(resolved.root, model)
    return model
  }

  /* The resolution rung is a property of the request, not of the read, so it
   * is attached here and never cached. A cached entry that carried the rung a
   * previous request took would report the wrong one. */
  const respond = (res, resolved, model, job, status) => json(res, {
    ...model,
    tree: { root: resolved.root, source: resolved.source },
    job,
  }, status)

  const stateHandler = async (req, res) => {
    if (!authenticated(req)) return unauthenticated(res)
    if (req.method !== 'GET' && req.method !== 'HEAD') {
      return json(res, failure('bad-request', 'Only GET and HEAD are supported'), 405)
    }
    try {
      const query = new URL(req.url, 'http://127.0.0.1').searchParams
      const sessionId = query.get('session') ?? ''
      const cwd = query.get('cwd') ?? ''
      const now = Date.now()
      sweep(now)

      const resolved = await resolveRoot(sessionId, cwd)
      if (!resolved) return json(res, describeMiss(sessionId))

      const hit = cache.get(resolved.root)
      const job = jobFor(resolved.root)
      /* While an action runs its own output is the news and the fleet state is
       * mid-change, so the last read is served rather than a fresh one. That
       * keeps a restore to one compass process instead of one every two
       * seconds. The cache entry is dropped when the action settles, so the
       * next read is fresh. */
      const model = (hit && job && job.running) || (hit && now - hit.at < CACHE_TTL_MS)
        ? hit.model
        : await readModel(resolved, now)

      return respond(res, resolved, model, job)
    } catch (error) {
      log('state route failed: ' + ((error && error.stack) || error))
      return json(res, failure('not-an-environment', String((error && error.message) || error)))
    }
  }

  const actionHandler = async (req, res) => {
    if (!authenticated(req)) return unauthenticated(res)
    if (req.method !== 'POST') {
      return json(res, failure('bad-request', 'Only POST is supported'), 405)
    }
    try {
      const contentType = String((req.headers && req.headers['content-type']) || '').toLowerCase()
      if (!contentType.startsWith('application/json')) {
        return json(res, failure('bad-request', 'Expected application/json'), 415)
      }
      const origin = req.headers && req.headers.origin
      const host = req.headers && req.headers.host
      if (origin && host) {
        let originHost = ''
        try { originHost = new URL(origin).host } catch { originHost = '' }
        if (originHost !== host) {
          return json(res, failure('bad-request', 'Cross-origin request denied'), 403)
        }
      }

      const chunks = []
      let size = 0
      for await (const chunk of req) {
        size += chunk.length
        if (size > MAX_HTTP_BODY) {
          return json(res, failure('bad-request', 'Request body too large'), 413)
        }
        chunks.push(chunk)
      }
      const raw = Buffer.concat(chunks).toString('utf8')
      const body = raw ? JSON.parse(raw) : {}

      const resolved = await resolveRoot(body.session, body.cwd)
      if (!resolved) return json(res, describeMiss(body.session))

      /* The guard for the one destructive action: the phrase must be the
       * environment name this browser was last shown. A caller cannot
       * rebuild a database it has not been shown. */
      const expected = shown.get(resolved.root)?.database?.confirmPhrase ?? ''
      const result = startAction(resolved.root, body, expected,
        () => cache.delete(resolved.root))
      if (!result.ok) return json(res, result, result.reason === 'busy' ? 409 : 400)
      return json(res, { ok: true, job: result.job })
    } catch (error) {
      const status = error instanceof SyntaxError ? 400 : 500
      return json(res, failure('bad-request', String((error && error.message) || error)), status)
    }
  }

  const route = { registered: false, dispose: null, timer: null, disposed: false }
  const register = () => {
    if (route.registered || route.disposed) return
    const webServer = ctx.get('webServer')
    if (!webServer) return
    try {
      const disposers = [
        webServer.register({ kind: 'exact', path: STATE_ROUTE, handler: stateHandler }),
        webServer.register({ kind: 'exact', path: ACTION_ROUTE, handler: actionHandler }),
      ]
      route.dispose = () => {
        for (const dispose of disposers) {
          if (typeof dispose === 'function') dispose()
        }
      }
      route.registered = true
      log(`${STATE_ROUTE} and ${ACTION_ROUTE} registered`)
    } catch (error) {
      log('route registration failed: ' + ((error && error.message) || error))
    }
  }

  ctx.effect(() => {
    register()
    if (!route.registered && ctx.get('timer')) {
      route.timer = ctx.get('timer').interval(() => {
        register()
        if (route.registered && route.timer) {
          route.timer()
          route.timer = null
        }
      }, 500)
    }
    return () => {
      route.disposed = true
      if (route.timer) route.timer()
      route.timer = null
      if (route.dispose) route.dispose()
      route.dispose = null
    }
  }, 'ores-dsh-environment: routes')
}
