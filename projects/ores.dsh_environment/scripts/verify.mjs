/**
 * ores-dsh-environment host-half gate, run against the real compass in this
 * work tree.
 *
 * The plugin is applied to a stub host that captures its routes, and the
 * captured handlers are then driven directly, so this exercises the whole host
 * path: the resolution ladder, the real `compass env status --json` payload,
 * the transform, the failure reasons, the action refusals, and a real action
 * job with its tail and exit code.
 *
 * The one action it runs is `services start` on a name compass does not know.
 * That spawns the real CLI, reaches its registry lookup, and exits 1 without
 * touching systemd, which is what makes the job lifecycle observable without
 * changing anything.
 *
 * Nothing here is destructive: no service is started or stopped, and the
 * database restore is only ever refused.
 *
 * Run: node projects/ores.dsh_environment/scripts/verify.mjs
 */

import { execFileSync } from 'node:child_process'
import { existsSync } from 'node:fs'
import { basename, dirname, join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

import { apply } from '../lib/index.js'

const SCRIPT_DIR = dirname(fileURLToPath(import.meta.url))
const REPO = resolve(SCRIPT_DIR, '..', '..', '..')

/* This gate reads the real environment through the real compass, so it needs a
 * configured checkout. CI has neither a .env nor a database, which is why the
 * workflow runs the hermetic checks and not this one. */
if (!existsSync(resolve(REPO, '.env'))) {
  console.error(`REFUSING TO CONTINUE: ${REPO} has no .env, so there is no environment to read.`)
  console.error('This is a local gate. Run it from a configured work tree.')
  process.exit(1)
}

const STATE_PATH = '/plugins/ores-dsh-environment/state'
const ACTION_PATH = '/plugins/ores-dsh-environment/action'
const LOGS_PATH = '/plugins/ores-dsh-environment/logs'

const results = []
let section = ''

function heading(title) {
  section = title
  console.log(`\n${title}`)
}

function check(name, ok, detail = '') {
  results.push({ section, name, ok, detail })
  console.log(`  ${ok ? 'PASS' : 'FAIL'}  ${name}${detail ? `  — ${detail}` : ''}`)
}

const round = (value) => Math.round(value * 100) / 100

/* ------------------------------------------------------------- the host */

const COOKIE = 'dsh-session=stub'

function makeHost(sessionCwd) {
  const routes = new Map()
  apply({
    effect: (fn) => fn(),
    get: (name) => {
      if (name === 'webServer') {
        return {
          register: ({ path, handler }) => {
            routes.set(path, handler)
            return () => {}
          },
        }
      }
      if (name === 'connection') {
        /* The harness's own browser-trust check, stubbed: the authority-bound
         * cookie is the whole of it here. */
        return {
          browserAuth: { isAuthenticated: (req) => req.headers.cookie === COOKIE },
        }
      }
      return undefined
    },
    sessions: {
      get: (id) => (id === 'known-session' ? { header: { cwd: sessionCwd } } : undefined),
    },
  })
  return routes
}

function makeReq({ method = 'GET', url = '/', headers = {}, body = null, cookie = COOKIE }) {
  const chunks = body === null ? [] : [Buffer.from(body)]
  return {
    method,
    url,
    headers: { host: '127.0.0.1:3199', ...(cookie === null ? {} : { cookie }), ...headers },
    async *[Symbol.asyncIterator]() {
      for (const chunk of chunks) yield chunk
    },
  }
}

function makeRes() {
  const res = { status: 0, headers: {}, body: '' }
  res.writeHead = (status, headers) => {
    res.status = status
    res.headers = headers ?? {}
  }
  res.end = (body) => { res.body = body ?? '' }
  res.json = () => JSON.parse(res.body)
  return res
}

async function call(handler, req) {
  const res = makeRes()
  await handler(req, res)
  return res
}

const getState = (routes, query) =>
  call(routes.get(STATE_PATH), makeReq({ url: `${STATE_PATH}?${query}` }))

const postAction = (routes, body) =>
  call(routes.get(ACTION_PATH), makeReq({
    method: 'POST',
    url: ACTION_PATH,
    headers: { 'content-type': 'application/json', host: '127.0.0.1:1' },
    body: JSON.stringify(body),
  }))

const delay = (ms) => new Promise((done) => setTimeout(done, ms))

async function waitForJob(routes, query, timeoutMs = 60000) {
  const deadline = Date.now() + timeoutMs
  let last = null
  while (Date.now() < deadline) {
    const res = await getState(routes, query)
    last = res.json()
    if (last && last.job && last.job.running === false) return last
    await delay(250)
  }
  return last
}

/* ------------------------------------------------------------- the gate */

const routes = makeHost(REPO)

heading('1. the plugin registers both routes')
check('state route registered', routes.has(STATE_PATH))
check('action route registered', routes.has(ACTION_PATH))
check('logs route registered', routes.has(LOGS_PATH))

heading('2. both routes need the browser session')
{
  const stateAnon = await call(routes.get(STATE_PATH),
    makeReq({ url: `${STATE_PATH}?cwd=${encodeURIComponent(REPO)}`, cookie: null }))
  check('an unauthenticated read is 401',
    stateAnon.status === 401 && stateAnon.json().reason === 'unauthenticated',
    `${stateAnon.status} ${stateAnon.json().reason}`)

  const actionAnon = await call(routes.get(ACTION_PATH), makeReq({
    method: 'POST',
    url: ACTION_PATH,
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ cwd: REPO, action: 'fleet-stop' }),
    cookie: null,
  }))
  check('an unauthenticated action is 401, so a rebound page cannot stop a fleet',
    actionAnon.status === 401 && actionAnon.json().reason === 'unauthenticated',
    `${actionAnon.status} ${actionAnon.json().reason}`)

  const wrongCookie = await call(routes.get(STATE_PATH),
    makeReq({ url: `${STATE_PATH}?cwd=${encodeURIComponent(REPO)}`, cookie: 'dsh-session=forged' }))
  check('a cookie this authority did not issue is 401', wrongCookie.status === 401,
    String(wrongCookie.status))
}

heading('3. the resolution ladder')
{
  const byCwd = await getState(routes, `cwd=${encodeURIComponent(REPO)}`)
  const body = byCwd.json()
  check('cwd resolves to this work tree', body.ok === true && body.tree?.root === REPO,
    body.ok ? `source=${body.tree.source}` : body.reason)
  check('the rung taken is reported', body.tree?.source === 'cwd', body.tree?.source)

  const bySession = await getState(routes, 'session=known-session')
  const sessionBody = bySession.json()
  check('the session directory wins over the query',
    sessionBody.ok === true && sessionBody.tree?.source === 'session',
    sessionBody.ok ? `source=${sessionBody.tree.source}` : sessionBody.reason)

  const nested = await getState(routes,
    `cwd=${encodeURIComponent(resolve(REPO, 'projects', 'ores.dsh_environment'))}`)
  check('a directory inside the work tree resolves to its root',
    nested.json().ok === true && nested.json().tree.root === REPO)

  const noRung = await getState(routes, '')
  check('no session and no cwd fails as not-an-environment',
    noRung.json().ok === false && noRung.json().reason === 'not-an-environment',
    noRung.json().reason)

  const unknown = await getState(routes, 'session=never-registered')
  check('an unknown session fails as unknown-session',
    unknown.json().ok === false && unknown.json().reason === 'unknown-session',
    unknown.json().reason)

  const outside = await getState(routes, `cwd=${encodeURIComponent('/tmp')}`)
  check('a directory outside a checkout does not resolve',
    outside.json().ok === false)
}

heading('4. the state contract against the real compass')
let payload = null
{
  const first = await getState(routes, `cwd=${encodeURIComponent(REPO)}`)
  payload = first.json()
  const second = await getState(routes, `cwd=${encodeURIComponent(REPO)}`)

  check('content-type is JSON', String(first.headers['content-type']).startsWith('application/json'),
    String(first.headers['content-type']))
  check('cache-control is no-store', first.headers['cache-control'] === 'no-store')
  check('ok is true', payload.ok === true, payload.reason ?? '')
  check('the payload carries no ANSI escape codes', !/\u001b\[/.test(first.body))
  check('env names the checkout', payload.env?.name === basename(REPO).replace(/^ores_dev_/, ''),
    payload.env?.name)
  check('env carries the preset from .env', typeof payload.env?.preset === 'string' && payload.env.preset.length > 0,
    payload.env?.preset)
  check('the database is reachable', payload.database?.reachable === true)
  check('the restore age is reported', Number.isFinite(payload.database?.restoredAgeSeconds),
    `${payload.database?.restoredAt} (${payload.database?.restoredAge})`)
  check('the restore carries a level from compass',
    ['ok', 'warn', 'critical', 'unknown'].includes(payload.database?.restoredLevel),
    payload.database?.restoredLevel)
  check('drift is reported with a level',
    typeof payload.database?.driftLabel === 'string' &&
    ['ok', 'warn', 'critical', 'unknown'].includes(payload.database?.driftLevel),
    `${payload.database?.driftLabel} (${payload.database?.driftLevel})`)
  check('the confirmation phrase is the database name',
    payload.database?.confirmPhrase === payload.database?.name,
    payload.database?.confirmPhrase)

  const ids = (payload.services?.states ?? []).map((state) => state.id)
  check('the state vocabulary arrives in compass order',
    ids.join(',') === 'running,starting,stopped,failed,missing', ids.join(','))
  check('every service unit is reported', (payload.services?.units ?? []).length === payload.services?.total,
    `${payload.services?.units?.length} of ${payload.services?.total}`)
  check('nats-server is reported apart from the fleet', payload.services?.nats !== null,
    payload.services?.nats?.state)
  check('the counts sum to the total',
    ids.reduce((sum, id) => sum + payload.services.counts[id], 0) === payload.services.total)

  check('three tiles are sent', (payload.tiles ?? []).map((tile) => tile.id).join(',') === 'database,services,config')
  check('every tile carries a tone',
    (payload.tiles ?? []).every((tile) => ['ok', 'warn', 'critical', 'unknown'].includes(tile.tone)),
    (payload.tiles ?? []).map((tile) => `${tile.id}=${tile.tone}`).join(' '))
  check('the health level is named with its reasons',
    ['ok', 'warn', 'critical', 'unknown'].includes(payload.health?.level) &&
    Array.isArray(payload.health?.reasons),
    `${payload.health?.level}: ${(payload.health?.reasons ?? []).length} reason(s)`)
  check('a job slot is always present', payload.job === null || typeof payload.job === 'object')

  /* The selector is what a row's Start or Stop posts, and compass resolves a
   * registry service rather than one replica of it, so a per-replica label
   * such as compute.wrapper-1 would fail its registry lookup. */
  const selectors = [...new Set((payload.services?.units ?? []).map((unit) => unit.selector))]
  check('every unit carries a registry service as its selector',
    selectors.length > 0 && selectors.every((selector) => selector.startsWith('ores.')),
    `${selectors.length} distinct`)
  check('a replicated service shares one selector across its rows',
    (payload.services?.units ?? []).filter((unit) => unit.selector === 'ores.compute.wrapper').length > 1)

  /* The restore age is a live difference against the clock, so two reads a
   * moment apart legitimately differ by a second. Everything else must hold. */
  const comparable = (body) => JSON.stringify({
    ...body,
    generatedAt: null,
    database: { ...body.database, restoredAgeSeconds: null },
  })
  check('two reads of the same work tree agree',
    comparable(second.json()) === comparable(payload))
  console.log(`  note  state payload ${Buffer.byteLength(first.body)} bytes, read ${first.status}`)
}

heading('5. the read route refuses what it should')
{
  const wrongMethod = await call(routes.get(STATE_PATH), makeReq({ method: 'POST', url: STATE_PATH }))
  check('a POST to the read route is 405', wrongMethod.status === 405, String(wrongMethod.status))
}

heading('6. the action route refuses what it should')
{
  const wrongType = await call(routes.get(ACTION_PATH), makeReq({
    method: 'POST', url: ACTION_PATH, headers: { 'content-type': 'text/plain' }, body: '{}',
  }))
  check('a non-JSON body is 415', wrongType.status === 415, String(wrongType.status))

  const crossOrigin = await call(routes.get(ACTION_PATH), makeReq({
    method: 'POST',
    url: ACTION_PATH,
    headers: { 'content-type': 'application/json', host: '127.0.0.1:1', origin: 'http://evil.invalid' },
    body: JSON.stringify({ cwd: REPO, action: 'fleet-stop' }),
  }))
  check('a cross-origin action is 403', crossOrigin.status === 403, String(crossOrigin.status))

  const unknown = await postAction(routes, { cwd: REPO, action: 'drop-everything' })
  check('an unknown action is refused',
    unknown.json().ok === false && unknown.json().reason === 'unknown-action', unknown.json().reason)

  for (const service of ['', 'web; rm -rf /', '../../etc']) {
    const bad = await postAction(routes, { cwd: REPO, action: 'service-stop', service })
    check(`a selector of ${JSON.stringify(service)} is refused`,
      bad.json().ok === false && bad.json().reason === 'bad-service', bad.json().reason)
  }

  const unconfirmed = await postAction(routes,
    { cwd: REPO, action: 'database-restore', stopServices: true })
  check('a rebuild without the typed name is refused',
    unconfirmed.json().ok === false && unconfirmed.json().reason === 'not-confirmed',
    unconfirmed.json().reason)

  const wrongName = await postAction(routes,
    { cwd: REPO, action: 'database-restore', confirm: 'some_other_environment' })
  check('a rebuild naming another environment is refused',
    wrongName.json().ok === false && wrongName.json().reason === 'not-confirmed',
    wrongName.json().reason)
}

heading('7. a real action job')
{
  const started = await postAction(routes, { cwd: REPO, action: 'service-start', service: 'nope' })
  const job = started.json()
  check('the action is accepted', job.ok === true && typeof job.job?.id === 'string', job.reason ?? '')
  check('the job names what it runs', job.job?.label === 'Start nope', job.job?.label)

  const busy = await postAction(routes, { cwd: REPO, action: 'service-start', service: 'web' })
  check('a second action in the same work tree is 409 busy',
    busy.status === 409 && busy.json().reason === 'busy', `${busy.status} ${busy.json().reason}`)

  /* A read during the action is served from the last read, so a restore costs
   * one compass process and not one every two seconds. The action takes about
   * two seconds, which is the window this measures in. */
  const started0 = performance.now()
  const during = await getState(routes, `cwd=${encodeURIComponent(REPO)}`)
  const duringMs = performance.now() - started0
  check('a read while the action runs does not spawn compass',
    during.json().ok === true && during.json().job?.running === true && duringMs < 1000,
    `${round(duringMs)}ms`)

  const settled = await waitForJob(routes, `cwd=${encodeURIComponent(REPO)}`)
  check('the job finishes with a non-zero code', settled.job?.running === false && settled.job?.code !== 0,
    `code=${settled.job?.code}`)
  check('the job reports its one step',
    settled.job?.steps?.length === 1 && settled.job.steps[0].code !== 0,
    (settled.job?.steps ?? []).map((step) => `${step.name}=${step.code}`).join(' '))
  check('the tail carries the command line and the failure',
    (settled.job?.tail ?? []).some((line) => line.includes('compass services start nope')) &&
    (settled.job?.tail ?? []).some((line) => line.includes('no service')),
    `${(settled.job?.tail ?? []).length} line(s)`)
  check('a finished job reports its elapsed time', settled.job?.elapsedSeconds >= 0)
  console.log(`  note  job tail:\n        ${(settled.job?.tail ?? []).join('\n        ')}`)
}

heading('8. a real journal read for one unit')
{
  const unit = payload.services?.units?.[0]?.unit ?? ''
  const anon = await call(routes.get(LOGS_PATH),
    makeReq({ url: `${LOGS_PATH}?cwd=${encodeURIComponent(REPO)}&unit=${unit}`, cookie: null }))
  check('an unauthenticated log read is 401', anon.status === 401, String(anon.status))

  const read = await call(routes.get(LOGS_PATH),
    makeReq({ url: `${LOGS_PATH}?cwd=${encodeURIComponent(REPO)}&unit=${encodeURIComponent(unit)}&level=all` }))
  const body = read.json()
  check('the unit\'s journal tail comes back', body.ok === true && Array.isArray(body.lines),
    body.ok ? `${body.count} line(s), truncated=${body.truncated}` : `${body.reason}: ${body.message}`)

  const nats = await call(routes.get(LOGS_PATH),
    makeReq({ url: `${LOGS_PATH}?cwd=${encodeURIComponent(REPO)}&unit=nats-server-${payload.env?.name ?? ''}` }))
  check('nats-server is reachable through the same route',
    nats.json().ok === true || nats.json().reason === 'compass-failed', nats.json().reason ?? 'ok')

  const bad = await call(routes.get(LOGS_PATH),
    makeReq({ url: `${LOGS_PATH}?cwd=${encodeURIComponent(REPO)}&unit=${encodeURIComponent('nope; rm -rf /')}` }))
  check('a unit name that is not one is refused', bad.status === 400, String(bad.status))
}

heading('9. the selector the view posts is one compass accepts')
{
  const selector = payload.services?.units?.[0]?.selector ?? ''
  check('the row offers a selector at all', selector.startsWith('ores.'), selector || '(empty)')
  let code = 0
  try {
    execFileSync('bash', [join(REPO, 'compass.sh'), 'services', 'status', selector],
      { cwd: REPO, stdio: 'pipe' })
  } catch (error) {
    code = typeof error?.status === 'number' ? error.status : 1
  }
  check(`compass services status ${selector} exits 0`, code === 0, `exit ${code}`)
}

/* ------------------------------------------------------------- report */

const failed = results.filter((result) => !result.ok)
console.log(`\n${results.length - failed.length} passed, ${failed.length} failed`)
if (failed.length > 0) {
  for (const result of failed) console.error(`FAIL  ${result.section} / ${result.name}  ${result.detail}`)
  process.exitCode = 1
} else {
  console.log('OK ores-dsh-environment: routes, ladder, contract, refusals, and a real action job')
}
