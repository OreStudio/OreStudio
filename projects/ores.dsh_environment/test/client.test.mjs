/**
 * Client-half tests: the bundle DSH serves verbatim.
 *
 * There is no build step and no compiler between this file and the browser, so
 * a mistake in it is invisible until the panel fails to appear. These tests
 * load the bundle the way the module loader does, run the factory, apply the
 * plugin to a stub slot registry, and render each seat against a real payload.
 *
 * Rendering needs hooks that hold state and effects that run, so the stub React
 * below is stateful and effects are flushed by hand. That is what lets the view
 * be rendered at all: with a no-op useState it could only ever render its
 * loading line, which is how a crash on the first successful read went unseen.
 *
 * Run from the plugin directory: node --test
 */

import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

import { buildModel } from '../lib/env.js'
import { COMPASS_PAYLOAD } from './fixture.mjs'

const here = dirname(fileURLToPath(import.meta.url))
const bundle = resolve(here, '..', 'lib', 'client.js')
const BUNDLE_SOURCE = readFileSync(bundle, 'utf8')

/* The stylesheet seat touches the DOM, so the runtime needs the two calls it
 * makes and nothing more. */
globalThis.document = {
  createElement: () => ({ setAttribute() {}, textContent: '' }),
  head: { appendChild() {} },
  querySelector: () => null,
  addEventListener() {},
  removeEventListener() {},
}

/* --------------------------------------------------------------- a React */

/**
 * Enough React to render one component tree with working state and effects.
 *
 * Hooks are indexed by call order, as React indexes them, so a second render
 * of the same component reads the same slots. `flushEffects` runs what the last
 * render registered and returns their cleanups.
 */
function makeReact() {
  const state = { slots: [], cursor: 0, pending: [] }

  const createElement = (type, props, ...children) => ({
    type,
    props: props ?? {},
    children: children.flat(Infinity).filter((child) => child !== null && child !== undefined),
  })

  return {
    react: {
      createElement,
      createContext: (value) => {
        const context = { __value: value }
        context.Provider = (props) => props.children
        return context
      },
      useContext: (context) => (context && '__value' in context ? context.__value : ''),
      useState: (initial) => {
        const index = state.cursor++
        if (!(index in state.slots)) {
          state.slots[index] = typeof initial === 'function' ? initial() : initial
        }
        return [state.slots[index], (value) => {
          state.slots[index] = typeof value === 'function' ? value(state.slots[index]) : value
        }]
      },
      useEffect: (fn) => { state.cursor += 1; state.pending.push(fn) },
      useRef: (initial) => {
        const index = state.cursor++
        if (!(index in state.slots)) state.slots[index] = { current: initial }
        return state.slots[index]
      },
      useCallback: (fn) => { state.cursor += 1; return fn },
    },
    begin() { state.cursor = 0; state.pending = [] },
    flushEffects() {
      const running = state.pending
      state.pending = []
      const cleanups = []
      for (const fn of running) {
        const cleanup = fn()
        if (typeof cleanup === 'function') cleanups.push(cleanup)
      }
      return cleanups
    },
  }
}

/** Load the bundle the way the harness's module loader does. */
async function loadBundle() {
  let captured = null
  globalThis.window = {
    __ModuleLoader__: {
      load: (definition) => { captured = definition },
    },
  }
  await import(`${bundle}?loaded=${Math.random()}`)
  assert.ok(captured, 'the bundle never called window.__ModuleLoader__.load')
  return captured
}

/**
 * Invoke every function component in a tree, depth first, so each one's hooks
 * run in render order and land in the runtime's slots. This is the part of a
 * reconciler these tests need, and nothing more.
 */
function expand(node, runtime) {
  if (Array.isArray(node)) return node.flatMap((child) => expand(child, runtime))
  if (!node || typeof node !== 'object') return [node]
  if (typeof node.type === 'function') {
    return expand(node.type({ ...node.props, children: node.children }), runtime)
  }
  return [{
    ...node,
    children: (node.children ?? []).flatMap((child) => expand(child, runtime))
      .filter((child) => child !== null && child !== undefined),
  }]
}

/**
 * Render a component with the stub runtime, running its effects and re-rendering
 * until it settles. What comes back is the expanded tree, so the readers below
 * only walk hosts.
 */
async function render(Component, props, payload, runtime, passes = 5) {
  const cleanups = []
  let tree = null
  globalThis.fetch = async () => ({ json: async () => payload })
  for (let pass = 0; pass < passes; pass += 1) {
    runtime.begin()
    tree = expand([Component(props)], runtime)
    cleanups.push(...runtime.flushEffects())
    await new Promise((done) => setTimeout(done, 2))
  }
  for (const cleanup of cleanups) {
    if (typeof cleanup === 'function') cleanup()
  }
  return tree
}

/** Every string in a rendered tree, joined. */
function texts(tree) {
  const collect = (node) => {
    if (Array.isArray(node)) return node.map(collect).join(' ')
    if (typeof node === 'string') return node
    if (typeof node === 'number') return String(node)
    if (!node || typeof node !== 'object') return ''
    return collect(node.children ?? [])
  }
  return collect(tree)
}

/** Every host element in a rendered tree, flattened. */
function nodes(tree) {
  const found = []
  const walk = (node) => {
    if (Array.isArray(node)) { node.forEach(walk); return }
    if (!node || typeof node !== 'object') return
    found.push(node)
    walk(node.children ?? [])
  }
  walk(tree)
  return found
}

/** What the host sends, then the browser's re-normalisation of it. */
function browserModel() {
  return buildModel(COMPASS_PAYLOAD)
}

/**
 * The plugin with its seats captured in registration order, and the runtime its
 * hooks write into. The runtime is made once and handed to the factory, because
 * the components close over the React they were given.
 */
async function seats() {
  const definition = await loadBundle()
  const runtime = makeReact()
  const plugin = definition.factory(() => runtime.react)
  const found = []
  plugin.apply({
    effect: (fn) => fn(),
    slots: {
      inject: (slot, register) => register(),
      register: (spec, component) => found.push({ spec, Seat: component }),
    },
    get: () => undefined,
  })
  assert.equal(found.length, 2, 'the plugin registers two seats')
  return { plugin, definition, runtime, readout: found[0], view: found[1] }
}

const SEAT_PROPS = { sessionId: 'session-1', useSessions: undefined, ctx: { get: () => undefined } }

/* ---------------------------------------------------------------- loading */

test('the bundle registers under the package name', async () => {
  const definition = await loadBundle()
  assert.equal(definition.id, 'ores-dsh-environment')
  assert.equal(typeof definition.factory, 'function')
})

test('the factory builds a plugin with both halves of its interface', async () => {
  const definition = await loadBundle()
  const plugin = definition.factory(() => makeReact().react)
  assert.equal(plugin.name, 'ores-dsh-environment')
  assert.deepEqual(plugin.inject, ['slots'])
  assert.equal(typeof plugin.apply, 'function')
})

test('apply registers the readout and the view, in their own effects', async () => {
  const definition = await loadBundle()
  const plugin = definition.factory(() => makeReact().react)
  const seats = []
  const injections = []
  const effects = []
  plugin.apply({
    effect: (fn, label) => { effects.push(label); return fn() },
    slots: {
      inject: (slot, register) => { injections.push(slot); return register() },
      register: (spec, component) => { seats.push({ spec, component }) },
    },
  })

  assert.deepEqual(injections,
    ['conversation.session.header.actions', 'conversation.view'])
  assert.deepEqual(effects,
    ['ores-dsh-environment: now readout', 'ores-dsh-environment: environment view'])
  assert.deepEqual(seats.map((seat) => seat.spec.name),
    ['conversation.session.header.actions', 'conversation.view'])
  assert.deepEqual(seats.map((seat) => seat.spec.id),
    ['ores-dsh-environment-now', 'environment'])
  assert.deepEqual(seats.map((seat) => seat.spec.order), [21, 30])
  assert.equal(seats[1].spec.label(), 'Environment')
})

test('apply survives a plugin context with no slots', async () => {
  const definition = await loadBundle()
  const plugin = definition.factory(() => makeReact().react)
  assert.doesNotThrow(() => plugin.apply({}))
  assert.doesNotThrow(() => plugin.apply(undefined))
})

/* ------------------------------------------------------------- rendering */

test('the readout names the environment once the read lands', async () => {
  const { readout, runtime } = await seats()
  assert.equal(readout.spec.id, 'ores-dsh-environment-now')
  assert.equal(readout.spec.order, 21)
  const painted = texts(await render(readout.Seat, SEAT_PROPS, browserModel(), runtime))
  assert.match(painted, /brave_hopper/)
  assert.match(painted, /db 1d/)
  assert.match(painted, /0\/24/)
})

test('the readout says so when no environment resolves', async () => {
  const { readout, runtime } = await seats()
  const painted = texts(await render(readout.Seat, SEAT_PROPS,
    { ok: false, reason: 'not-an-environment', message: 'no session' }, runtime))
  assert.match(painted, /no environment/)
})

test('the view renders a whole environment without throwing', async () => {
  const { view, runtime } = await seats()
  assert.equal(view.spec.label(), 'Environment')
  const painted = texts(await render(view.Seat, SEAT_PROPS, browserModel(), runtime))

  assert.match(painted, /database/)
  assert.match(painted, /services/)
  assert.match(painted, /config/)
  assert.match(painted, /restored 2026-09-28 15:34/)
  assert.match(painted, /out of sync/)
  assert.match(painted, /running 0 starting 0 stopped 24 failed 0 missing 0/)
  assert.match(painted, /Restore database…/)
  assert.match(painted, /24 of 24 services are stopped/)
  assert.match(painted, /nats-server/)
  assert.match(painted, /Start all/)
  assert.match(painted, /Stop all/)
  assert.match(painted, /logs:/)
  /* Every row offers the journal, including nats-server. */
  assert.match(painted, /Logs/)
})

test('the view draws the states the host sent, in its order and colours', async () => {
  const { view, runtime } = await seats()
  const tree = await render(view.Seat, SEAT_PROPS, browserModel(), runtime)
  const counts = nodes(tree).filter((node) => node.props && node.props['data-env-count'])
  assert.deepEqual(counts.map((node) => node.props['data-env-count']),
    ['running', 'starting', 'stopped', 'failed', 'missing'])
  assert.equal(counts.find((node) => node.props['data-env-count'] === 'stopped').props.style.color,
    '#6e7681')
})

/* ------------------------------------------------------- the model contract */

test('every field the view reads is one the browser model carries', async () => {
  const { plugin } = await seats()
  const model = plugin.__test.normalize(browserModel())

  /* The general guard for the class of defect that blanks a panel: a field the
   * view dereferences that the model never builds. The paths are read out of
   * the bundle's own source, so a new read in the view is covered without this
   * test being edited. */
  const METHODS = new Set(['map', 'filter', 'slice', 'join', 'length', 'forEach',
    'concat', 'indexOf', 'some', 'every', 'includes'])
  const paths = new Set()
  for (const match of BUNDLE_SOURCE.matchAll(/snapshot\.([A-Za-z_$][\w$]*)(?:\.([A-Za-z_$][\w$]*))?/g)) {
    if (match[2] === undefined) { paths.add(match[1]); continue }
    if (METHODS.has(match[2])) continue
    paths.add(`${match[1]}.${match[2]}`)
  }
  assert.ok(paths.size > 10, `expected the view to read many fields, found ${paths.size}`)
  for (const path of paths) {
    const [first, second] = path.split('.')
    assert.notEqual(model[first], undefined, `the browser model has no ${first}`)
    /* A null field is present: the contract types the job slot as null when no
     * action has run. Only undefined means the model never built it. */
    if (second !== undefined && model[first] !== null) {
      assert.notEqual(model[first][second], undefined, `the browser model has no ${path}`)
    }
  }
})

test('every field the log panel reads is one the log model carries', async () => {
  const { plugin } = await seats()
  const log = plugin.__test.normalizeLogs({
    ok: true,
    units: ['ores.web.service-brave_hopper'],
    unit: 'ores.web.service-brave_hopper',
    level: 'warnings',
    count: 2,
    truncated: false,
    lines: ['one', 'two'],
  })
  const paths = new Set()
  for (const match of BUNDLE_SOURCE.matchAll(/\blog\.([A-Za-z_$][\w$]*)(?:\.([A-Za-z_$][\w$]*))?/g)) {
    if (match[2] === undefined) { paths.add(match[1]); continue }
    paths.add(`${match[1]}.${match[2]}`)
  }
  assert.ok(paths.size >= 3, `expected the panel to read the log fields, found ${paths.size}`)
  for (const path of paths) {
    const [first, second] = path.split('.')
    assert.notEqual(log[first], undefined, `the log model has no ${first}`)
    if (second !== undefined && log[first] !== null) {
      assert.notEqual(log[first][second], undefined, `the log model has no ${path}`)
    }
  }
})

test('the log model defaults every field of a failure and of an empty read', async () => {
  const { plugin } = await seats()
  const failed = plugin.__test.normalizeLogs({ ok: false, reason: 'unknown-unit', message: 'x' })
  assert.equal(failed.ok, false)
  assert.deepEqual(failed.lines, [])
  assert.equal(failed.truncated, false)
  const empty = plugin.__test.normalizeLogs({ ok: true, count: 0, lines: [] })
  assert.equal(empty.ok, true)
  assert.deepEqual(empty.lines, [])
  assert.equal(empty.level, 'all')
})

test('the browser model carries the remedies the restore control names', async () => {
  const { plugin } = await seats()
  const model = plugin.__test.normalize(browserModel())
  assert.equal(model.remedies.recreateDatabase, 'compass db recreate -y -k')
  assert.equal(model.remedies.startServices, 'compass services start')
  assert.equal(model.remedies.stopServices, 'compass services stop')
})

test('the browser model carries every state the host sent, with its flag', async () => {
  const { plugin } = await seats()
  const model = plugin.__test.normalize(browserModel())
  assert.deepEqual(model.services.states.map((state) => state.id),
    ['running', 'starting', 'stopped', 'failed', 'missing'])
  assert.deepEqual(model.services.brokenStates, ['failed', 'missing'])
})

test('a partial payload still produces a whole model', async () => {
  const { plugin } = await seats()
  const model = plugin.__test.normalize({ ok: true })
  assert.equal(model.ok, true)
  assert.deepEqual(model.services.units, [])
  assert.deepEqual(model.services.brokenStates, [])
  assert.deepEqual(model.remedies,
    { startServices: '', stopServices: '', recreateDatabase: '', configureEnv: '' })
  assert.equal(model.job, null)
})

/* ---------------------------------------------------------------- palette */

test('the bundle carries the palette the story promises', () => {
  const palette = /const STATE_COLOR = \{([\s\S]*?)\}/.exec(BUNDLE_SOURCE)
  assert.ok(palette, 'the bundle declares no STATE_COLOR table')
  const entries = Object.fromEntries(
    [...palette[1].matchAll(/(\w+):\s*'(#[0-9a-f]{6})'/g)].map(([, key, value]) => [key, value]))
  assert.equal(entries.running, '#3fb950')
  assert.equal(entries.starting, '#d29922')
  assert.equal(entries.stopped, '#6e7681')
  assert.equal(entries.failed, '#f85149')
  assert.equal(entries.missing, '#f85149')
  assert.equal(entries.failed, entries.missing, 'both broken states are red together')
  assert.notEqual(entries.stopped, entries.failed, 'a stopped unit must not read as broken')

  const tones = /const TONE_COLOR = \{([\s\S]*?)\}/.exec(BUNDLE_SOURCE)
  assert.ok(tones, 'the bundle declares no TONE_COLOR table')
  const byTone = Object.fromEntries(
    [...tones[1].matchAll(/(\w+):\s*'(#[0-9a-f]{6})'/g)].map(([, key, value]) => [key, value]))
  assert.equal(byTone.ok, '#3fb950')
  assert.equal(byTone.warn, '#d29922')
  assert.equal(byTone.critical, '#f85149')
  assert.equal(byTone.unknown, '#6e7681')
})

test('both seats fetch the routes the host registers', () => {
  assert.match(BUNDLE_SOURCE, /fetch\('\/plugins\/ores-dsh-environment\/state\?'/)
  assert.match(BUNDLE_SOURCE, /fetch\('\/plugins\/ores-dsh-environment\/action', \{/)
})
