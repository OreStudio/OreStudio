/**
 * Client-half tests: the bundle DSH serves verbatim.
 *
 * There is no build step and no compiler between this file and the browser, so
 * a mistake in it is invisible until the panel fails to appear. These tests
 * load the bundle the way the module loader does, run the factory, apply the
 * plugin to a stub slot registry, and render each seat once with a stub React.
 * That catches the whole "blank panel" class: a top-level error, a bad seat
 * name, a wrong order, or a first render that throws.
 *
 * Run from the plugin directory: node --test
 */

import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const here = dirname(fileURLToPath(import.meta.url))
const bundle = resolve(here, '..', 'lib', 'client.js')

/* A React just large enough for a first render. Effects never run, so each
 * component is exercised in its initial state, which is the state that decides
 * whether anything appears at all. */
function stubReact() {
  const createElement = (type, props, ...children) => ({
    type,
    props: props ?? {},
    children: children.flat(Infinity).filter((child) => child !== null && child !== undefined),
  })
  return {
    createElement,
    createContext: (value) => {
      const context = { __value: value }
      context.Provider = (props) => props.children
      return context
    },
    useContext: (context) => (context && '__value' in context ? context.__value : ''),
    useState: (initial) => [typeof initial === 'function' ? initial() : initial, () => {}],
    useEffect: () => {},
    useRef: (initial) => ({ current: initial }),
    useCallback: (fn) => fn,
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

const BUNDLE_SOURCE = readFileSync(bundle, 'utf8')

test('the bundle registers under the package name', async () => {
  const definition = await loadBundle()
  assert.equal(definition.id, 'ores-dsh-environment')
  assert.equal(typeof definition.factory, 'function')
})

test('the factory builds a plugin with both halves of its interface', async () => {
  const definition = await loadBundle()
  const plugin = definition.factory((name) => {
    assert.equal(name, 'react', 'the bundle requires nothing but react from the platform')
    return stubReact()
  })
  assert.equal(plugin.name, 'ores-dsh-environment')
  assert.deepEqual(plugin.inject, ['slots'])
  assert.equal(typeof plugin.apply, 'function')
})

test('apply registers the readout and the view, in their own effects', async () => {
  const definition = await loadBundle()
  const plugin = definition.factory(() => stubReact())
  const injections = []
  const seats = []
  const effects = []
  const ctx = {
    effect: (fn, label) => { effects.push(label); return fn() },
    slots: {
      inject: (slot, register) => { injections.push(slot); return register() },
      register: (spec, component) => { seats.push({ spec, component }) },
    },
  }
  plugin.apply(ctx)

  assert.deepEqual(injections,
    ['conversation.session.header.actions', 'conversation.view'])
  assert.deepEqual(effects,
    ['ores-dsh-environment: now readout', 'ores-dsh-environment: environment view'])
  assert.deepEqual(seats.map((seat) => seat.spec.name),
    ['conversation.session.header.actions', 'conversation.view'])
  assert.deepEqual(seats.map((seat) => seat.spec.id),
    ['ores-dsh-environment-now', 'environment'])
  assert.deepEqual(seats.map((seat) => seat.spec.order), [21, 30])
  assert.equal(typeof seats[1].spec.label(), 'string')
  assert.equal(seats[1].spec.label(), 'Environment')
})

test('apply survives a plugin context with no slots', async () => {
  const definition = await loadBundle()
  const plugin = definition.factory(() => stubReact())
  assert.doesNotThrow(() => plugin.apply({}))
  assert.doesNotThrow(() => plugin.apply(undefined))
})

test('each seat renders a first frame without throwing', async () => {
  const definition = await loadBundle()
  const plugin = definition.factory(() => stubReact())
  const seats = []
  plugin.apply({
    effect: (fn) => fn(),
    slots: {
      inject: (slot, register) => register(),
      register: (spec, component) => { seats.push(component) },
    },
    get: () => undefined,
  })

  for (const Seat of seats) {
    const tree = Seat({ sessionId: 'session-1', useSessions: undefined, ctx: { get: () => undefined } })
    assert.ok(tree, 'a seat rendered nothing at all')
  }
})

test('the readout names the environment and the view names itself', async () => {
  const definition = await loadBundle()
  const plugin = definition.factory(() => stubReact())
  const seats = []
  plugin.apply({
    effect: (fn) => fn(),
    slots: {
      inject: (slot, register) => register(),
      register: (spec, component) => { seats.push(component) },
    },
    get: () => undefined,
  })

  /* A first render with no snapshot: the readout must say so rather than
   * render an empty chip, and the view must show its loading line. */
  const texts = (tree) => {
    const render = (node) => {
      if (Array.isArray(node)) return node.map(render).join(' ')
      if (typeof node === 'string') return node
      if (typeof node === 'number') return String(node)
      if (!node || typeof node !== 'object') return ''
      if (typeof node.type === 'function') {
        return render(node.type({ ...node.props, children: node.children }))
      }
      return render(node.children ?? [])
    }
    return render(tree)
  }

  const readout = texts(seats[0]({ sessionId: 's', useSessions: undefined, ctx: { get: () => undefined } }))
  assert.match(readout, /no environment/)

  const view = texts(seats[1]({ sessionId: 's', useSessions: undefined, ctx: { get: () => undefined } }))
  assert.match(view, /Reading the environment/)
})

test('the bundle carries the palette the story promises', () => {
  /* The host decides the tone; the bundle decides the colour. A failed unit
   * and a unit the manager has never heard of are both red, and a stopped one
   * is not, because stopped is the state an operator chooses. */
  const palette = /const STATE_COLOR = \{([\s\S]*?)\}/.exec(BUNDLE_SOURCE)
  assert.ok(palette, 'the bundle declares no STATE_COLOR table')
  const entries = Object.fromEntries(
    [...palette[1].matchAll(/(\w+):\s*'(#[0-9a-f]{6})'/g)].map(([, key, value]) => [key, value]))
  assert.equal(entries.running, '#3fb950')
  assert.equal(entries.failed, '#f85149')
  assert.equal(entries.missing, '#f85149')
  assert.equal(entries.stopped, '#6e7681')
  assert.equal(entries.failed, entries.missing, 'both broken states are red together')
  assert.notEqual(entries.stopped, entries.failed, 'a stopped unit must not read as broken')
})

test('both seats reach the routes the host registers', () => {
  assert.ok(BUNDLE_SOURCE.includes('/plugins/ores-dsh-environment/state'))
  assert.ok(BUNDLE_SOURCE.includes('/plugins/ores-dsh-environment/action'))
})
