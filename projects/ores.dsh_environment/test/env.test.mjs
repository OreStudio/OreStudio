/**
 * Host-half tests: the transform, the action table, and the two boundaries
 * that can be exercised without a live environment.
 *
 * Run from the plugin directory: node --test
 */

import test from 'node:test'
import assert from 'node:assert/strict'
import { mkdtempSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

import { ACTIONS, jobFor, readEnvironment, resetJobs, resolveWorktree, startAction }
  from '../lib/compass.js'
import { buildModel, failure } from '../lib/env.js'

const PAYLOAD = {
  ok: true,
  generatedAt: '2026-09-30T12:45:00Z',
  env: {
    name: 'brave_hopper',
    label: 'brave_hopper',
    preset: 'linux-clang-debug-make',
    worktree: '/home/marco/Development/OreStudio/ores_dev_brave_hopper',
    envVersion: 26,
    requiredEnvVersion: 26,
    envStale: false,
    scope: 'dsh-1.scope',
    slice: 'app.slice',
    activities: [],
    vcpkgWarning: '',
  },
  database: {
    reachable: true,
    name: 'ores_dev_brave_hopper',
    restoredAt: '2026-09-28 15:34',
    restoredAgeSeconds: 162000,
    restoredAge: '1d',
    restoredLevel: 'critical',
    schemaVersion: '0.0.25',
    builtFrom: '5559b012aed',
    builtAt: '2026/09/28 14:19:46',
    driftSeconds: 90000,
    driftLabel: '1d behind HEAD — drifting',
    driftLevel: 'warn',
    bootstrapMode: true,
    warning: '',
  },
  services: {
    total: 24,
    counts: { running: 0, starting: 0, stopped: 24, failed: 0, missing: 0 },
    nats: { unit: 'nats-server-brave_hopper', label: 'nats-server', state: 'stopped', detail: 'inactive' },
    units: [
      {
        unit: 'ores.web.service-brave_hopper',
        service: 'ores.web.service',
        replica: 0,
        label: 'web',
        log: 'ores.web.service.0.log',
        state: 'stopped',
        detail: 'inactive',
      },
    ],
    logDir: '/logs',
  },
  health: { level: 'warn', reasons: ['24 of 24 services are stopped'] },
  remedies: {
    startServices: 'compass services start',
    stopServices: 'compass services stop',
    recreateDatabase: 'compass db recreate -y -k',
    configureEnv: 'compass env configure --preset p -y',
  },
}

test('the model takes the state vocabulary from the payload, in its order', () => {
  const model = buildModel(PAYLOAD)
  assert.deepEqual(model.services.states.map((state) => state.id),
    ['running', 'starting', 'stopped', 'failed', 'missing'])
  assert.deepEqual(model.services.states.map((state) => state.count),
    [0, 0, 24, 0, 0])
  assert.equal(model.services.states[3].title, 'Failed')
})

test('a state this build does not know still reaches the browser', () => {
  const payload = {
    ...PAYLOAD,
    services: { ...PAYLOAD.services, counts: { running: 1, degraded: 2 } },
  }
  const model = buildModel(payload)
  assert.deepEqual(model.services.states.map((state) => state.id), ['running', 'degraded'])
})

test('the database tile carries the restore age and compass own level', () => {
  const model = buildModel(PAYLOAD)
  const tile = model.tiles.find((entry) => entry.id === 'database')
  assert.equal(tile.value, '1d')
  assert.equal(tile.tone, 'critical')
  assert.equal(tile.detail, 'schema 0.0.25 · 1d behind HEAD — drifting')
})

test('a failed service makes the services tile critical', () => {
  const payload = {
    ...PAYLOAD,
    services: { ...PAYLOAD.services, counts: { running: 23, stopped: 0, failed: 1, missing: 0 } },
  }
  const tile = buildModel(payload).tiles.find((entry) => entry.id === 'services')
  assert.equal(tile.value, '23/24')
  assert.equal(tile.tone, 'critical')
})

test('a missing unit makes the services tile critical', () => {
  const payload = {
    ...PAYLOAD,
    services: { ...PAYLOAD.services, counts: { running: 22, stopped: 0, failed: 0, missing: 2 } },
  }
  assert.equal(buildModel(payload).tiles.find((t) => t.id === 'services').tone, 'critical')
})

test('stopped services warn rather than fail', () => {
  assert.equal(buildModel(PAYLOAD).tiles.find((t) => t.id === 'services').tone, 'warn')
})

test('every service running reads ok', () => {
  const payload = {
    ...PAYLOAD,
    services: { ...PAYLOAD.services, counts: { running: 24, starting: 0, stopped: 0, failed: 0, missing: 0 } },
    database: { ...PAYLOAD.database, restoredLevel: 'ok', driftLevel: 'ok' },
    health: { level: 'ok', reasons: [] },
  }
  assert.equal(buildModel(payload).tiles.find((t) => t.id === 'services').tone, 'ok')
  assert.equal(buildModel(payload).health.level, 'ok')
})

test('an unreachable database is critical and names the remedy', () => {
  const payload = {
    ...PAYLOAD,
    database: { reachable: false, name: 'ores_dev_brave_hopper', reason: 'unreachable' },
    health: { level: 'critical', reasons: ['the database does not answer'] },
  }
  const model = buildModel(payload)
  assert.equal(model.database.reachable, false)
  assert.equal(model.tiles.find((t) => t.id === 'database').tone, 'critical')
  assert.equal(model.tiles.find((t) => t.id === 'database').value, '—')
  assert.equal(model.remedies.recreateDatabase, 'compass db recreate -y -k')
})

test('a stale env file tones the config tile warn', () => {
  const payload = { ...PAYLOAD, env: { ...PAYLOAD.env, envStale: true, requiredEnvVersion: 27 } }
  const tile = buildModel(payload).tiles.find((entry) => entry.id === 'config')
  assert.equal(tile.tone, 'warn')
  assert.equal(tile.value, 'v26')
})

test('the confirmation phrase is the database name', () => {
  assert.equal(buildModel(PAYLOAD).database.confirmPhrase, 'ores_dev_brave_hopper')
})

test('the work tree name drops the ores_dev_ prefix', () => {
  assert.equal(buildModel(PAYLOAD).env.worktreeName, 'brave_hopper')
})

test('a replicated service carries the service name as its selector', () => {
  const payload = {
    ...PAYLOAD,
    services: {
      ...PAYLOAD.services,
      total: 2,
      units: [
        { unit: 'ores.compute.wrapper-brave_hopper-1', service: 'ores.compute.wrapper',
          replica: 1, label: 'compute.wrapper-1', state: 'stopped', detail: '' },
        { unit: 'ores.compute.wrapper-brave_hopper-2', service: 'ores.compute.wrapper',
          replica: 2, label: 'compute.wrapper-2', state: 'stopped', detail: '' },
      ],
    },
  }
  const units = buildModel(payload).services.units
  /* compass resolves a registry service and never one replica of it, so a
   * start or stop must send the service name. Sending the per-replica label
   * fails its registry lookup, which is what this pins. */
  assert.deepEqual(units.map((unit) => unit.selector),
    ['ores.compute.wrapper', 'ores.compute.wrapper'])
  assert.deepEqual(units.map((unit) => unit.label),
    ['compute.wrapper-1', 'compute.wrapper-2'])
})

test('a unit with no registry service carries no selector', () => {
  const payload = {
    ...PAYLOAD,
    services: {
      ...PAYLOAD.services,
      units: [{ unit: 'nats-server-brave_hopper', service: '', replica: 0,
                label: 'nats-server', state: 'stopped', detail: '' }],
    },
  }
  assert.equal(buildModel(payload).services.units[0].selector, '')
})

test('a payload with nothing in it still produces a whole model', () => {
  /* No counts means no states, not five invented ones: a fleet nobody could
   * read must not render as an empty but healthy one. */
  const model = buildModel({})
  assert.equal(model.ok, true)
  assert.equal(model.env.name, 'unknown')
  assert.equal(model.services.total, 0)
  assert.deepEqual(model.services.states, [])
  assert.deepEqual(model.health.reasons, [])
})

test('failure carries a reason and a message for the browser', () => {
  assert.deepEqual(failure('compass-too-old', 'rebase the work tree'),
    { ok: false, reason: 'compass-too-old', message: 'rebase the work tree' })
})

test('the restore sequence stops the fleet, rebuilds, then starts it', () => {
  assert.deepEqual(ACTIONS['database-restore'].steps({ stopServices: true }),
    [['services', 'stop'], ['db', 'recreate', '-y', '-k'], ['services', 'start']])
})

test('the rebuild-only sequence leaves the fleet alone', () => {
  assert.deepEqual(ACTIONS['database-restore'].steps({ stopServices: false }),
    [['db', 'recreate', '-y', '-k']])
})

test('a single service action names only that service', () => {
  assert.deepEqual(ACTIONS['service-stop'].steps({ service: 'web' }),
    [['services', 'stop', 'web']])
})

test('the fleet actions take no service', () => {
  assert.deepEqual(ACTIONS['fleet-start'].steps({}), [['services', 'start']])
  assert.deepEqual(ACTIONS['fleet-stop'].steps({}), [['services', 'stop']])
})

test('an unknown action is refused', () => {
  resetJobs()
  const result = startAction('/tmp/tree', { action: 'drop-everything' }, 'name')
  assert.equal(result.ok, false)
  assert.equal(result.reason, 'unknown-action')
})

test('a service action without a usable selector is refused', () => {
  resetJobs()
  for (const service of ['', 'web; rm -rf /', '../../etc', undefined]) {
    const result = startAction('/tmp/tree', { action: 'service-stop', service }, 'name')
    assert.equal(result.ok, false, `expected ${String(service)} to be refused`)
    assert.equal(result.reason, 'bad-service')
  }
})

test('a restore without the environment name is refused', () => {
  resetJobs()
  const result = startAction('/tmp/tree', { action: 'database-restore', stopServices: true }, 'brave_hopper')
  assert.equal(result.ok, false)
  assert.equal(result.reason, 'not-confirmed')
})

test('a restore for a different environment is refused', () => {
  resetJobs()
  const result = startAction('/tmp/tree',
    { action: 'database-restore', confirm: 'merry_newton' }, 'brave_hopper')
  assert.equal(result.ok, false)
  assert.equal(result.reason, 'not-confirmed')
})

test('no job is reported for a work tree nothing has run in', () => {
  resetJobs()
  assert.equal(jobFor('/tmp/never-used'), null)
})

test('a directory that is not an ORE Studio checkout does not resolve', async () => {
  const scratch = mkdtempSync(join(tmpdir(), 'ores-dsh-env-'))
  assert.equal(await resolveWorktree(scratch), null)
  assert.equal(await resolveWorktree('/home/marco'), null)
  assert.equal(await resolveWorktree('/nonexistent-path-for-tests'), null)
  assert.equal(await resolveWorktree(''), null)
  assert.equal(await resolveWorktree(undefined), null)
})

test('reading an environment where compass is missing reports a failure', async () => {
  const scratch = mkdtempSync(join(tmpdir(), 'ores-dsh-env-'))
  const result = await readEnvironment(scratch)
  assert.equal(result.ok, false)
  assert.equal(result.reason, 'compass-failed')
  assert.ok(result.message.length > 0)
})
