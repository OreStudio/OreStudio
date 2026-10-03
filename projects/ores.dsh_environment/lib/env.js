/**
 * ores-dsh-environment host half: the pure transform.
 *
 * Reads the object `compass env status --json` prints and produces the model
 * the browser renders. It touches no process and no file, so every rule it
 * carries can be tested on its own.
 *
 * Two rules live here rather than in the browser, because the browser must
 * not be the second place either one is written down:
 *
 * - the service state vocabulary, which is read out of the payload rather
 *   than restated, so a state compass adds appears here without a second
 *   edit;
 * - the tone of each health tile, which the browser turns into a colour.
 *
 * The level of the environment as a whole is compass's, not ours: it arrives
 * in `health` with the reasons that produced it.
 */

/** The shape a failure takes. The browser switches on `reason`. */
export function failure(reason, message) {
  return { ok: false, reason, message, job: null }
}

/* The states that mean something is broken rather than merely switched off.
 * One list, read by the tile tone and by the flag each state carries, so the
 * browser never restates it. */
const BROKEN_STATES = new Set(['failed', 'missing'])

const isObject = (value) => typeof value === 'object' && value !== null && !Array.isArray(value)
const str = (value, fallback = '') => (typeof value === 'string' ? value : fallback)
const num = (value, fallback = 0) => (typeof value === 'number' && Number.isFinite(value) ? value : fallback)
const arr = (value) => (Array.isArray(value) ? value : [])
const bool = (value, fallback = false) => (typeof value === 'boolean' ? value : fallback)

const NAME_FALLBACK = 'unknown'

/** A work tree's directory name, which is what a person calls the checkout. */
function worktreeName(path) {
  const trimmed = str(path).replace(/\/+$/, '')
  const name = trimmed.split('/').pop() ?? ''
  return name.startsWith('ores_dev_') ? name.slice('ores_dev_'.length) : name
}

/**
 * The tone of a tile: the browser's colour comes from this.
 *
 * `ok` is the healthy state, `warn` is something that needs attention
 * without being broken, `critical` is broken, and `unknown` is a value that
 * could not be read.
 */
function serviceTone(counts, total) {
  for (const state of BROKEN_STATES) {
    if (num(counts[state]) > 0) return 'critical'
  }
  if (total > 0 && num(counts.running) === total) return 'ok'
  if (total === 0) return 'unknown'
  return 'warn'
}

export function buildModel(payload) {
  const env = isObject(payload.env) ? payload.env : {}
  const database = isObject(payload.database) ? payload.database : {}
  const services = isObject(payload.services) ? payload.services : {}
  const health = isObject(payload.health) ? payload.health : {}
  const remedies = isObject(payload.remedies) ? payload.remedies : {}

  const name = str(env.name, NAME_FALLBACK)
  const envVersion = num(env.envVersion)
  const requiredEnvVersion = env.requiredEnvVersion === null || env.requiredEnvVersion === undefined
    ? null
    : num(env.requiredEnvVersion)
  const activities = arr(env.activities).map((activity) => ({
    number: num(activity.number),
    date: str(activity.date),
    title: str(activity.title),
    recipeId: str(activity.recipeId),
  }))

  const counts = isObject(services.counts) ? services.counts : {}
  /* The vocabulary comes from the payload, in the order compass emits it,
   * so this file never restates the list. */
  const states = Object.keys(counts).map((id) => ({
    id,
    title: id.charAt(0).toUpperCase() + id.slice(1),
    count: num(counts[id]),
    broken: BROKEN_STATES.has(id),
  }))
  const total = num(services.total)

  const units = arr(services.units).map((unit) => ({
    unit: str(unit.unit),
    /* The selector a start or stop must send. compass resolves a registry
     * service and never one replica of it, so every row of a replicated
     * service carries the service name while its label stays per replica.
     * Empty for nats-server, which is not in the registry and has no
     * selector. */
    selector: str(unit.service),
    service: str(unit.service),
    replica: num(unit.replica),
    label: str(unit.label, str(unit.unit)),
    state: str(unit.state, 'unknown'),
    detail: str(unit.detail),
  }))
  const nats = isObject(services.nats)
    ? {
        unit: str(services.nats.unit),
        label: str(services.nats.label, 'nats-server'),
        state: str(services.nats.state, 'unknown'),
        detail: str(services.nats.detail),
      }
    : null

  const reachable = bool(database.reachable)
  const driftLabel = str(database.driftLabel)
  const driftLevel = str(database.driftLevel, 'unknown')

  const tiles = [
    {
      id: 'database',
      label: 'database',
      value: reachable ? str(database.restoredAge, '?') : '—',
      tone: reachable ? str(database.restoredLevel, 'unknown') : 'critical',
      detail: reachable
        ? `schema ${driftLabel || '?'}`
        : 'unreachable',
    },
    {
      id: 'services',
      label: 'services',
      value: `${num(counts.running)}/${total}`,
      tone: serviceTone(counts, total),
      detail: states
        .filter((state) => state.count > 0)
        .map((state) => `${state.count} ${state.id}`)
        .join(' · ') || 'none reported',
    },
    {
      id: 'config',
      label: 'config',
      value: `v${envVersion}`,
      tone: bool(env.envStale) ? 'warn' : 'ok',
      detail: activities.length > 0
        ? `${activities.length} activit${activities.length === 1 ? 'y' : 'ies'} outstanding`
        : (requiredEnvVersion !== null && envVersion !== requiredEnvVersion
            ? `needs v${requiredEnvVersion}`
            : 'current'),
    },
  ]

  return {
    ok: true,
    generatedAt: str(payload.generatedAt),
    env: {
      name,
      label: str(env.label, name),
      preset: str(env.preset),
      worktree: str(env.worktree),
      worktreeName: worktreeName(env.worktree),
      scope: str(env.scope),
      slice: str(env.slice),
      envVersion,
      requiredEnvVersion,
      envStale: bool(env.envStale),
      activities,
      vcpkgWarning: str(env.vcpkgWarning),
    },
    database: {
      reachable,
      name: str(database.name),
      restoredAt: str(database.restoredAt),
      restoredAge: str(database.restoredAge),
      restoredAgeSeconds: database.restoredAgeSeconds === null
        || database.restoredAgeSeconds === undefined
        ? null
        : num(database.restoredAgeSeconds),
      restoredLevel: str(database.restoredLevel, 'unknown'),
      schemaFingerprint: str(database.schemaFingerprint),
      expectedFingerprint: str(database.expectedFingerprint),
      builtFrom: str(database.builtFrom),
      builtAt: str(database.builtAt),
      driftLabel,
      driftLevel,
      bootstrapMode: database.bootstrapMode === true,
      warning: str(database.warning),
      /* The name the restore dialog makes the operator type. */
      confirmPhrase: reachable || database.name ? str(database.name, name) : name,
    },
    services: {
      total,
      counts: counts,
      states,
      units,
      nats,
      logDir: str(services.logDir),
    },
    health: {
      level: str(health.level, 'unknown'),
      reasons: arr(health.reasons).map((reason) => str(reason)),
    },
    tiles,
    remedies: {
      startServices: str(remedies.startServices),
      stopServices: str(remedies.stopServices),
      recreateDatabase: str(remedies.recreateDatabase),
      configureEnv: str(remedies.configureEnv),
    },
  }
}
