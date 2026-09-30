/**
 * ores-dsh-environment host half: the Compass boundary.
 *
 * This is the only module that starts a process. A read runs
 * `<worktree>/compass.sh env status --json` and answers with the parsed
 * object. An action runs compass and keeps its output as a tail, because
 * compass prints its own phases, and a progress meter computed from nothing
 * would be a guess.
 *
 * One action runs per work tree at a time, and its state lives here in the
 * server process. The state route reports it, so the browser can show what
 * compass is doing without holding a request open for the minutes a database
 * restore takes.
 */

import { spawn } from 'node:child_process'
import { promises as fs } from 'node:fs'
import { join } from 'node:path'

/* A read is one compass process. The CLI takes about a second and a half on
 * a healthy checkout; the rest is headroom for a cold import. */
export const READ_TIMEOUT_MS = 30000

/* Restoring the database prints a few hundred lines of SQL notices. Keeping
 * the tail rather than everything bounds the payload the browser polls. */
const TAIL_LIMIT = 400

/* A service selector reaches a process argument list, so it is constrained
 * to what compass itself accepts: a registry name (ores.web.service), a unit
 * name (ores.web.service-brave_hopper) or a short name (web). */
const SELECTOR = /^[A-Za-z0-9][A-Za-z0-9._-]*$/

/* Every action is a sequence of compass argument lists. `database-restore`
 * composes three, because a rebuild leaves running services holding
 * connections to tables that no longer exist; compass's own connection check
 * says to stop the services first. */
export const ACTIONS = {
  'service-start': {
    label: (request) => `Start ${request.service}`,
    steps: (request) => [['services', 'start', request.service]],
    timeoutMs: 900000,
  },
  'service-stop': {
    label: (request) => `Stop ${request.service}`,
    steps: (request) => [['services', 'stop', request.service]],
    timeoutMs: 300000,
  },
  'fleet-start': {
    label: () => 'Start every service',
    steps: () => [['services', 'start']],
    timeoutMs: 1800000,
  },
  'fleet-stop': {
    label: () => 'Stop every service',
    steps: () => [['services', 'stop']],
    timeoutMs: 900000,
  },
  'database-restore': {
    /* A rebuild is minutes of SQL; the deadline is a safety net against a
     * wedged process, not a budget. */
    timeoutMs: 3600000,
    label: (request) => request.stopServices
      ? 'Stop services, rebuild the database, start services'
      : 'Rebuild the database',
    steps: (request) => request.stopServices
      ? [['services', 'stop'], ['db', 'recreate', '-y', '-k'], ['services', 'start']]
      : [['db', 'recreate', '-y', '-k']],
  },
}

/* The step names the browser shows, in the order the sequence runs them. */
const STEP_NAMES = {
  'services stop': 'stop services',
  'services start': 'start services',
  'db recreate -y -k': 'rebuild the database',
}

const jobs = new Map()

function stepName(args) {
  return STEP_NAMES[args.join(' ')] ?? args.join(' ')
}

function run(root, args, timeoutMs) {
  return new Promise((resolve) => {
    const child = spawn('bash', [join(root, 'compass.sh'), ...args], { cwd: root })
    let stdout = ''
    let stderr = ''
    const timer = timeoutMs
      ? setTimeout(() => child.kill('SIGKILL'), timeoutMs)
      : null
    const settle = (code, extra) => {
      if (timer) clearTimeout(timer)
      resolve({ code, stdout, stderr: stderr + (extra ?? '') })
    }
    child.stdout.on('data', (chunk) => { stdout += chunk })
    child.stderr.on('data', (chunk) => { stderr += chunk })
    child.on('error', (error) => settle(-1, String((error && error.message) || error)))
    child.on('close', (code) => settle(code))
  })
}

const firstLine = (text) =>
  String(text || '').split('\n').map((line) => line.trim()).filter(Boolean)[0] ?? ''

/* The levels `compass services logs` accepts, and the flag each one is. A
 * level is not journald's own priority: journald records console output at one
 * priority, so compass matches the token the service printed instead. */
export const LOG_LEVELS = { all: '', warnings: '--warnings', errors: '--errors' }

/**
 * One unit's recent journal output, as compass reports it.
 *
 * Fetched on demand rather than with the environment, because a log tail
 * changes constantly and is only wanted when somebody is looking at it. The
 * same failure reasons as the environment read, plus `unknown-unit` when
 * compass resolves the selector to nothing.
 */
export async function readLogs(root, unit, level = 'all', lines = 200) {
  const flag = LOG_LEVELS[level] ?? LOG_LEVELS.all
  const args = ['services', 'logs', unit, '-n', String(lines), '--json']
  if (flag) args.push(flag)
  const result = await run(root, args, READ_TIMEOUT_MS)
  if (result.code !== 0) {
    const text = result.stderr + result.stdout
    if (/invalid choice/.test(text)) {
      return {
        ok: false,
        reason: 'compass-too-old',
        message: 'this work tree carries a compass without `services logs`; '
          + 'rebase the work tree to pick it up',
      }
    }
    if (/nothing named/.test(text)) {
      return { ok: false, reason: 'unknown-unit', message: unit }
    }
    return {
      ok: false,
      reason: 'compass-failed',
      message: firstLine(text) || `compass exited with code ${result.code}`,
    }
  }
  try {
    const payload = JSON.parse(result.stdout)
    if (!payload || typeof payload !== 'object' || payload.ok !== true) {
      return {
        ok: false,
        reason: 'compass-unreadable',
        message: 'compass services logs reported no output',
      }
    }
    return { ok: true, payload }
  } catch {
    return {
      ok: false,
      reason: 'compass-unreadable',
      message: 'compass services logs --json did not print JSON',
    }
  }
}

/**
 * The environment of one work tree, as compass reports it.
 *
 * Returns { ok: true, payload } or { ok: false, reason, message }. The
 * reasons are the contract: `not-an-environment` when the directory is not
 * an ORE Studio checkout, `compass-too-old` when its compass has no
 * `env status` verb, `compass-failed` when the verb ran and failed, and
 * `compass-unreadable` when it did not print JSON.
 */
export async function readEnvironment(root) {
  const result = await run(root, ['env', 'status', '--json'], READ_TIMEOUT_MS)
  if (result.code !== 0) {
    const text = result.stderr + result.stdout
    if (/invalid choice/.test(text)) {
      return {
        ok: false,
        reason: 'compass-too-old',
        message: 'this work tree carries a compass without `env status`; '
          + 'rebase the work tree to pick it up',
      }
    }
    return {
      ok: false,
      reason: 'compass-failed',
      message: firstLine(text) || `compass exited with code ${result.code}`,
    }
  }
  try {
    const payload = JSON.parse(result.stdout)
    if (!payload || typeof payload !== 'object' || payload.ok !== true) {
      return {
        ok: false,
        reason: 'compass-unreadable',
        message: 'compass env status --json reported no environment',
      }
    }
    return { ok: true, payload }
  } catch {
    return {
      ok: false,
      reason: 'compass-unreadable',
      message: 'compass env status --json did not print JSON',
    }
  }
}

function pushTail(job, chunk) {
  const parts = (job.pending + String(chunk)).split('\n')
  job.pending = parts.pop() ?? ''
  job.lines = job.lines.concat(parts).slice(-TAIL_LIMIT)
}

function flushTail(job) {
  if (job.pending === '') return
  job.lines = job.lines.concat([job.pending]).slice(-TAIL_LIMIT)
  job.pending = ''
}

function runStep(root, args, job, timeoutMs) {
  return new Promise((resolve) => {
    const step = {
      name: stepName(args),
      args,
      running: true,
      code: null,
      startedAt: new Date().toISOString(),
      finishedAt: null,
    }
    job.steps.push(step)
    const child = spawn('bash', [join(root, 'compass.sh'), ...args], { cwd: root })
    const timer = timeoutMs
      ? setTimeout(() => {
          pushTail(job, `\ncompass did not finish within ${Math.round(timeoutMs / 1000)}s; it was killed\n`)
          child.kill('SIGKILL')
        }, timeoutMs)
      : null
    const settle = (code) => {
      if (timer) clearTimeout(timer)
      flushTail(job)
      step.code = code
      step.running = false
      step.finishedAt = new Date().toISOString()
      resolve(code)
    }
    child.stdout.on('data', (chunk) => pushTail(job, chunk))
    child.stderr.on('data', (chunk) => pushTail(job, chunk))
    child.on('error', (error) => {
      pushTail(job, String((error && error.message) || error) + '\n')
      settle(-1)
    })
    child.on('close', (code) => settle(code))
  })
}

function jobView(job) {
  const elapsed = (from, to) =>
    Math.max(0, Math.round(((to ?? Date.now()) - Date.parse(from)) / 1000))
  return {
    id: job.id,
    kind: job.kind,
    label: job.label,
    running: job.running,
    code: job.code,
    startedAt: job.startedAt,
    finishedAt: job.finishedAt,
    elapsedSeconds: elapsed(job.startedAt, job.finishedAt ? Date.parse(job.finishedAt) : undefined),
    steps: job.steps.map((step) => ({
      name: step.name,
      running: step.running,
      code: step.code,
      elapsedSeconds: elapsed(step.startedAt,
        step.finishedAt ? Date.parse(step.finishedAt) : undefined),
    })),
    tail: job.lines,
  }
}

/** The action job for a work tree, or null when none has run here. */
export function jobFor(root) {
  const job = jobs.get(root)
  return job ? jobView(job) : null
}

/**
 * Start an action. Returns { ok: true, job } or { ok: false, reason,
 * message }.
 *
 * `expectedName` is the environment name the browser was last shown. It
 * guards the one destructive action: a caller may only rebuild the database
 * it has already been shown, so a stale or mistaken request cannot drop
 * another checkout's database.
 *
 * `onSettled` runs once when the sequence ends, whatever its exit code. The
 * caller uses it to drop a cached read, because a rebuild rewrites the
 * database and a fleet action rewrites every unit's state.
 */
export function startAction(root, request, expectedName, onSettled) {
  const kind = typeof request?.action === 'string' ? request.action : ''
  const spec = ACTIONS[kind]
  if (!spec) {
    return { ok: false, reason: 'unknown-action', message: `no action named '${kind}'` }
  }
  const running = jobs.get(root)
  if (running && running.running) {
    return {
      ok: false,
      reason: 'busy',
      message: `'${running.label}' is still running in this work tree`,
    }
  }
  if (kind === 'service-start' || kind === 'service-stop') {
    const service = typeof request.service === 'string' ? request.service : ''
    if (!SELECTOR.test(service)) {
      return {
        ok: false,
        reason: 'bad-service',
        message: 'name a service, for example `web` or `ores.web.service`',
      }
    }
  }
  const stopServices = request?.stopServices !== false
  if (kind === 'database-restore'
      && (!expectedName || request?.confirm !== expectedName)) {
    return {
      ok: false,
      reason: 'not-confirmed',
      message: 'rebuilding the database needs the environment name typed in',
    }
  }

  const resolved = { ...request, stopServices }
  const job = {
    id: `${kind}-${Date.now()}`,
    kind,
    label: spec.label(resolved),
    root,
    running: true,
    code: null,
    startedAt: new Date().toISOString(),
    finishedAt: null,
    steps: [],
    lines: [`$ compass ${spec.steps(resolved).map((args) => args.join(' ')).join('  &&  compass ')}`],
    pending: '',
  }
  jobs.set(root, job)

  const sequence = spec.steps(resolved)
  void (async () => {
    let code = 0
    for (const args of sequence) {
      code = await runStep(root, args, job, spec.timeoutMs)
      if (code !== 0) break
    }
    job.code = code
    job.running = false
    job.finishedAt = new Date().toISOString()
    if (typeof onSettled === 'function') onSettled(job)
  })()

  return { ok: true, job: jobView(job) }
}

/** Test seam: drop every job, so one test cannot see another's. */
export function resetJobs() {
  jobs.clear()
}

function git(args) {
  return new Promise((resolve) => {
    const child = spawn('git', args)
    let stdout = ''
    child.stdout.on('data', (chunk) => { stdout += chunk })
    child.on('error', () => resolve(null))
    child.on('close', (code) => resolve(code === 0 ? stdout : null))
  })
}

/**
 * The work tree root that holds `candidate`, or null.
 *
 * A directory resolves only when git finds a top level for it and that top
 * level is an ORE Studio checkout: the root `compass.sh` and the compass
 * project beside it are what establish that. The DSH server's own working
 * directory is never a candidate, because dsh-web.service starts from the
 * user's home, so a fallback would answer for a directory that is not a
 * checkout at all.
 */
export async function resolveWorktree(candidate) {
  if (typeof candidate !== 'string' || candidate === '') return null
  const top = await git(['-C', candidate, 'rev-parse', '--show-toplevel'])
  if (top === null) return null
  const root = top.trim()
  if (root === '') return null
  try {
    await Promise.all([
      fs.access(join(root, 'compass.sh')),
      fs.access(join(root, 'projects', 'ores.compass', 'compass.sh')),
    ])
  } catch {
    return null
  }
  return root
}
