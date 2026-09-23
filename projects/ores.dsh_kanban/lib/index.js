// ores-dsh-kanban host half: one read-only route that renders one work tree's
// current sprint from the org tree on disk. See CONTRACT.md for the frozen
// interface.

import { promises as fs } from 'node:fs'
import { join } from 'node:path'
import { buildModel, failure, parseSprintDocument } from './agile.js'
import { readJournals, readWorkTrees } from './git.js'

export const name = 'ores-dsh-kanban'

export const inject = ['webServer', 'sessions']

const ROUTE = '/plugins/ores-dsh-kanban/state'
const AGILE_ROOT = 'doc/agile/versions'
const CACHE_TTL_MS = 3000

export function apply(ctx) {
  const log = (message) => console.log('ores-dsh-kanban: ' + message)

  // A board costs roughly 550 file reads, and a session refetches on every mount
  // and every Refresh. The entry expires on the clock alone: a hit older than the
  // TTL is a miss, and a miss behaves exactly as an uncached request.
  const cache = new Map()

  const json = (res, value) => {
    const body = JSON.stringify(value)
    res.writeHead(200, {
      'content-type': 'application/json; charset=utf-8',
      'cache-control': 'no-store',
      'content-length': Buffer.byteLength(body),
    })
    res.end(body)
  }

  const handler = async (req, res) => {
    if (req.method !== 'GET' && req.method !== 'HEAD') {
      return json(res, failure('not-an-agile-tree', 'Only GET and HEAD are supported'))
    }
    try {
      const query = new URL(req.url, 'http://127.0.0.1').searchParams
      const sessionId = query.get('session') ?? ''
      const sessionCwd = sessionId ? ctx.sessions?.get(sessionId)?.header?.cwd : undefined
      const cwd =
        typeof sessionCwd === 'string' && sessionCwd ? sessionCwd : query.get('cwd') || process.cwd()
      const hit = cache.get(cwd)
      const now = Date.now()
      if (hit && now - hit.at < CACHE_TTL_MS) return json(res, hit.value)
      const value = await buildState(cwd)
      if (value.ok) cache.set(cwd, { at: now, value })
      else cache.delete(cwd)
      return json(res, value)
    } catch (err) {
      log('state route failed: ' + ((err && err.stack) || err))
      return json(res, failure('not-an-agile-tree', String((err && err.message) || err)))
    }
  }

  const buildState = async (cwd) => {
    const versions = join(cwd, AGILE_ROOT)
    if (!(await isDirectory(versions))) {
      return failure('not-an-agile-tree', `${AGILE_ROOT} not found under ${cwd}`)
    }

    const workTrees = await readWorkTrees(cwd)
    if (workTrees === null) {
      return failure('git-unavailable', `git worktree list failed under ${cwd}`)
    }
    const root =
      workTrees.find((tree) => cwd === tree.root || cwd.startsWith(tree.root + '/'))?.root ?? cwd

    const sprint = await findSprint(join(root, AGILE_ROOT))
    if (!sprint) {
      return failure('no-sprint', `no sprint_NN directory under ${join(root, AGILE_ROOT)}`)
    }

    // One request, one pass: the sprint's documents are read once each here and
    // never re-read downstream.
    const [sprintText, dirs] = await Promise.all([
      readText(sprint.path),
      readStoryDirs(sprint.path),
    ])
    if (sprintText === null) {
      return failure('no-sprint', `${join(AGILE_ROOT, sprint.name, 'sprint.org')} is unreadable`)
    }
    const doc = parseSprintDocument(sprintText)
    const entries = await readJournals(workTrees.map((tree) => tree.root))
    const withEntries = workTrees.map((tree) => ({ ...tree, entry: entries.get(tree.root) ?? null }))

    return {
      ok: true,
      generatedAt: new Date().toISOString(),
      ...buildModel({
        doc,
        dirs,
        options: {
          version: sprint.version,
          sprintName: sprint.name,
          sprintPath: `${AGILE_ROOT}/${sprint.version}/${sprint.name}/sprint.org`,
          today: new Date().toISOString().slice(0, 10),
          tree: withEntries.find((tree) => tree.root === root) ?? null,
          trees: withEntries,
        },
      }),
    }
  }

  const route = { registered: false, dispose: null, timer: null, disposed: false }
  const register = () => {
    if (route.registered || route.disposed) return
    const webServer = ctx.get('webServer')
    if (!webServer) return
    try {
      const dispose = webServer.register({ kind: 'exact', path: ROUTE, handler })
      route.dispose = typeof dispose === 'function' ? dispose : null
      route.registered = true
      log(`${ROUTE} registered`)
    } catch (err) {
      log('route registration failed: ' + ((err && err.message) || err))
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
  }, 'ores-dsh-kanban: state route')
}

async function findSprint(versions) {
  if (!(await isDirectory(versions))) return null
  const versionNames = (await readDirNames(versions))
    .filter((entry) => entry.isDirectory())
    .map((entry) => entry.name)
  for (const version of versionNames.sort().reverse()) {
    const sprintNames = (await readDirNames(join(versions, version)))
      .filter((entry) => entry.isDirectory() && /^sprint_\d+$/.test(entry.name))
      .map((entry) => entry.name)
    for (const name of sprintNames.sort().reverse()) {
      const path = join(versions, version, name, 'sprint.org')
      const text = await readText(path)
      if (text === null) continue
      return { version, name, path, doc: parseSprintDocument(text) }
    }
  }
  return null
}

async function readStoryDirs(sprintPath) {
  const sprintDir = sprintPath.replace(/\/sprint\.org$/, '')
  const dirs = []
  for (const entry of await readDirNames(sprintDir)) {
    if (!entry.isDirectory() || entry.name.startsWith('.')) continue
    const dir = join(sprintDir, entry.name)
    const story = await readText(join(dir, 'story.org'))
    if (story === null) continue
    const tasks = []
    for (const task of await readDirNames(dir)) {
      const match = /^task_(.+)\.org$/.exec(task.name)
      if (!task.isFile() || !match) continue
      const text = await readText(join(dir, task.name))
      if (text !== null) tasks.push({ slug: match[1], text })
    }
    dirs.push({ slug: entry.name, story, tasks })
  }
  return dirs
}

async function readDirNames(dir) {
  try {
    return await fs.readdir(dir, { withFileTypes: true })
  } catch {
    return []
  }
}

async function readText(path) {
  try {
    return await fs.readFile(path, 'utf8')
  } catch {
    return null
  }
}

async function isDirectory(path) {
  try {
    return (await fs.stat(path)).isDirectory()
  } catch {
    return false
  }
}
