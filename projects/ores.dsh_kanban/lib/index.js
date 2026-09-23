// ores-dsh-kanban host half: one read-only route that renders one work tree's
// current sprint from the org tree on disk. See CONTRACT.md for the frozen
// interface.

import { promises as fs } from 'node:fs'
import { join } from 'node:path'
import { buildModel, failure, parseSprintDocument } from './agile.js'
import { listWorkTrees, readJournals, readWorkTree } from './git.js'

export const name = 'ores-dsh-kanban'

export const inject = ['webServer', 'sessions']

const ROUTE = '/plugins/ores-dsh-kanban/state'
const AGILE_ROOT = 'doc/agile/versions'
const CACHE_TTL_MS = 3000

export function apply(ctx) {
  const log = (message) => console.log('ores-dsh-kanban: ' + message)

  // A board costs roughly 550 file reads, and a session refetches on every mount
  // and every Refresh. An entry expires on the clock alone: a hit older than the
  // TTL is a miss, and a miss behaves exactly as an uncached request.
  const cache = new Map()

  const sweep = (now) => {
    for (const [key, entry] of cache) {
      if (now - entry.at >= CACHE_TTL_MS) cache.delete(key)
    }
  }

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
      const asked = query.get('session') ?? ''
      const sessionCwd = asked ? ctx.sessions?.get(asked)?.header?.cwd : undefined
      const fromCwd = query.get('cwd') ?? ''
      const now = Date.now()
      sweep(now)

      // A board is only ever read for a work tree of this repository, so the
      // cache is keyed by the resolved root and not by whatever cwd was asked
      // for. Several sessions inside one tree then share one entry.
      const key = await resolveRoot(fromCwd, sessionCwd)
      if (key) {
        const hit = cache.get(key.root)
        if (hit) return json(res, hit.value)
      }

      const value = key ? await buildState(key.root, key.source) : await describeMiss(asked)
      if (value.ok && key) cache.set(key.root, { at: now, value })
      return json(res, value)
    } catch (err) {
      log('state route failed: ' + ((err && err.stack) || err))
      return json(res, failure('not-an-agile-tree', String((err && err.message) || err)))
    }
  }

  // The ladder: the session's own directory first, then the cwd the client sends,
  // each accepted only when it is a work tree root of this repository. The
  // process working directory is never a rung: the shipped unit runs the server
  // from the user's home, so that would answer for the wrong tree.
  const resolveRoot = async (fromCwd, sessionCwd) => {
    const trees = await listWorkTrees(fromCwd || sessionCwd)
    if (trees === null) return null
    const at = (path) =>
      typeof path === 'string' && path !== ''
        ? trees.find((tree) => path === tree.root || path.startsWith(tree.root + '/')) ?? null
        : null
    const session = at(sessionCwd)
    if (session) return { ...session, source: 'session' }
    const query = at(fromCwd)
    if (query) return { ...query, source: 'cwd' }
    return null
  }

  const describeMiss = async (asked) => {
    if (asked) return failure('unknown-session', `session ${asked} is not in a work tree of this repository`)
    return failure('not-an-agile-tree', 'no session and no cwd resolved to a work tree root')
  }

  const buildState = async (root, source) => {
    const versions = join(root, AGILE_ROOT)
    if (!(await isDirectory(versions))) {
      return failure('not-an-agile-tree', `${AGILE_ROOT} not found under ${root}`)
    }

    const sprint = await findSprint(join(root, AGILE_ROOT))
    if (!sprint) {
      return failure('no-sprint', `no sprint_NN directory under ${join(root, AGILE_ROOT)}`)
    }

    // One request, one pass: the sprint's documents are read once each here and
    // never re-read downstream.
    const [sprintText, dirs, trees] = await Promise.all([
      readText(sprint.path),
      readStoryDirs(sprint.path),
      listWorkTrees(root),
    ])
    if (sprintText === null) {
      return failure('no-sprint', `${join(AGILE_ROOT, sprint.name, 'sprint.org')} is unreadable`)
    }
    if (trees === null) return failure('git-unavailable', `git worktree list failed under ${root}`)

    const doc = parseSprintDocument(sprintText)
    const entries = await readJournals(trees.map((tree) => tree.root))
    const withEntries = trees.map((tree) => ({ ...tree, entry: entries.get(tree.root) ?? null }))
    const selected = withEntries.find((tree) => tree.root === root)
    if (!selected) return failure('not-an-agile-tree', `${root} is not a work tree of this repository`)

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
          tree: await readWorkTree(selected),
          source,
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

// The sprint is the newest sprint_NN by directory name, which is the ordering the
// repository documents. Nothing reads a sprint.org to decide that, and the winner
// is read once by the caller.
async function findSprint(versions) {
  if (!(await isDirectory(versions))) return null
  const versionNames = (await readDirNames(versions))
    .filter((entry) => entry.isDirectory())
    .map((entry) => entry.name)
    .sort()
    .reverse()
  for (const version of versionNames) {
    const sprintNames = (await readDirNames(join(versions, version)))
      .filter((entry) => entry.isDirectory() && /^sprint_\d+$/.test(entry.name))
      .map((entry) => entry.name)
      .sort()
      .reverse()
    if (sprintNames.length === 0) continue
    return {
      version,
      name: sprintNames[0],
      path: join(versions, version, sprintNames[0], 'sprint.org'),
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
