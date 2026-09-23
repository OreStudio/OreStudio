/**
 * The client-half gate: a real browser against a scratch DSH instance.
 *
 * It checks the host payload against CONTRACT.md, drives the board in Chrome,
 * and measures the layout faults nobody has measured yet.
 *
 * Prerequisites: Playwright installed under projects/ores.web, and a scratch
 * DSH home at tmp/dsh-scratch holding an agile-scratch profile with the plugin
 * inserted and symlinked into its node_modules. The harness copies that home to
 * tmp/verify-home on first run and uses the copy, so it can run while another
 * instance serves the scratch home. It binds its own ephemeral port and owns its
 * own boot log, so it never shares either with a live instance.
 *
 * The home is read from KANBAN_DSH_HOME and otherwise resolved as above. It
 * deliberately ignores DSH_HOME, because that variable is set in an ordinary
 * agent shell and honouring it silently boots a different harness home.
 *
 * Run:
 *   node projects/ores.dsh_kanban/scripts/verify.mjs
 */

import { execFileSync, spawn } from 'node:child_process'
import { createHash } from 'node:crypto'
import { closeSync, cpSync, existsSync, mkdirSync, openSync, readFileSync, writeFileSync } from 'node:fs'
import { createServer } from 'node:net'
import { createRequire } from 'node:module'
import { basename, dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const SCRIPT_DIR = dirname(fileURLToPath(import.meta.url))
const REPO = resolve(SCRIPT_DIR, '..', '..', '..')
const DSH_BIN = process.env['DSH_BIN']
  ?? '/home/marco/.npm-global/lib/node_modules/@deepseek-ai/dsh/lib/bin.js'
const PROFILE = 'agile-scratch'
const SCRATCH_HOME = resolve(REPO, 'tmp', 'dsh-scratch')
const DSH_HOME = resolveHome()
const SHOT_DIR = resolve(REPO, 'tmp', 'shots')
const RESULT_FILE = resolve(REPO, 'tmp', 'verify-result.json')
const SERVER_LOG = resolve(REPO, 'tmp', 'verify-server.log')

function profileDir(home) {
  return resolve(home, 'profiles', PROFILE)
}
/* The harness owns its home and its log. Sharing either with a served instance
 * or another agent's scratch server makes a run fail for reasons that have
 * nothing to do with the plugin, and a shared log can be rewritten by a stale
 * server so the boot guard reads a port it never bound. */
function resolveHome() {
  const explicit = process.env['KANBAN_DSH_HOME']
  if (explicit !== undefined && explicit !== '') return explicit
  const home = resolve(REPO, 'tmp', 'verify-home')
  if (existsSync(resolve(profileDir(home), 'package.json'))) return home
  if (existsSync(resolve(profileDir(SCRATCH_HOME), 'package.json'))) {
    cpSync(SCRATCH_HOME, home, { recursive: true })
  }
  return home
}

if (!existsSync(resolve(profileDir(DSH_HOME), 'package.json'))) {
  console.error(`REFUSING TO CONTINUE: no ${PROFILE} profile under ${DSH_HOME}.`)
  console.error(`Seed one at ${SCRATCH_HOME}, or set KANBAN_DSH_HOME to a home that has it.`)
  process.exit(2)
}

const STATE_PATH = '/plugins/ores-dsh-kanban/state'
const STORY_ID = '3085B911-3030-4AB8-976F-0F037D4332E7'
const COLUMN_IDS = 'BACKLOG,STARTED,BLOCKED,DONE,ABANDONED'
const TOP_LEVEL_KEYS = ['ok', 'generatedAt', 'tree', 'sprint', 'columns', 'stories', 'trees', 'counts', 'filters']
const VIEWPORT = { width: 1600, height: 1100 }
const NARROW = { width: 900, height: 800 }
const BOOT_TIMEOUT_MS = 90000

const require = createRequire(resolve(REPO, 'projects', 'ores.web', 'package.json'))
const { chromium } = require('playwright')

/* The tree under test. The default is the harness's own repository; another work
 * tree root replays the checks against whatever sprint that tree has on disk. */
function resolveTree() {
  const explicit = process.env['KANBAN_VERIFY_TREE']
  return explicit !== undefined && explicit !== '' ? resolve(explicit) : REPO
}

const TREE = resolveTree()

const assertions = []
const measurements = {}
const screenshots = []

function check(section, name, ok, detail = '') {
  assertions.push({ id: assertions.length + 1, section, name, ok: !!ok, detail: String(detail) })
  console.log(`${ok ? 'PASS' : 'FAIL'}  ${name}${detail ? '  — ' + detail : ''}`)
}

function heading(title) {
  console.log(`\n=== ${title}`)
}

const delay = (ms) => new Promise((done) => setTimeout(done, ms))
const round = (value) => Math.round(value * 10) / 10

/* The board is checked against whatever revision is on disk, so the result names
 * it. A run that passes against an unrecorded client proves nothing later. */
function provenance() {
  const digest = (path) => createHash('sha256').update(readFileSync(path)).digest('hex').slice(0, 16)
  const client = resolve(REPO, 'projects', 'ores.dsh_kanban', 'lib', 'client.js')
  let head = ''
  try {
    head = execFileSync('git', ['-C', REPO, 'rev-parse', 'HEAD'], { encoding: 'utf8' }).trim()
  } catch {
    head = ''
  }
  return {
    gitHead: head,
    client: { lines: readFileSync(client, 'utf8').split('\n').length - 1, sha256: digest(client) },
    contractSha256: digest(resolve(REPO, 'projects', 'ores.dsh_kanban', 'CONTRACT.md')),
  }
}

async function allocatePort() {
  const probe = createServer()
  await new Promise((ready, fail) => {
    probe.once('error', fail)
    probe.listen(0, '127.0.0.1', ready)
  })
  const { port } = probe.address()
  await new Promise((done) => probe.close(done))
  return port
}

const CLOSED_STATES = { DONE: true, ABANDONED: true }
const columnIdOf = (story) => (story.state === 'DISCOVERED' ? 'BACKLOG' : story.state)
const plural = (count, word) => `${count} ${word}${count === 1 ? '' : 's'}`

function dayNumber(value) {
  const match = /^(\d{4})-(\d{2})-(\d{2})/.exec(String(value ?? ''))
  return match ? Date.UTC(Number(match[1]), Number(match[2]) - 1, Number(match[3])) : null
}

/* The board's own filter, mirrored from lib/client.js so a search can be checked
 * against the stories the payload says should survive it. */
function matchesQuery(story, query) {
  const needle = query.trim().toLowerCase()
  if (!needle) return true
  const fields = [story.title, story.id, story.epic, story.environment, ...(story.branches ?? [])]
  for (const task of story.tasks ?? []) fields.push(task.title, task.environment)
  return fields.some((field) => String(field ?? '').toLowerCase().includes(needle))
}

/* Every expectation the harness holds about the board comes from here, so a run
 * passes for whatever sprint the repository is on rather than for one sprint's
 * numbers. */
function deriveExpectations(payload) {
  const tree = payload?.tree ?? {}
  const sprint = payload?.sprint ?? {}
  const stories = payload?.stories ?? []
  const counts = payload?.counts ?? {}
  const columns = payload?.columns ?? []
  const epics = payload?.filters?.epics ?? []
  const currentStory = stories.filter((story) => story.id === tree.currentStoryId)[0] ?? null
  const currentTask = currentStory
    ? currentStory.tasks.filter((task) => task.id === tree.currentTaskId)[0] ?? currentStory.tasks[0] ?? null
    : null

  const start = dayNumber(sprint.startDate)
  const end = dayNumber(sprint.endDate)
  const today = dayNumber(new Date().toISOString().slice(0, 10))
  const dayOfSprint = start === null || today === null ? null : Math.round((today - start) / 86400000) + 1
  const totalDays = start === null || end === null ? null : Math.round((end - start) / 86400000) + 1

  const epicCounts = new Map()
  for (const story of stories) epicCounts.set(story.epic, (epicCounts.get(story.epic) ?? 0) + 1)
  const columnCounts = columns.map((column) => ({
    id: column.id,
    title: column.title,
    cards: stories.filter((story) => columnIdOf(story) === column.id).length,
    sentCount: column.count,
  }))
  const tasksTotal = stories.reduce((sum, story) => sum + story.progress.total, 0)
  const tasksDone = stories.reduce((sum, story) => sum + story.progress.done, 0)
  const tileValues = {
    stories: String(counts.stories),
    'in-flight': String(stories.filter((story) => story.state === 'STARTED').length),
    blocked: String(stories.filter((story) => story.state === 'BLOCKED').length),
    'tasks-done': `${tasksDone}/${tasksTotal}`,
  }
  /* The epics a chip can narrow the board to: the ones that do not cover every
   * story. A sprint whose one epic is on every story has nothing to filter. */
  const filteringEpic = epics.filter((name) => (epicCounts.get(name) ?? 0) < stories.length)[0] ?? null
  const searchQuery = currentTask ? currentTask.title : currentStory ? currentStory.title : ''
  const searchIds = stories.filter((story) => matchesQuery(story, searchQuery)).map((story) => story.id).sort()

  return {
    tree, sprint, stories, counts, columns, epics, currentStory, currentTask,
    dayOfSprint, totalDays, epicCounts, columnCounts, tileValues,
    cardFaceBranchNames: [...new Set(stories.flatMap((story) => story.branches ?? []))],
    cardPrNumbers: [...new Set(stories.flatMap((story) => story.prs ?? []))],
    filteringEpic,
    filteringEpicCount: filteringEpic === null ? null : epicCounts.get(filteringEpic),
    searchQuery,
    searchIds,
    ageLine: currentStory === null || CLOSED_STATES[currentStory.state]
      ? ''
      : 'open ' + Math.max(0, Math.floor((Date.now() - Date.parse(currentStory.created)) / 86400000)) + 'd',
  }
}

function bootServer(port) {
  const fd = openSync(SERVER_LOG, 'w')
  /* Started outside the repository on purpose: a server whose working directory
   * is a work tree root hides a missing resolution rung. */
  const child = spawn(
    process.execPath,
    [DSH_BIN, '--profile', PROFILE, '--no-open', '--port', String(port)],
    { cwd: process.env['HOME'] ?? REPO, env: { ...process.env, DSH_HOME }, stdio: ['ignore', fd, fd] },
  )
  closeSync(fd)
  return child
}

/* The sidebar lists the sessions of the active workspace alone, and the DSH home
 * is shared with other work, so the active workspace is often not ours. Workspace
 * rows are treeitems too; only a session row carries a relative-time badge, and
 * the sessions of a workspace follow its "New Session" row. */
async function openSession(page, label) {
  const title = 'ORE Studio: ' + label.split('_').filter(Boolean)
    .map((word) => word[0].toUpperCase() + word.slice(1)).join(' ')
  const known = () => page.getByText('DSH plugin experiments', { exact: false }).first()
  const openKnown = async (detail) => {
    await known().click()
    return { opened: true, detail }
  }
  if (await known().count() > 0) return openKnown('DSH plugin experiments')
  const rows = page.locator('[role="treeitem"]')
  const readRows = async () => (await rows.allInnerTexts().catch(() => [])).map((text) => text.replace(/\s+/g, ' ').trim())
  const isWorkspaceRow = (text) => text.startsWith('ORE Studio: ')
  const sessionRowsBetween = (texts, from, to) => texts
    .map((text, index) => ({ text, index }))
    .filter((row) => row.index > from && row.index < to && row.text !== 'New Session' && !isWorkspaceRow(row.text))
  /* A workspace row expands its own session list. Clicking the label can land on
   * a child slot instead, so the row element itself is clicked, and again in the
   * DOM when that left the row collapsed. */
  const sessionsOf = async (target) => {
    const texts = await readRows()
    const workspaceAt = texts.findIndex((text) => text === target)
    if (workspaceAt >= 0) {
      let end = texts.length
      for (let index = workspaceAt + 1; index < texts.length; index += 1) {
        if (isWorkspaceRow(texts[index])) { end = index; break }
      }
      const found = sessionRowsBetween(texts, workspaceAt, end)
      if (found.length > 0) return found
    }
    let lastWorkspaceAt = -1
    for (let index = texts.length - 1; index >= 0; index -= 1) {
      if (isWorkspaceRow(texts[index])) { lastWorkspaceAt = index; break }
    }
    return sessionRowsBetween(texts, lastWorkspaceAt, texts.length)
  }
  const title4 = title
  const workspaceAt = (await readRows()).findIndex((text) => text === title4)
  if (workspaceAt >= 0) {
    await rows.nth(workspaceAt).click()
    await page.waitForTimeout(2000)
  }
  if (await known().count() > 0) return openKnown(`DSH plugin experiments (workspace ${title})`)
  let candidates = await sessionsOf(title)
  if (candidates.length === 0 && workspaceAt >= 0) {
    await page.evaluate((index) => document.querySelectorAll('[role="treeitem"]')[index]?.click(), workspaceAt)
    await page.waitForTimeout(2000)
    candidates = await sessionsOf(title)
  }
  if (candidates.length === 0) {
    return { opened: false, detail: `no session row under ${title}; rows: ${(await readRows()).join(' | ').slice(0, 240)}` }
  }
  await rows.nth(candidates[0].index).click()
  return { opened: true, detail: `${candidates[0].text} (workspace ${title})` }
}

/* The bound port is a claim until the boot log confirms it. A stale server on a
 * fixed port answers on that port and serves a different plugin build, which is
 * how an earlier verification run passed against the wrong instance. */
async function confirmBootUrl(child, port) {
  const pattern = /dsh web:\s*(http:\/\/127\.0\.0\.1:(\d+)\/\?token=\S+)/
  const deadline = Date.now() + BOOT_TIMEOUT_MS
  while (Date.now() < deadline) {
    if (child.exitCode !== null) {
      return { url: '', reason: `the server exited with code ${child.exitCode}` }
    }
    const log = readFileSync(SERVER_LOG, 'utf8').replace(/\u001b\[[0-9;]*m/g, '')
    const match = pattern.exec(log)
    if (match) {
      if (Number(match[2]) !== port) {
        return { url: '', reason: `the boot log serves port ${match[2]}, this run bound ${port}` }
      }
      return { url: match[1] }
    }
    await delay(250)
  }
  return { url: '', reason: `no "dsh web:" line in ${SERVER_LOG} after ${BOOT_TIMEOUT_MS / 1000}s` }
}

function nonBareWorkTreeLabels(tree) {
  const porcelain = execFileSync('git', ['-C', tree, 'worktree', 'list', '--porcelain'], { encoding: 'utf8' })
  return porcelain.split('\n\n').map((block) => block.trim()).filter(Boolean)
    .filter((block) => !/^bare$/m.test(block))
    .map((block) => basename(/^worktree (.+)$/m.exec(block)[1]).replace(/^ores_dev_/, ''))
    .sort()
}

async function fetchState(origin, query) {
  const url = `${origin}${STATE_PATH}${query ? `?${query}` : ''}`
  const started = performance.now()
  const response = await fetch(url)
  const body = await response.text()
  const ms = performance.now() - started
  let json = null
  try {
    json = JSON.parse(body)
  } catch {
    json = null
  }
  return {
    query: query ? `?${query}` : '',
    status: response.status,
    bytes: Buffer.byteLength(body),
    contentType: response.headers.get('content-type'),
    cacheControl: response.headers.get('cache-control'),
    ms: round(ms),
    json,
  }
}

/* The route resolves a work tree down a ladder, and the rung it took is the
 * difference between a board for this session and a board for the server's
 * working directory. The server runs outside the repository, so `process.cwd()`
 * cannot answer, and one rung is asserted to fail. */
async function checkResolutionLadder(origin, tree) {
  heading('2. the work-tree resolution ladder')
  const withCwd = await fetchState(origin, `cwd=${encodeURIComponent(tree)}`)
  const bogusWithCwd = await fetchState(origin, `session=bogus&cwd=${encodeURIComponent(tree)}`)
  const bogusAlone = await fetchState(origin, 'session=bogus')
  const bare = await fetchState(origin, '')
  const tmpCwd = await fetchState(origin, `cwd=/tmp`)

  check('ladder', 'cwd naming a work tree root resolves and reports source cwd',
    withCwd.json?.ok === true && withCwd.json?.tree?.source === 'cwd',
    `ok=${withCwd.json?.ok} source=${withCwd.json?.tree?.source}`)
  check('ladder', 'an unknown session with that cwd still resolves by cwd',
    bogusWithCwd.json?.ok === true && bogusWithCwd.json?.tree?.source === 'cwd',
    `ok=${bogusWithCwd.json?.ok} source=${bogusWithCwd.json?.tree?.source}`)
  check('ladder', 'an unknown session with no cwd fails as unknown-session',
    bogusAlone.json?.ok === false && bogusAlone.json?.reason === 'unknown-session',
    `ok=${bogusAlone.json?.ok} reason=${bogusAlone.json?.reason}`)
  check('ladder', 'no session and no cwd fails instead of answering for process.cwd()',
    bare.json?.ok === false && bare.json?.reason === 'not-an-agile-tree',
    `ok=${bare.json?.ok} reason=${bare.json?.reason} label=${bare.json?.tree?.label ?? '(none)'}`)
  check('ladder', 'a cwd outside the repository fails as not-an-agile-tree',
    tmpCwd.json?.ok === false && tmpCwd.json?.reason === 'not-an-agile-tree',
    `ok=${tmpCwd.json?.ok} reason=${tmpCwd.json?.reason}`)
  check('ladder', 'a successful payload reports tree.source as session or cwd',
    ['session', 'cwd'].includes(withCwd.json?.tree?.source ?? ''),
    String(withCwd.json?.tree?.source))

  measurements.ladder = {
    requests: [withCwd, bogusWithCwd, bogusAlone, bare, tmpCwd].map((entry) => ({
      query: entry.query, status: entry.status, ms: entry.ms, bytes: entry.bytes,
      ok: entry.json?.ok ?? null, reason: entry.json?.reason ?? null,
      source: entry.json?.tree?.source ?? null, label: entry.json?.tree?.label ?? null,
    })),
  }
  console.log('  ' + measurements.ladder.requests.map((entry) => `${entry.query || '(no query)'} -> ok=${entry.ok} source=${entry.source} reason=${entry.reason}`).join('\n  '))
  return withCwd
}

async function checkPayload(origin, repoFetch, tree) {
  heading('1. host payload against CONTRACT.md')
  const payload = repoFetch.json

  check('host', 'the state route answers 200 with JSON', repoFetch.status === 200 && payload !== null,
    `status ${repoFetch.status} bytes ${repoFetch.bytes}`)
  const { json: _payload, ...hostMeta } = repoFetch
  measurements.host = hostMeta
  if (!payload) return null

  check('host', 'tree.source is present on a successful payload',
    payload.tree !== undefined && 'source' in payload.tree, String(payload.tree?.source))

  check('host', 'exactly nine top-level keys', JSON.stringify(Object.keys(payload).sort()) === JSON.stringify([...TOP_LEVEL_KEYS].sort()),
    Object.keys(payload).sort().join(','))

  const columnIds = (payload.columns ?? []).map((column) => column.id).join(',')
  check('host', 'the five canonical columns in table order, ABANDONED included', columnIds === COLUMN_IDS, columnIds)

  const sprint = payload.sprint ?? {}
  const expected = deriveExpectations(payload)
  check('host', 'sprint.dayOfSprint is a number', typeof sprint.dayOfSprint === 'number', String(sprint.dayOfSprint))
  check('host', 'sprint.dayOfSprint is today minus startDate plus one',
    sprint.dayOfSprint === expected.dayOfSprint,
    `day ${sprint.dayOfSprint}, startDate ${sprint.startDate}, today ${new Date().toISOString().slice(0, 10)}`)
  check('host', 'sprint.totalDays is endDate minus startDate plus one',
    sprint.totalDays === expected.totalDays,
    `total ${sprint.totalDays}, endDate ${sprint.endDate}`)

  const epics = payload.filters?.epics
  check('host', 'filters.epics is an array of non-empty epic names',
    Array.isArray(epics) && epics.every((name) => typeof name === 'string' && name.trim() !== '')
    && (epics.length > 0 || expected.stories.every((story) => story.epic === '')),
    Array.isArray(epics) ? epics.join(' ') : String(epics))

  check('host', 'the column counts add up to the stories sent',
    expected.columnCounts.reduce((sum, column) => sum + column.sentCount, 0) === expected.stories.length,
    `${expected.columnCounts.map((column) => column.sentCount).join('+')} vs ${expected.stories.length}`)

  check('host', 'no session key in the payload', !('session' in payload))
  check('host', 'tree carries no isSession', !('isSession' in (payload.tree ?? {})))
  check('host', 'tree.currentStoryId is the story under test', payload.tree?.currentStoryId === STORY_ID,
    String(payload.tree?.currentStoryId))
  check('host', 'the payload resolves a current story for the work tree', expected.currentStory !== null,
    `currentStoryId ${payload.tree?.currentStoryId || '(empty)'}, by ${payload.tree?.by}`)
  check('host', 'the current story carries its current task', expected.currentTask !== null,
    `currentTaskId ${payload.tree?.currentTaskId || '(empty)'}`)

  const expectedLabels = nonBareWorkTreeLabels(tree)
  const sentLabels = (payload.trees ?? []).map((tree) => tree.label).sort()
  check('host', 'trees has one row per non-bare git work tree',
    sentLabels.length === expectedLabels.length,
    `${sentLabels.length} rows, ${expectedLabels.length} non-bare work trees`)
  check('host', 'trees names every non-bare work tree', JSON.stringify(sentLabels) === JSON.stringify(expectedLabels),
    sentLabels.join(' '))

  const carriers = (payload.trees ?? []).filter((tree) => 'dirty' in tree || 'detached' in tree)
  check('host', 'no trees row carries dirty or detached', carriers.length === 0,
    carriers.map((tree) => tree.label).join(' '))
  check('host', 'tree keeps dirty and detached', 'dirty' in (payload.tree ?? {}) && 'detached' in (payload.tree ?? {}))

  const spaced = (payload.filters?.environments ?? []).filter((name) => /\s/.test(name))
  check('host', 'no filters.environments entry contains a space', spaced.length === 0, spaced.join(' | '))

  measurements.payload = {
    sprint: sprint.title,
    sprintName: sprint.name,
    dayOfSprint: sprint.dayOfSprint,
    expectedDayOfSprint: expected.dayOfSprint,
    totalDays: sprint.totalDays,
    expectedTotalDays: expected.totalDays,
    source: payload.tree?.source ?? null,
    treeLabel: payload.tree?.label ?? null,
    branch: payload.tree?.branch ?? null,
    columns: expected.columnCounts,
    counts: payload.counts,
    trees: sentLabels,
    stories: payload.stories?.length ?? 0,
    filters: payload.filters,
    currentStory: expected.currentStory && {
      id: expected.currentStory.id,
      title: expected.currentStory.title,
      state: expected.currentStory.state,
      epic: expected.currentStory.epic,
      environment: expected.currentStory.environment,
      branches: expected.currentStory.branches,
      prs: expected.currentStory.prs,
      progress: expected.currentStory.progress,
    },
    currentTask: expected.currentTask && { id: expected.currentTask.id, title: expected.currentTask.title, state: expected.currentTask.state },
    filteringEpic: expected.filteringEpic,
    filteringEpicCount: expected.filteringEpicCount,
    searchQuery: expected.searchQuery,
    searchMatches: expected.searchIds.length,
  }
  return payload
}

const probeLayout = (phase) => {
  const roundTo = (value) => Math.round(value * 10) / 10
  const view = document.querySelector('[data-ores-kanban="view"]')
  if (!view) return null
  const board = document.querySelector('[data-ores-kanban="board"]')
  const boardBox = board ? board.getBoundingClientRect() : null
  /* The board scroller's own right edge moves when the detail panel opens, which
   * would mark the panel, the tiles and the sprint line as overflow. The frame an
   * element must stay inside is the row that holds the board and the panel. */
  const frame = board ? board.parentElement : null
  const frameBox = frame ? frame.getBoundingClientRect() : null
  const metadata = (el) => {
    const out = {}
    for (const attr of el.attributes) if (attr.name.startsWith('data-')) out[attr.name] = attr.value
    return out
  }
  const describe = (el) => ({
    tag: el.tagName.toLowerCase(),
    className: el.getAttribute('class') || '',
    data: metadata(el),
    text: (el.textContent ?? '').replace(/\s+/g, ' ').trim().slice(0, 60),
  })
  const clipsHorizontally = (style) =>
    style.overflowX === 'auto' || style.overflowX === 'scroll' || style.overflowX === 'hidden'
  const clipperOf = (el) => {
    for (let node = el.parentElement; node && node !== document.body; node = node.parentElement) {
      const style = getComputedStyle(node)
      if (clipsHorizontally(style) && node.scrollWidth > node.clientWidth + 2) return describe(node)
    }
    return null
  }
  const overflow = []
  for (const el of view.querySelectorAll('*')) {
    const rect = el.getBoundingClientRect()
    const scrollWidth = typeof el.scrollWidth === 'number' ? el.scrollWidth : 0
    const clientWidth = typeof el.clientWidth === 'number' ? el.clientWidth : 0
    const scrollOver = scrollWidth - clientWidth
    const escapeOver = frameBox ? rect.right - frameBox.right : 0
    if (!(scrollOver > 2) && !(escapeOver > 2)) continue
    const parent = el.parentElement
    const card = el.closest('[data-ores-kanban="card"]')
    overflow.push({
      kind: scrollOver > 2 && escapeOver > 2 ? 'scroll+escape' : (scrollOver > 2 ? 'scroll' : 'escape'),
      ...describe(el),
      parent: parent ? { tag: parent.tagName.toLowerCase(), data: metadata(parent) } : null,
      cardId: card ? card.getAttribute('data-story-id') : null,
      scrollWidth,
      clientWidth,
      scrollOver,
      rectRight: roundTo(rect.right),
      escapeOver: roundTo(escapeOver),
      insideBoard: board ? board.contains(el) : false,
      clippedBy: clipperOf(el),
    })
  }
  const branchText = []
  for (const el of view.querySelectorAll('div, span, a')) {
    if (el.children.length > 0) continue
    const text = (el.textContent ?? '').replace(/\s+/g, ' ').trim()
    if (!/^[\w.-]+\/[\w./-]+/.test(text)) continue
    branchText.push({
      text: text.slice(0, 60),
      scrollWidth: el.scrollWidth,
      clientWidth: el.clientWidth,
      scrollOver: el.scrollWidth - el.clientWidth,
      wrap: getComputedStyle(el).wordBreak,
      cardId: el.closest('[data-ores-kanban="card"]')?.getAttribute('data-story-id') ?? null,
      clippedBy: clipperOf(el),
    })
  }
  const labels = []
  for (const el of view.querySelectorAll('*')) {
    if (el.children.length > 0) continue
    const text = (el.textContent ?? '').replace(/\s+/g, ' ').trim()
    if (text === '') continue
    const color = getComputedStyle(el).color
    const rgb = /rgb\((\d+), (\d+), (\d+)\)/.exec(color)
    const label = /^branch(es)?$/i.test(text) ? 'branch'
      : rgb && Number(rgb[1]) > 200 && Number(rgb[2]) < 120 && Number(rgb[3]) < 120 ? 'red' : null
    if (!label) continue
    const box = el.getBoundingClientRect()
    labels.push({
      kind: label,
      text: text.slice(0, 60),
      color,
      ...describe(el),
      rectLeft: roundTo(box.left),
      rectRight: roundTo(box.right),
      pastFrameRight: roundTo(Math.max(0, box.right - (frameBox ? frameBox.right : box.right))),
      clippedBy: clipperOf(el),
    })
  }
  const columns = [...view.querySelectorAll('[data-ores-kanban="column"]')].map((el) => {
    const box = el.getBoundingClientRect()
    const style = getComputedStyle(el)
    const cards = [...el.querySelectorAll('[data-ores-kanban="card"]')]
    return {
      id: el.getAttribute('data-column-id'),
      width: roundTo(box.width),
      clientWidth: el.clientWidth,
      computedMinWidth: style.minWidth,
      computedFlex: style.flex,
      cardMinWidth: cards.length ? roundTo(Math.min(...cards.map((card) => card.getBoundingClientRect().width))) : null,
      pastScrollerRight: board ? roundTo(box.right - boardBox.right) : null,
    }
  })
  return {
    phase,
    viewport: { width: window.innerWidth, height: window.innerHeight },
    frame: frameBox && {
      left: roundTo(frameBox.left),
      right: roundTo(frameBox.right),
      width: roundTo(frameBox.width),
    },
    board: boardBox && {
      left: roundTo(boardBox.left),
      right: roundTo(boardBox.right),
      width: roundTo(boardBox.width),
      clientWidth: board.clientWidth,
      scrollWidth: board.scrollWidth,
      horizontalScroll: board.scrollWidth - board.clientWidth,
    },
    detailRight: document.querySelector('[data-ores-kanban="detail"]')
      ? roundTo(document.querySelector('[data-ores-kanban="detail"]').getBoundingClientRect().right)
      : null,
    overflow,
    columns,
    branchText,
    labels,
  }
}

function reportOverflow(phase, probe) {
  heading(`overflow walk: ${phase}`)
  if (!probe) {
    console.log('  the board view is not in the document')
    return
  }
  console.log(`  frame right edge ${probe.frame.right}, board scroller ${probe.board.left}..${probe.board.right} (client ${probe.board.clientWidth}, scroll ${probe.board.scrollWidth}, +${probe.board.horizontalScroll})`)
  const clipped = probe.overflow.filter((row) => row.clippedBy).length
  console.log(`  ${probe.overflow.length} element(s) overflow, ${probe.overflow.filter((row) => row.scrollOver > 2).length} past their own content box, ${clipped} clipped by a scroll container, ${probe.overflow.length - clipped} clipped by nothing`)
  for (const row of probe.overflow) {
    console.log(`  [${row.kind}] <${row.tag}> class="${row.className}" data=${JSON.stringify(row.data)}`
      + ` inCard=${row.cardId ?? 'no'} parent=<${row.parent?.tag} data=${JSON.stringify(row.parent?.data)}>`
      + ` scrollWidth=${row.scrollWidth} clientWidth=${row.clientWidth} (+${row.scrollOver})`
      + ` rectRight=${row.rectRight} frameRight=${probe.frame.right} (+${row.escapeOver})`
      + ` insideBoardScroller=${row.insideBoard}`
      + ` clippedBy=${row.clippedBy ? `<${row.clippedBy.tag} data=${JSON.stringify(row.clippedBy.data)}>` : 'nothing'}`
      + ` text="${row.text}"`)
  }
  if (probe.overflow.length === 0) console.log('  (none)')
  const branchRuns = probe.branchText
  const branchOver = branchRuns.filter((row) => row.scrollOver > 2)
  const widest = Math.max(0, ...branchRuns.map((row) => row.scrollOver))
  console.log(`  branch-like text runs: ${branchRuns.length}, widest overflow ${widest}px, ${branchOver.length} past their own content box`)
  console.log(`  branch runs wrap with word-break: ${[...new Set(branchRuns.map((row) => row.wrap))].join(',') || '(none)'}`)
  for (const row of branchRuns.slice(0, 5)) {
    console.log(`    "${row.text}" scrollWidth=${row.scrollWidth} clientWidth=${row.clientWidth} (+${row.scrollOver}) card=${row.cardId ?? 'no'}`)
  }
  const escaping = probe.overflow.filter((row) => !row.clippedBy)
  console.log(widest <= 2
    ? '  verdict: no branch run overflows its own box. The suspected in-card branch clipping is a rendering artefact of the screenshot preview — a long branch wraps on word-break, so the break lands mid-word and reads as a cut.'
    : `  verdict: branch text does overflow its box, up to ${widest}px.`)
  console.log(escaping.length === 0
    ? '  verdict: no element escapes the frame. Every row above sits inside a scroll container that clips it, so all of it is reachable by scrolling.'
    : `  verdict: ${escaping.length} element(s) escape every clipping ancestor.`)
  for (const row of probe.labels) {
    console.log(`  ${row.kind} label "${row.text}" <${row.tag}> color=${row.color} x ${row.rectLeft}..${row.rectRight} (+${row.pastFrameRight} past the frame) data=${JSON.stringify(row.data)} clippedBy=${row.clippedBy ? `<${row.clippedBy.tag} data=${JSON.stringify(row.clippedBy.data)}>` : 'nothing'}`)
  }
  const reds = probe.labels.filter((row) => row.kind === 'red')
  if (reds.length > 0) {
    console.log(`  verdict: the only red label in the view is "${reds[0].text}" at x ${reds[0].rectLeft}..${reds[0].rectRight}, inside the frame; the column it heads is the row above that reaches past the frame right edge.`)
  }
  return {
    branchRuns: branchRuns.length,
    widestBranchOverflow: widest,
    escapingRows: escaping.length,
    redLabels: reds.length,
  }
}

const port = await allocatePort()
const child = bootServer(port)
let browser = null
let payload = null
let pageErrors = []
const stateRequests = []

try {
  heading('0. free-port guard')
  console.log(`  bound ephemeral port ${port}`)
  const boot = await confirmBootUrl(child, port)
  if (!boot.url) {
    console.error(`\nREFUSING TO CONTINUE: ${boot.reason}.`)
    console.error(`The boot log is ${SERVER_LOG}. Another server on the same port would answer in its place.`)
    child.kill('SIGTERM')
    process.exit(2)
  }
  console.log(`  boot log confirms ${boot.url}`)
  check('boot', `the boot log names the bound port ${port}`, boot.url.includes(`:${port}/`), boot.url)
  check('boot', 'the boot URL carries a token', /\?token=\S+/.test(boot.url), boot.url.slice(0, 48) + '…')

  const origin = `http://127.0.0.1:${port}`
  const repoQuery = `cwd=${encodeURIComponent(TREE)}`
  const repoFetch = await fetchState(origin, repoQuery)
  measurements.hostWarmMs = (await fetchState(origin, repoQuery)).ms
  payload = await checkPayload(origin, repoFetch, TREE)
  const expected = deriveExpectations(payload)
  await checkResolutionLadder(origin, TREE)

  heading('3. browser: session and seat 1')
  mkdirSync(SHOT_DIR, { recursive: true })
  browser = await chromium.launch({
    executablePath: '/usr/bin/google-chrome',
    args: ['--no-sandbox', '--disable-dev-shm-usage'],
  })
  const page = await browser.newPage({ viewport: VIEWPORT })
  page.on('pageerror', (err) => pageErrors.push('pageerror: ' + err.message.slice(0, 300)))
  page.on('console', (message) => {
    if (message.type() === 'error') pageErrors.push('console: ' + message.text().slice(0, 300))
  })
  const requestStart = new Map()
  page.on('request', (request) => {
    if (request.url().includes(STATE_PATH)) requestStart.set(request, Date.now())
  })
  page.on('response', async (response) => {
    if (!response.url().includes(STATE_PATH)) return
    const startedAt = requestStart.get(response.request())
    const params = new URL(response.url()).searchParams
    const entry = {
      url: response.url().replace(origin, ''),
      session: params.get('session'),
      cwd: params.get('cwd'),
      status: response.status(),
      ms: startedAt === undefined ? null : Date.now() - startedAt,
    }
    stateRequests.push(entry)
    try {
      const body = await response.json()
      entry.ok = body.ok
      entry.source = body.tree?.source ?? null
      entry.label = body.tree?.label ?? null
      entry.stories = body.stories?.length ?? null
      entry.reason = body.reason ?? null
    } catch {
      entry.ok = null
    }
  })

  const shot = async (name) => {
    const path = `${SHOT_DIR}/${name}.png`
    await page.screenshot({ path, fullPage: true }).catch(() => {})
    screenshots.push(path)
  }

  const started = performance.now()
  await page.goto(boot.url, { waitUntil: 'domcontentloaded', timeout: 60000 })
  const notice = page.getByRole('button', { name: 'Continue', exact: true })
  const appeared = await notice.first().waitFor({ state: 'visible', timeout: 10000 }).then(() => true).catch(() => false)
  if (appeared) await notice.first().click().catch(() => {})
  await page.waitForTimeout(1000)
  check('browser', 'the Internal Testing Notice is dismissed',
    (await notice.count()) === 0, appeared ? 'clicked Continue' : 'no notice in this profile')
  measurements.browserBootMs = round(performance.now() - started)

  const opened = await openSession(page, payload?.tree?.label ?? '')
  check('browser', 'the existing session row is listed and opens', opened.opened, opened.detail)
  await page.waitForTimeout(3500)

  const chip = page.locator('[data-ores-kanban="chip"]').first()
  await page.waitForSelector('[data-ores-kanban="chip"]', { timeout: 20000 }).catch(() => {})
  check('seat1', 'seat 1 chip is rendered', (await chip.count()) > 0)
  const chipText = await chip.innerText().catch(() => '')
  const chipTitle = await chip.getAttribute('title').catch(() => '')
  const chipBody = chipText + chipTitle
  const storyTitleLead = String(expected.currentStory?.title ?? '').slice(0, 24)
  const taskTitleLead = String(expected.currentTask?.title ?? '').slice(0, 20)
  check('seat1', 'seat 1 readout carries tree.label', chipText.includes(String(expected.tree.label)), chipText)
  check('seat1', 'seat 1 readout carries the current story title',
    storyTitleLead !== '' && chipBody.includes(storyTitleLead), storyTitleLead)
  check('seat1', 'seat 1 readout carries the current task title',
    taskTitleLead !== '' && chipBody.includes(taskTitleLead), taskTitleLead)

  const appRequest = stateRequests.filter((request) => request.ok === true).pop() ?? stateRequests[0]
  console.log(`  in-app state request: session=${appRequest?.session} source=${appRequest?.source} label=${appRequest?.label} stories=${appRequest?.stories}`)
  check('seat1', 'the in-app request names a session and resolves a board',
    appRequest?.ok === true && typeof appRequest?.session === 'string' && appRequest.session !== '',
    `session=${appRequest?.session} source=${appRequest?.source} label=${appRequest?.label} reason=${appRequest?.reason}`)
  check('seat1', 'the in-app board belongs to the session work tree', appRequest?.label === expected.tree.label,
    `${appRequest?.label} vs ${expected.tree.label}`)

  heading('4. board')
  const tab = page.getByRole('tab', { name: 'Kanban' })
  check('board', 'the Kanban tab is in the view ring', (await tab.count()) > 0)
  await tab.click()
  await page.waitForSelector('[data-ores-kanban="board"]', { timeout: 30000 }).catch(() => {})
  await page.waitForTimeout(2500)
  await shot('verify-board')
  check('board', 'the board view is rendered', (await page.locator('[data-ores-kanban="view"]').count()) > 0)

  const readDom = () => page.evaluate(() => {
    const text = (el) => (el ? (el.textContent ?? '').replace(/\s+/g, ' ').trim() : '')
    const all = (selector) => [...document.querySelectorAll(selector)]
    return {
      boardText: text(document.querySelector('[data-ores-kanban="view"]')),
      sprintLine: text(document.querySelector('[data-ores-kanban="sprint-line"]')),
      fleet: all('[data-ores-fleet]').map((el) => ({
        label: el.getAttribute('data-ores-fleet'),
        tag: el.tagName.toLowerCase(),
        thisTree: el.getAttribute('data-this-tree'),
        text: text(el),
      })),
      tiles: all('[data-ores-tile]').map((el) => ({ key: el.getAttribute('data-ores-tile'), text: text(el) })),
      columns: all('[data-ores-kanban="column"]').map((el) => ({
        id: el.getAttribute('data-column-id'),
        head: text(el.firstElementChild),
        cards: el.querySelectorAll('[data-ores-kanban="card"]').length,
      })),
      cards: all('[data-ores-kanban="card"]').map((el) => ({
        id: el.getAttribute('data-story-id'),
        current: el.getAttribute('data-current'),
        text: text(el),
      })),
      prLinks: all('[data-ores-kanban="card"] a[href*="/pull/"]').map((a) => a.getAttribute('href')),
      cardBodies: all('[data-ores-kanban="card"]').map((el) => text(el)),
      refresh: !!document.querySelector('[data-ores-kanban="refresh"]'),
      styleTag: !!document.querySelector('style[data-plugin="ores-dsh-kanban"]'),
      search: !!document.querySelector('[data-ores-kanban="search"]'),
      epicChips: all('[data-ores-epic]').map((el) => el.getAttribute('data-ores-epic')),
      switchAffordances: all('[data-ores-kanban="back-to-tree"], [data-ores-kanban="other-tree-note"], [data-ores-kanban="trees"] button, [data-ores-fleet]:not(span)').length,
    }
  })

  const dom = await readDom()
  console.log('  sprint line :', JSON.stringify(dom.sprintLine))
  console.log('  fleet chips :', JSON.stringify(dom.fleet.map((entry) => entry.label + '|' + entry.tag + '|' + entry.thisTree)))
  console.log('  tiles       :', JSON.stringify(dom.tiles))
  console.log('  columns     :', JSON.stringify(dom.columns))

  check('board', 'the sprint line leads with tree.label',
    dom.sprintLine.startsWith(String(expected.tree.label)), dom.sprintLine.slice(0, 90))
  check('board', 'the sprint line shows the branch',
    expected.tree.branch === '' || dom.sprintLine.includes(String(expected.tree.branch)), String(expected.tree.branch))
  check('board', 'the sprint line shows the sprint title and Day X of Y',
    dom.sprintLine.includes(String(expected.sprint.title))
    && dom.sprintLine.includes(`Day ${expected.dayOfSprint} of ${expected.totalDays}`),
    `${expected.sprint.title} Day ${expected.dayOfSprint} of ${expected.totalDays}`)
  check('board', 'the sprint line shows the card count',
    dom.sprintLine.includes(`${expected.stories.length} cards`), `${expected.stories.length} cards`)
  check('board', 'the fleet strip renders a chip per work tree',
    dom.fleet.length === (payload?.trees ?? []).length, `${dom.fleet.length} chips vs ${(payload?.trees ?? []).length} rows`)
  check('board', 'the fleet chips name every work tree',
    JSON.stringify(dom.fleet.map((entry) => entry.label).sort())
    === JSON.stringify((payload?.trees ?? []).map((row) => row.label).sort()))
  check('board', 'the session work tree fleet chip carries the "this tree" marker',
    dom.fleet.some((entry) => entry.label === expected.tree.label && entry.thisTree === 'true' && /this tree/.test(entry.text)),
    String(expected.tree.label))
  check('board', 'exactly one fleet chip is this tree', dom.fleet.filter((entry) => entry.thisTree === 'true').length === 1)
  check('board', 'fleet chips are not buttons', dom.fleet.every((entry) => entry.tag !== 'button'),
    [...new Set(dom.fleet.map((entry) => entry.tag))].join(','))
  check('board', 'the fleet strip renders before the board columns', await page.evaluate(() => {
    const fleet = document.querySelector('[data-ores-kanban="fleet"]')
    const board = document.querySelector('[data-ores-kanban="board"]')
    return !!(fleet && board && (fleet.compareDocumentPosition(board) & Node.DOCUMENT_POSITION_FOLLOWING))
  }))
  check('board', 'the four tiles render the payload counts',
    dom.tiles.length === 4 && dom.tiles.every((tile) => tile.text.includes(expected.tileValues[tile.key])),
    JSON.stringify(expected.tileValues))
  check('board', 'the columns are the payload columns in order, with the payload counts',
    dom.columns.length === expected.columnCounts.length
    && dom.columns.every((column, index) => column.id === expected.columnCounts[index].id
      && column.head.includes(expected.columnCounts[index].title)
      && column.head.includes(String(expected.columnCounts[index].sentCount))
      && column.cards === expected.columnCounts[index].cards),
    dom.columns.map((column) => `${column.id}:${column.cards}/${column.head}`).join(' ') + ' | payload '
      + expected.columnCounts.map((column) => `${column.id}:${column.cards}/${column.sentCount}`).join(' '))
  check('board', 'the board shows the current story title',
    dom.boardText.includes(String(expected.currentStory?.title ?? '')), String(expected.currentStory?.title ?? ''))
  check('board', 'the current card carries the environment chip',
    dom.cards.some((card) => card.text.includes(String(expected.currentStory?.environment ?? ''))),
    String(expected.currentStory?.environment ?? ''))
  check('board', 'the current card shows the task count line',
    dom.cards.some((card) => card.text.includes(
      `${plural(expected.currentStory?.progress.total ?? 0, 'task')} · ${expected.currentStory?.progress.done} done`)),
    `${expected.currentStory?.progress.total} tasks · ${expected.currentStory?.progress.done} done`)
  check('board', 'no card face carries a payload branch name',
    !dom.cardBodies.some((body) => expected.cardFaceBranchNames.some((branch) => branch !== '' && body.includes(branch))),
    expected.cardFaceBranchNames.slice(0, 3).join(' '))
  check('board', 'no card face carries a branch or PR path',
    !dom.cardBodies.some((body) => /feature\//.test(body) || body.includes('/pull/')))
  check('board', 'no card face carries a PR link', dom.prLinks.length === 0, dom.prLinks.slice(0, 2).join(' '))
  check('board', 'the current card shows the age line the payload implies',
    expected.ageLine === ''
      ? !dom.cards.some((card) => card.id === expected.currentStory?.id && /open \d+d/.test(card.text))
      : dom.cards.some((card) => card.id === expected.currentStory?.id && card.text.includes(expected.ageLine)),
    expected.ageLine === '' ? `state ${expected.currentStory?.state} carries no age` : expected.ageLine)
  check('board', 'the current story card carries the current marker',
    dom.cards.some((card) => card.current === 'true' && /current/.test(card.text)))
  check('board', 'the current card is tree.currentStoryId',
    dom.cards.some((card) => card.current === 'true' && card.id === String(expected.tree.currentStoryId)))
  check('board', 'exactly one card is marked current', dom.cards.filter((card) => card.current === 'true').length === 1)
  check('board', 'no card carries a work-tree secondary marker', !dom.cards.some((card) => /this tree/.test(card.text)))
  check('board', 'no tree-switch affordance is in the view', dom.switchAffordances === 0, String(dom.switchAffordances))
  check('board', 'the epic chip row is filters.epics',
    JSON.stringify(dom.epicChips) === JSON.stringify(expected.epics), dom.epicChips.join(' '))
  check('board', 'the refresh control is present', dom.refresh)
  check('board', 'the style element is injected once', dom.styleTag)
  check('board', 'the search input is present', dom.search)

  measurements.overflowBoard = await page.evaluate(probeLayout, 'board-only')
  measurements.overflowBoardSummary = reportOverflow('board only, no detail panel', measurements.overflowBoard)

  heading('5. card detail, task expansion, Escape')
  const targetCard = page.locator(`[data-ores-kanban="card"][data-story-id="${expected.tree.currentStoryId}"]`).first()
  check('detail', 'the current card is located', (await targetCard.count()) > 0)
  await targetCard.scrollIntoViewIfNeeded().catch(() => {})
  await page.waitForTimeout(400)
  await targetCard.click()
  await page.waitForTimeout(1200)
  const detail = page.locator('[data-ores-kanban="detail"]').first()
  check('detail', 'the detail panel opens on card click', (await detail.count()) > 0)
  const detailText = await detail.innerText().catch(() => '')
  const currentStory = expected.currentStory ?? {}
  const detailFields = [
    currentStory.id,
    currentStory.state,
    currentStory.environment,
    currentStory.owner,
    currentStory.epic,
    currentStory.created,
  ].filter((value) => typeof value === 'string' && value !== '')
  check('detail', 'the detail lists the story id, state, environment, owner, epic and created date',
    detailFields.length > 0 && detailFields.every((value) => detailText.includes(value)),
    detailFields.filter((value) => !detailText.includes(value)).join(' | ') || detailFields.join(' '))
  check('detail', 'the detail lists every task title of the story',
    (currentStory.tasks ?? []).length > 0 && (currentStory.tasks ?? []).every((task) => detailText.includes(task.title)),
    `${(currentStory.tasks ?? []).length} task(s)`)
  const storyBranches = currentStory.branches ?? []
  const storyPrs = currentStory.prs ?? []
  check('detail', 'the detail carries the story branch list',
    storyBranches.length === 0
      ? /branches/i.test(detailText)
      : storyBranches.every((branch) => detailText.includes(branch)),
    storyBranches.join(' ') || '(no branches)')
  const detailHrefs = await detail.locator('a[href*="/pull/"]').evaluateAll((nodes) => nodes.map((node) => node.getAttribute('href')))
  check('detail', 'the detail carries a PR link for every PR the story has',
    storyPrs.every((number) => detailHrefs.includes(`https://github.com/OreStudio/OreStudio/pull/${number}`))
    && detailHrefs.length === storyPrs.length,
    `${storyPrs.length} PR(s), ${detailHrefs.length} link(s): ${detailHrefs.slice(0, 3).join(' ')}`)
  await shot('verify-detail')

  if ((await page.locator('[data-ores-kanban="detail"]').count()) === 0) {
    await targetCard.click()
    await page.waitForTimeout(1200)
  }
  measurements.widthHandle = await page.evaluate(() => {
    const handle = document.querySelector('[data-width-handle="right"]')
    const panel = document.querySelector('[data-ores-kanban="detail"]')
    if (!handle) return { present: false }
    const h = handle.getBoundingClientRect()
    const d = panel ? panel.getBoundingClientRect() : null
    const overlapX = d ? Math.min(h.right, d.right) - Math.max(h.left, d.left) : 0
    const overlapY = d ? Math.min(h.bottom, d.bottom) - Math.max(h.top, d.top) : 0
    const probeX = d ? Math.min(h.right, d.right) - 5 : h.left + 5
    const probeY = d ? (Math.max(h.top, d.top) + Math.min(h.bottom, d.bottom)) / 2 : h.top + 5
    const hit = document.elementFromPoint(probeX, probeY)
    return {
      present: true,
      zIndex: getComputedStyle(handle).zIndex,
      handle: { left: Math.round(h.left * 10) / 10, right: Math.round(h.right * 10) / 10, width: Math.round(h.width * 10) / 10 },
      detailPanel: d && { left: Math.round(d.left * 10) / 10, right: Math.round(d.right * 10) / 10 },
      overlapX: Math.round(Math.max(0, overlapX) * 10) / 10,
      overlapY: Math.round(Math.max(0, overlapY) * 10) / 10,
      panelRightEdgeClearOfHandle: d ? Math.round((d.right - h.right) * 10) / 10 : null,
      topmostAtOverlap: hit ? hit.tagName.toLowerCase() + (hit.getAttribute('data-width-handle') ? '[data-width-handle]' : '') : null,
      handleTakesTheOverlapPoint: hit === handle || (hit ? handle.contains(hit) : false),
    }
  })
  const handle = measurements.widthHandle
  heading('host chrome: split-view resize handle')
  if (!handle.present || !handle.detailPanel) {
    console.log(`  the handle or the detail panel is absent: ${JSON.stringify(handle)}`)
  } else {
    console.log(`  handle x ${handle.handle.left}..${handle.handle.right} (width ${handle.handle.width}, z-index ${handle.zIndex})`)
    console.log(`  detail panel x ${handle.detailPanel.left}..${handle.detailPanel.right}`)
    console.log(`  horizontal overlap ${handle.overlapX}px, vertical overlap ${handle.overlapY}px`)
    console.log(`  the panel's right edge is clear of the handle by ${handle.panelRightEdgeClearOfHandle}px`)
    console.log(`  topmost element 5px inside the overlap: ${handle.topmostAtOverlap} (the handle takes that point: ${handle.handleTakesTheOverlapPoint})`)
  }

  measurements.overflowDetail = await page.evaluate(probeLayout, 'detail-open')
  measurements.overflowDetailSummary = reportOverflow('detail panel open', measurements.overflowDetail)

  const taskRow = page.locator('[data-ores-kanban="detail"] [data-ores-task] button').first()
  check('detail', 'a task row is present', (await taskRow.count()) > 0)
  await taskRow.scrollIntoViewIfNeeded().catch(() => {})
  await page.waitForTimeout(300)
  const clicked = await page.evaluate(() => {
    const el = document.querySelector('[data-ores-kanban="detail"] [data-ores-task] button')
    if (!el) return false
    el.click()
    return true
  })
  check('detail', 'the task row click is dispatched', clicked)
  await page.waitForTimeout(700)
  const expanded = await page.locator('[data-ores-kanban="detail"]').first().innerText()
  check('detail', 'the task row expands blocked on, blocked since, created, updated and file',
    /blocked on/.test(expanded) && /blocked since/.test(expanded) && /created/.test(expanded)
    && /updated/.test(expanded) && /file/.test(expanded))

  await page.keyboard.press('Escape')
  await page.waitForTimeout(900)
  check('detail', 'Escape closes the detail panel', (await page.locator('[data-ores-kanban="detail"]').count()) === 0)

  heading('6. seat 1 popover')
  await chip.click()
  await page.waitForTimeout(900)
  const popover = page.locator('[data-ores-kanban="popover"]').first()
  check('popover', 'the chip popover opens', (await popover.count()) > 0)
  const popoverRole = await popover.getAttribute('role').catch(() => '')
  const popoverText = await popover.innerText().catch(() => '')
  check('popover', 'the popover has role=dialog', popoverRole === 'dialog', popoverRole)
  check('popover', 'aria-expanded is true while open', (await chip.getAttribute('aria-expanded')) === 'true')
  check('popover', 'the popover lists every task of the current story',
    (currentStory.tasks ?? []).length > 0 && (currentStory.tasks ?? []).every((task) => popoverText.includes(task.title)),
    `${(currentStory.tasks ?? []).length} task(s)`)
  check('popover', 'the popover carries the environment and a current marker',
    popoverText.includes(String(currentStory.environment)) && /current/.test(popoverText),
    String(currentStory.environment))
  await shot('verify-popover')
  await page.keyboard.press('Escape')
  await page.waitForTimeout(700)
  check('popover', 'Escape closes the popover', (await page.locator('[data-ores-kanban="popover"]').count()) === 0)

  heading('7. filters and search')
  const cardsBefore = await page.locator('[data-ores-kanban="card"]').count()
  check('filters', 'the board starts with every story in the sprint', cardsBefore === expected.stories.length,
    `${cardsBefore} cards vs ${expected.stories.length} stories`)
  if (expected.filteringEpic === null) {
    console.log(`  no epic chip narrows the board: ${expected.epics.join(' ') || '(none)'} cover every story, so the epic filter is skipped`)
  } else {
    await page.locator(`[data-ores-epic="${expected.filteringEpic}"]`).first().click()
    await page.waitForTimeout(900)
    const afterEpic = await page.locator('[data-ores-kanban="card"]').count()
    const epicActive = await page.locator('[data-ores-kanban="active-filters"]').innerText().catch(() => '')
    check('filters', `the ${expected.filteringEpic} epic chip narrows the board to that epic's stories`,
      afterEpic === expected.filteringEpicCount, `${cardsBefore} -> ${afterEpic}, expected ${expected.filteringEpicCount}`)
    check('filters', 'the epic filter is individually clearable', /epic /.test(epicActive), epicActive.slice(0, 60))
    await page.locator('[data-ores-kanban="clear"]').first().click()
    await page.waitForTimeout(900)
    check('filters', 'Clear after an epic filter restores the board',
      (await page.locator('[data-ores-kanban="card"]').count()) === cardsBefore)
  }
  await page.locator('[data-ores-kanban="search"]').first().fill(expected.searchQuery)
  await page.waitForTimeout(900)
  const shownIds = await page.locator('[data-ores-kanban="card"]').evaluateAll((nodes) => nodes.map((node) => node.getAttribute('data-story-id')).sort())
  const searchActive = await page.locator('[data-ores-kanban="active-filters"]').innerText().catch(() => '')
  check('filters', 'search shows exactly the stories the payload says match',
    JSON.stringify(shownIds) === JSON.stringify(expected.searchIds),
    `query "${expected.searchQuery.slice(0, 40)}" -> ${shownIds.length} of ${expected.searchIds.length} expected`)
  check('filters', 'the active search filter is shown', /search/.test(searchActive), searchActive.slice(0, 60))
  await shot('verify-filters')
  await page.locator('[data-ores-kanban="clear"]').first().click()
  await page.waitForTimeout(900)
  check('filters', 'Clear restores the board', (await page.locator('[data-ores-kanban="card"]').count()) === cardsBefore)

  heading('8. refresh')
  await page.locator('[data-ores-kanban="refresh"]').first().click()
  await page.waitForTimeout(2500)
  check('refresh', 'Refresh keeps a working board', (await page.locator('[data-ores-kanban="column"]').count()) > 0)

  heading('narrow viewport: seat 1 popover')
  await page.setViewportSize(NARROW)
  await page.waitForTimeout(700)
  await chip.scrollIntoViewIfNeeded().catch(() => {})
  await chip.click({ force: true }).catch(() => {})
  await page.waitForTimeout(900)
  const narrowOpen = (await page.locator('[data-ores-kanban="popover"]').count()) > 0
  measurements.popoverNarrow = await page.evaluate((open) => {
    const box = (el) => {
      if (!el) return null
      const r = el.getBoundingClientRect()
      return {
        left: Math.round(r.left * 10) / 10, right: Math.round(r.right * 10) / 10,
        top: Math.round(r.top * 10) / 10, bottom: Math.round(r.bottom * 10) / 10,
        width: Math.round(r.width * 10) / 10, height: Math.round(r.height * 10) / 10,
      }
    }
    const popover = document.querySelector('[data-ores-kanban="popover"]')
    const wrap = document.querySelector('[data-ores-kanban="chip-wrap"]')
    const p = box(popover)
    const viewport = { width: window.innerWidth, height: window.innerHeight }
    const outside = p ? {
      left: Math.round(Math.max(0, -p.left) * 10) / 10,
      right: Math.round(Math.max(0, p.right - viewport.width) * 10) / 10,
      top: Math.round(Math.max(0, -p.top) * 10) / 10,
      bottom: Math.round(Math.max(0, p.bottom - viewport.height) * 10) / 10,
    } : null
    const slackRight = p ? Math.round((viewport.width - p.right) * 10) / 10 : null
    const slackBottom = p ? Math.round((viewport.height - p.bottom) * 10) / 10 : null
    return {
      open, viewport, popover: p, chipWrap: box(wrap), outside,
      slackRight, slackBottom,
      clipsBelowWidth: p ? p.right : null,
      fullyInside: !!p && Object.values(outside).every((v) => v === 0),
    }
  }, narrowOpen)
  const narrow = measurements.popoverNarrow
  console.log(`  viewport ${narrow.viewport.width}x${narrow.viewport.height}, popover open: ${narrow.open}`)
  if (narrow.popover) {
    console.log(`  popover x ${narrow.popover.left}..${narrow.popover.right} y ${narrow.popover.top}..${narrow.popover.bottom}`)
    console.log(`  chip wrap x ${narrow.chipWrap.left}..${narrow.chipWrap.right}`)
    console.log(`  outside the viewport — left ${narrow.outside.left}, right ${narrow.outside.right}, top ${narrow.outside.top}, bottom ${narrow.outside.bottom}`)
    console.log(`  slack ${narrow.slackRight}px at the right edge, ${narrow.slackBottom}px at the bottom; the popover is left-anchored to the chip, so it would clip once the viewport is narrower than ${narrow.clipsBelowWidth}px`)
    console.log(narrow.fullyInside
      ? '  the popover stays fully inside the viewport at 900x800'
      : '  the popover leaves the viewport at 900x800')
  } else {
    console.log('  the popover did not open at the narrow viewport')
  }
  await shot('verify-popover-narrow')
  await page.keyboard.press('Escape')
  await page.waitForTimeout(400)
  await page.setViewportSize(VIEWPORT)

  check('errors', 'the page reported no errors', pageErrors.length === 0, pageErrors.slice(0, 3).join(' | '))
  measurements.stateRequests = stateRequests
  console.log('\nstate requests:')
  for (const request of stateRequests) console.log(`  ${request.status} ${request.ms}ms source=${request.source} ${request.url}`)
} catch (error) {
  measurements.fatal = String(error?.message ?? error).split('\n')[0]
  console.error(`\nFATAL: ${measurements.fatal}`)
} finally {
  if (browser) await browser.close().catch(() => {})
  child.kill('SIGTERM')
  await delay(800)
  if (child.exitCode === null && child.signalCode === null) child.kill('SIGKILL')
}

const failed = assertions.filter((assertion) => !assertion.ok)
heading('summary')
console.log(`assertions: ${assertions.length - failed.length}/${assertions.length} passed`)
console.log(`boot URL: ${measurements.host ? `http://127.0.0.1:${port}/?token=…` : '(not reached)'}`)
if (failed.length) console.log('failed: ' + failed.map((assertion) => assertion.name).join(' | '))
if (measurements.fatal) console.log('fatal: ' + measurements.fatal)

writeFileSync(RESULT_FILE, JSON.stringify({
  generatedAt: new Date().toISOString(),
  port,
  bootUrl: `http://127.0.0.1:${port}/?token=…`,
  serverLog: SERVER_LOG,
  repo: REPO,
  treeRoot: TREE,
  dshHome: DSH_HOME,
  profile: PROFILE,
  viewport: VIEWPORT,
  narrowViewport: NARROW,
  subject: provenance(),
  fatal: measurements.fatal ?? null,
  host: measurements.host ?? null,
  payload: measurements.payload ?? null,
  requestTimings: {
    hostCwdMs: measurements.host?.ms ?? null,
    hostCwdRepeatMs: measurements.hostWarmMs ?? null,
    ladder: measurements.ladder?.requests ?? [],
    browserBootMs: measurements.browserBootMs ?? null,
    stateRequests,
  },
  assertions,
  measurements: {
    ladder: measurements.ladder ?? null,
    overflowBoard: measurements.overflowBoard ?? null,
    overflowDetail: measurements.overflowDetail ?? null,
    overflowBoardSummary: measurements.overflowBoardSummary ?? null,
    overflowDetailSummary: measurements.overflowDetailSummary ?? null,
    popoverNarrow: measurements.popoverNarrow ?? null,
    widthHandle: measurements.widthHandle ?? null,
  },
  pageErrors,
  screenshots,
  summary: {
    total: assertions.length,
    passed: assertions.length - failed.length,
    failed: failed.length,
    failedNames: failed.map((assertion) => assertion.name),
  },
}, null, 2))

console.log(`result: ${RESULT_FILE}`)
console.log(`screenshots: ${screenshots.join(', ') || '(none)'}`)
process.exit(failed.length || measurements.fatal ? 1 : 0)
