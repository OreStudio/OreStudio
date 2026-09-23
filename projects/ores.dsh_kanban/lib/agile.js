// The agile org tree as a model. Pure: no fs, no child_process, no globals.
// Callers hand in file text and directory slugs; every process boundary lives in
// git.js.

// The closed state vocabulary, in board order. `column` folds states that share a
// kanban column, so the columns, the counts and the task ordering all read from
// this one table instead of comparing state strings anywhere else.
const STATES = [
  { id: 'DISCOVERED', title: 'Backlog', column: 'BACKLOG' },
  { id: 'BACKLOG', title: 'Backlog', column: 'BACKLOG' },
  { id: 'STARTED', title: 'Started', column: 'STARTED' },
  { id: 'BLOCKED', title: 'Blocked', column: 'BLOCKED' },
  { id: 'DONE', title: 'Done', column: 'DONE' },
  { id: 'ABANDONED', title: 'Abandoned', column: 'ABANDONED' },
]

const UNKNOWN_STATE = 'UNKNOWN'
const UNKNOWN_COLUMN = { id: UNKNOWN_STATE, title: 'Unknown' }

// The fleet strip is a glance, not an inventory; the contract caps it.
const FLEET_CAP = 40

const STATE_ORDER = new Map(STATES.map((s, index) => [s.id, index]))
const STATE_COLUMN = new Map(STATES.map((s) => [s.id, s.column]))

export const STATE_IDS = STATES.map((s) => s.id).concat(UNKNOWN_STATE)

export function normalizeState(raw) {
  const state = String(raw ?? '').trim().toUpperCase()
  return STATE_ORDER.has(state) ? state : UNKNOWN_STATE
}

function columnOf(state) {
  return STATE_COLUMN.get(state) ?? UNKNOWN_COLUMN.id
}

export function columnOrder(state) {
  const index = STATE_ORDER.get(state)
  return index === undefined ? STATES.length : index
}

export function readOrgDocument(text) {
  const keywords = new Map()
  const fields = new Map()
  const epics = new Map()
  let drawer = null
  let firstDrawer = null

  const lines = String(text ?? '').split(/\r?\n/)
  let theme = ''
  let inStoriesSection = false

  for (let index = 0; index < lines.length; index += 1) {
    const line = lines[index]
    const keyword = /^#\+([A-Za-z0-9_-]+):[ \t]*(.*)$/.exec(line)
    if (keyword) {
      const key = keyword[1].toLowerCase()
      if (!keywords.has(key)) keywords.set(key, keyword[2].trim())
      continue
    }

    const heading = /^(\*+)\s+(.*)$/.exec(line)
    if (heading) {
      const level = heading[1].length
      const title = heading[2].trim()
      if (level === 1) {
        inStoriesSection = title.toLowerCase() === 'stories'
        theme = ''
      } else if (inStoriesSection) {
        // The theme is the `**` group heading the story's row sits under, so a
        // deeper `***` heading does not replace it.
        if (level === 2) theme = title
        else if (level === 3 && theme === '') theme = title
      }
      continue
    }

    // The contract reads :ID: from the first properties drawer, so keep that one
    // after it closes.
    if (drawer) {
      const entry = /^:([A-Za-z0-9_-]+):[ \t]*(.*)$/.exec(line)
      if (!entry) continue
      if (entry[1].toUpperCase() === 'END') {
        firstDrawer ??= drawer
        drawer = null
        continue
      }
      if (!drawer.has(entry[1].toLowerCase())) {
        drawer.set(entry[1].toLowerCase(), entry[2].trim())
      }
      continue
    }
    if (/^:PROPERTIES:[ \t]*$/i.test(line)) {
      drawer = new Map()
      continue
    }

    const cells = tableRow(line)
    if (!cells) continue

    if (cells.length >= 2) {
      const key = cells[0].trim()
      if (!fields.has(key)) fields.set(key, cells[1].trim())
    }
    if (inStoriesSection && cells[0].toLowerCase() === 'story') {
      for (const row of storyRows(lines, index)) {
        if (!epics.has(row.id)) epics.set(row.id, theme)
      }
    }
  }

  return { keywords, fields, epics, properties: firstDrawer ?? drawer ?? new Map() }
}

function tableRow(line) {
  const trimmed = line.trim()
  if (!trimmed.startsWith('|') || !trimmed.endsWith('|')) return null
  const cells = trimmed.slice(1, -1).split('|').map((cell) => cell.trim())
  if (cells.length > 0 && cells.every((cell) => /^[-+]+$/.test(cell) || cell === '')) return null
  return cells
}

function storyRows(lines, startIndex) {
  const rows = []
  for (let i = startIndex + 1; i < lines.length; i += 1) {
    const line = lines[i]
    if (/^(\*+)\s+/.test(line) || /^#\+/.test(line)) break
    const cells = tableRow(line)
    if (!cells) continue
    const id = /\[\[id:([0-9A-Fa-f-]+)\]/.exec(cells[0] ?? '')
    if (id) rows.push({ id: id[1].toUpperCase(), cells })
  }
  return rows
}

function collapsed(value) {
  return typeof value === 'string' ? value.trim().replace(/\s+/g, ' ') : ''
}

// A single-token keyword is cut at its first whitespace: upstream carries
// `#+environment: brave_hopper brave_hopper`, and the doubled value must not
// become a filter chip.
function collapsedToken(value) {
  const joined = collapsed(value)
  const cut = joined.indexOf(' ')
  return cut < 0 ? joined : joined.slice(0, cut)
}

function keyword(doc, name) {
  const value = doc.keywords.get(name)
  return value === undefined || value === '' ? '' : collapsed(value)
}

function tokenKeyword(doc, name) {
  const value = doc.keywords.get(name)
  return value === undefined || value === '' ? '' : collapsedToken(value)
}

function stripTitlePrefix(title) {
  return title.replace(/^(Story|Task):\s*/i, '').trim()
}

function documentId(doc) {
  const declared = keyword(doc, 'id')
  return (declared || doc.properties.get('id') || '').toUpperCase()
}

export function parseStoryDocument(text, dirSlug) {
  const doc = readOrgDocument(text)
  const title = keyword(doc, 'title')
  return {
    id: documentId(doc),
    slug: dirSlug,
    title: stripTitlePrefix(title),
    type: keyword(doc, 'type'),
    description: keyword(doc, 'description'),
    environment: tokenKeyword(doc, 'environment'),
    owner: tokenKeyword(doc, 'owner'),
    blockedOn: keyword(doc, 'blocked_on'),
    blockedSince: keyword(doc, 'blocked_since'),
    created: keyword(doc, 'created'),
    updated: keyword(doc, 'updated'),
    state: normalizeState(doc.fields.get('State')),
    waiting: doc.fields.get('Waiting on') ?? '',
  }
}

export function parseTaskDocument(text, fileSlug) {
  const doc = readOrgDocument(text)
  const title = stripTitlePrefix(keyword(doc, 'title'))
  return {
    id: documentId(doc),
    slug: fileSlug,
    title,
    type: keyword(doc, 'type'),
    description: keyword(doc, 'description'),
    environment: tokenKeyword(doc, 'environment'),
    owner: tokenKeyword(doc, 'owner'),
    branch: keyword(doc, 'branch'),
    pr: keyword(doc, 'pr'),
    blockedOn: keyword(doc, 'blocked_on'),
    blockedSince: keyword(doc, 'blocked_since'),
    created: keyword(doc, 'created'),
    updated: keyword(doc, 'updated'),
    state: normalizeState(doc.fields.get('State')),
    waiting: doc.fields.get('Waiting on') ?? '',
    scaffold: /^Scaffold story:/i.test(title),
  }
}

export function parseSprintDocument(text) {
  const doc = readOrgDocument(text)
  return {
    title: keyword(doc, 'title'),
    description: keyword(doc, 'description'),
    created: keyword(doc, 'created'),
    updated: keyword(doc, 'updated'),
    startDate: keyword(doc, 'start_date') || collapsed(doc.fields.get('Start')) || '',
    endDate:
      keyword(doc, 'end_date') ||
      collapsed(doc.fields.get('End (expected)')) ||
      collapsed(doc.fields.get('End')) ||
      '',
    state: normalizeState(doc.fields.get('State')),
    waiting: doc.fields.get('Waiting on') ?? '',
    epics: doc.epics,
  }
}

export function parsePrNumber(raw) {
  const digits = /#?\s*(\d+)/.exec(String(raw ?? ''))
  return digits ? Number(digits[1]) : null
}

export function parsePrNumbers(values) {
  const numbers = new Set()
  for (const value of values) {
    const number = parsePrNumber(value)
    if (number !== null) numbers.add(number)
  }
  return [...numbers].sort((a, b) => a - b)
}

function dayOf(startDate, endDate) {
  const start = Date.parse(`${startDate}T00:00:00Z`)
  const end = Date.parse(`${endDate}T00:00:00Z`)
  if (!Number.isFinite(start) || !Number.isFinite(end)) return null
  return Math.max(Math.round((end - start) / 86400000) + 1, 0)
}

// An overrun sprint reports its true day: clamping it to the sprint's own span
// would hide how far past the end the work has run.
function sprintDay(startDate, today) {
  const start = Date.parse(`${startDate}T00:00:00Z`)
  const now = Date.parse(`${today}T00:00:00Z`)
  if (!Number.isFinite(start) || !Number.isFinite(now)) return null
  return Math.floor((now - start) / 86400000) + 1
}

// The board covers one work tree, so the work item is resolved against that
// tree's own sprint: the branch picks the candidates, the tree's journal entry
// picks between tasks that share a branch.
export function resolveItem(tasks, branch, entry) {
  const unresolved = { storyId: '', taskId: '', by: 'none' }
  if (!branch) return unresolved
  const candidates = tasks.filter((candidate) => candidate.branch === branch)
  if (candidates.length === 0) return unresolved
  return { ...pickCandidate(candidates, entry), by: 'branch' }
}

function pickCandidate(candidates, entry) {
  const fromJournal = entry?.taskId
    ? candidates.find((candidate) => candidate.id === entry.taskId)
    : undefined
  if (fromJournal) return { storyId: fromJournal.storyId, taskId: fromJournal.id }
  const chosen = [...candidates].sort(byUpdatedThenPath)[0]
  return { storyId: chosen.storyId, taskId: chosen.id }
}

function byUpdatedThenPath(a, b) {
  if (a.updated !== b.updated) return a.updated < b.updated ? 1 : -1
  return a.path < b.path ? -1 : a.path > b.path ? 1 : 0
}

export function buildModel(input) {
  const { doc, dirs } = input
  const options = input.options ?? {}
  const storiesPath = options.storiesPath ?? 'story.org'

  const stories = []
  const tasks = []
  const today = options.today ?? new Date().toISOString().slice(0, 10)

  for (const dir of dirs) {
    if (typeof dir.story !== 'string') continue
    const story = parseStoryDocument(dir.story, dir.slug)
    story.epic = doc.epics.get(story.id) ?? ''
    story.path = `${sprintDirPath(options.sprintPath, storiesPath)}${dir.slug}/story.org`
    story.tasks = []

    for (const task of dir.tasks ?? []) {
      const parsed = parseTaskDocument(task.text, task.slug)
      parsed.storyId = story.id
      parsed.path = `${sprintDirPath(options.sprintPath, storiesPath)}${dir.slug}/task_${task.slug}.org`
      story.tasks.push(parsed)
      tasks.push(parsed)
    }

    story.tasks.sort(byStateThenTitle)
    story.branches = unique(story.tasks.map((task) => task.branch))
    story.prs = parsePrNumbers(story.tasks.map((task) => task.pr))
    story.progress = {
      done: story.tasks.filter((task) => task.state === 'DONE').length,
      total: story.tasks.length,
      abandoned: story.tasks.filter((task) => task.state === 'ABANDONED').length,
    }
    stories.push(story)
  }

  stories.sort(byStateThenTitle)
  const columns = buildColumns(stories)
  const sprint = {
    version: options.version ?? '',
    name: options.sprintName ?? '',
    title: doc.title,
    startDate: doc.startDate,
    endDate: doc.endDate,
    dayOfSprint: sprintDay(doc.startDate, today),
    totalDays: dayOf(doc.startDate, doc.endDate),
    path: options.sprintPath ?? '',
  }

  // The board belongs to the session's work tree and to no other, so `tree` is
  // never a selection: it is the tree every card above was read from.
  const ownTree = options.tree ?? null
  const own = ownTree ? resolveItem(tasks, ownTree.branch, ownTree.entry) : null
  const tree = ownTree
    ? {
        root: ownTree.root,
        name: ownTree.name,
        label: ownTree.label,
        branch: ownTree.branch,
        detached: ownTree.detached,
        dirty: ownTree.dirty,
        currentStoryId: own.storyId,
        currentTaskId: own.taskId,
        by: own.by === 'none' && sprint.name ? 'sprint-only' : own.by,
      }
    : null

  return {
    tree,
    sprint,
    columns,
    stories,
    trees: buildTreeRows(options.trees, stories, tasks, ownTree?.root),
    counts: buildCounts(stories, tasks),
    filters: {
      environments: unique([...stories.map((s) => s.environment), ...tasks.map((t) => t.environment)]),
      epics: unique(stories.map((s) => s.epic)),
    },
  }
}

export function buildTreeRows(trees, stories, tasks, sessionRoot) {
  const byId = new Map(tasks.map((task) => [task.id, task]))
  const storyById = new Map(stories.map((story) => [story.id, story]))
  const rows = (trees ?? []).map((candidate) => {
    const entry = candidate.entry ?? null
    // A work tree's branch checkout may not carry the story or task its journal
    // names, so only an id this sprint really holds is reported as an id; the
    // titles the journal carries always describe that work tree's own item.
    const task = resolveWorkItem(byId, storyById, tasks, candidate.branch, entry)
    return {
      label: candidate.label,
      name: candidate.name,
      root: candidate.root,
      branch: candidate.branch,
      detached: Boolean(candidate.detached),
      dirty: Boolean(candidate.dirty),
      isSession: sessionRoot !== undefined && candidate.root === sessionRoot,
      currentStoryId: task.story?.id ?? '',
      currentTaskId: task.task?.id ?? entry?.taskId ?? '',
      storyTitle: task.story?.title ?? entry?.storyTitle ?? '',
      taskTitle: task.task?.title ?? entry?.taskTitle ?? '',
      state: task.task?.state ?? normalizeState(entry?.state),
      pr: task.task?.pr || entry?.pr || '',
    }
  })
  rows.sort((a, b) => (a.label < b.label ? -1 : a.label > b.label ? 1 : 0))
  return rows.slice(0, FLEET_CAP)
}

function resolveWorkItem(byId, storyById, tasks, branch, entry) {
  // A journal that names a task is authoritative: falling back to the branch
  // there would report a task the work tree already moved on from.
  const resolved = resolveItem(tasks, entry?.taskId ? '' : branch, entry)
  const task = byId.get(entry?.taskId ?? resolved.taskId) ?? null
  return { task, story: task ? storyById.get(task.storyId) ?? null : null }
}

function sprintDirPath(sprintPath, storiesPath) {
  const suffix = storiesPath.replace(/^story\.org$/, '')
  return `${String(sprintPath).replace(/sprint\.org$/, '')}${suffix}`
}

function byStateThenTitle(a, b) {
  const order = columnOrder(a.state) - columnOrder(b.state)
  if (order !== 0) return order
  return a.title < b.title ? -1 : a.title > b.title ? 1 : 0
}

function unique(values) {
  const seen = new Set()
  for (const value of values) {
    if (typeof value === 'string' && value !== '') seen.add(value)
  }
  return [...seen].sort()
}

function buildColumns(stories) {
  const counts = new Map()
  for (const story of stories) {
    const column = columnOf(story.state)
    counts.set(column, (counts.get(column) ?? 0) + 1)
  }
  const columns = []
  const emitted = new Set()
  // The canonical columns are always sent, empty ones included, so the board
  // does not rearrange itself as cards move between states.
  for (const state of STATES) {
    if (emitted.has(state.column)) continue
    emitted.add(state.column)
    columns.push({ id: state.column, title: state.title, count: counts.get(state.column) ?? 0 })
  }
  // The unknown column earns its place only when a card lands in it, and it sits last.
  if (counts.has(UNKNOWN_COLUMN.id)) {
    columns.push({ ...UNKNOWN_COLUMN, count: counts.get(UNKNOWN_COLUMN.id) })
  }
  return columns
}

function buildCounts(stories, tasks) {
  const inState = (list, state) => list.filter((item) => item.state === state).length
  return {
    stories: stories.length,
    storiesDone: inState(stories, 'DONE'),
    storiesStarted: inState(stories, 'STARTED'),
    storiesBlocked: inState(stories, 'BLOCKED'),
    tasks: tasks.length,
    tasksDone: inState(tasks, 'DONE'),
  }
}

export function failure(reason, message) {
  return { ok: false, reason, message }
}

export function parseJournalEntry(text) {
  const lines = String(text ?? '').split(/\r?\n/)
  const entry = { date: '', storyId: '', storyTitle: '', taskId: '', taskTitle: '', state: '', branch: '', pr: '' }
  const stamp = /^\*\s+(\d{4}-\d{2}-\d{2}(?:[ T]\d{2}:\d{2})?)/.exec(lines[0]?.trim() ?? '')
  if (stamp) entry.date = stamp[1]
  const heading = /\[\[id:([0-9A-Fa-f-]+)\]\[(.*?)\]\]\s*$/.exec(lines[0]?.trim() ?? '')
  if (heading) {
    entry.storyId = heading[1].toUpperCase()
    entry.storyTitle = heading[2]
  }
  for (const line of lines.slice(1)) {
    const field = /^-\s*([A-Za-z ]+?)\s*::\s*(.*)$/.exec(line.trim())
    if (!field) continue
    const value = field[2].trim()
    switch (field[1].toLowerCase()) {
      case 'task': {
        const task = /\[\[id:([0-9A-Fa-f-]+)\]\[(.*)\]\]$/.exec(value)
        if (task) {
          entry.taskId = task[1].toUpperCase()
          entry.taskTitle = task[2]
        }
        break
      }
      case 'state':
        entry.state = value
        break
      case 'branch':
        entry.branch = value === 'none' ? '' : value
        break
      case 'pr':
        entry.pr = parsePrNumber(value) === null ? '' : String(parsePrNumber(value))
        break
      default:
        break
    }
  }
  return entry
}
