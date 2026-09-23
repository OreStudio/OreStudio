import assert from 'node:assert/strict'
import test from 'node:test'
import { readFileSync, readdirSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { dirname, join } from 'node:path'
import {
  STATE_IDS,
  buildModel,
  columnOrder,
  normalizeState,
  parseJournalEntry,
  parsePrNumber,
  parsePrNumbers,
  parseSprintDocument,
  parseStoryDocument,
  parseTaskDocument,
  resolveItem,
} from '../lib/agile.js'

// The live org tree is the fixture: these parsers are only worth anything against
// the files they read in production. Other agents close, start and abandon work in
// this tree while the suite runs, so a test asserts a literal only for a field an
// edit cannot move, and every expected value comes from the file, not from the
// code under test.
const SPRINT_DIR = join(
  dirname(fileURLToPath(import.meta.url)),
  '..',
  '..',
  '..',
  'doc/agile/versions/v0/sprint_25',
)
const STORY_DIR = join(SPRINT_DIR, 'dsh_agile_plugin')
const SPRINT_PATH = 'doc/agile/versions/v0/sprint_25/sprint.org'
const STORY_ID = '3085B911-3030-4AB8-976F-0F037D4332E7'
const SCAFFOLD_ID = '8A81CB08-A606-4E30-A717-88B810308137'
const IMPLEMENT_ID = '2612E669-545E-4482-8115-E9C1A3278770'
const STORY_TITLE = 'Show the current agile work item inside DSH'
const SCAFFOLD_TITLE = `Scaffold story: ${STORY_TITLE}`
const IMPLEMENT_TITLE = 'Build the ORE Studio kanban plugin for DSH'
const BRANCH = 'feature/dsh-agile-plugin'
const ELSEWHERE_TASK_ID = '826C9671-AE0D-48CD-B8E6-666DF92DB4AF'

const sprintText = readFileSync(join(SPRINT_DIR, 'sprint.org'), 'utf8')
const storyText = readFileSync(join(STORY_DIR, 'story.org'), 'utf8')
const scaffoldText = readFileSync(join(STORY_DIR, 'task_scaffold_dsh_agile_plugin.org'), 'utf8')
const implementText = readFileSync(join(STORY_DIR, 'task_implement_dsh_agile_plugin.org'), 'utf8')

const TREE = {
  root: '/w/ores_dev_bright_faraday',
  name: 'ores_dev_bright_faraday',
  label: 'bright_faraday',
  branch: BRANCH,
  detached: false,
  dirty: false,
}

function storyFile(slug) {
  return readFileSync(join(SPRINT_DIR, slug, 'story.org'), 'utf8')
}

function taskFile(slug, file) {
  return readFileSync(join(SPRINT_DIR, slug, `task_${file}.org`), 'utf8')
}

function taskFiles(slug) {
  return readdirSync(join(SPRINT_DIR, slug))
    .map((name) => /^task_(.+)\.org$/.exec(name))
    .filter((match) => match !== null)
    .map((match) => match[1])
}

// The Status row as the file literally carries it, which is the only thing an
// assertion about `state` may be compared against.
function statusOf(text) {
  const row = /^\|\s*State\s*\|(.*)\|$/m.exec(text)
  return row ? row[1].trim().replace(/\s+/g, ' ').toUpperCase() : 'UNKNOWN'
}

function story(overrides = {}) {
  return {
    id: '00000000-0000-0000-0000-00000000000A',
    slug: 'a',
    title: 'A story',
    state: 'BACKLOG',
    epic: '',
    description: '',
    environment: '',
    owner: '',
    created: '',
    updated: '',
    path: '',
    progress: { done: 0, total: 0, abandoned: 0 },
    branches: [],
    prs: [],
    tasks: [],
    ...overrides,
  }
}

function task(overrides = {}) {
  return {
    id: '00000000-0000-0000-0000-00000000000B',
    slug: 'b',
    title: 'A task',
    state: 'BACKLOG',
    branch: '',
    pr: '',
    owner: '',
    environment: '',
    created: '',
    updated: '',
    path: '',
    storyId: '',
    scaffold: false,
    ...overrides,
  }
}

function readSprintDirs() {
  return readdirSync(SPRINT_DIR, { withFileTypes: true })
    .filter((entry) => entry.isDirectory())
    .map((entry) => {
      const dir = join(SPRINT_DIR, entry.name)
      const tasks = readdirSync(dir, { withFileTypes: true })
        .map((item) => /^task_(.+)\.org$/.exec(item.name))
        .filter((match) => match !== null)
        .map((match) => ({ slug: match[1], text: readFileSync(join(dir, match[0]), 'utf8') }))
      return { slug: entry.name, story: readFileSync(join(dir, 'story.org'), 'utf8'), tasks }
    })
}

function realModel(options = {}) {
  return buildModel({
    doc: parseSprintDocument(sprintText),
    dirs: readSprintDirs(),
    options: {
      version: 'v0',
      sprintName: 'sprint_25',
      sprintPath: SPRINT_PATH,
      today: '2026-09-23',
      ...options,
    },
  })
}

function realTasks() {
  return realModel().stories.flatMap((story) => story.tasks)
}

function sprintStory(slug) {
  return realModel().stories.find((story) => story.slug === slug)
}

test('a story carries the literal fields of its own org file', () => {
  const parsed = parseStoryDocument(storyText, 'dsh_agile_plugin')
  assert.equal(parsed.id, STORY_ID)
  assert.equal(parsed.title, STORY_TITLE)
  assert.equal(parsed.type, 'story')
  assert.equal(parsed.environment, 'bright_faraday')
  assert.equal(parsed.created, '2026-09-23')
  assert.equal(parsed.updated, '2026-09-23')
  assert.equal(parsed.owner, '')
  assert.equal(
    parsed.description,
    "A DSH plugin that renders the sprint's stories and tasks as a kanban board, read from the org tree, with the work tree properties visible on every card.",
  )
})

test('the Story: and Task: title prefixes are stripped once', () => {
  const parsedStory = parseStoryDocument(storyText, 'dsh_agile_plugin')
  const scaffold = parseTaskDocument(scaffoldText, 'scaffold_dsh_agile_plugin')
  assert.equal(parsedStory.title.startsWith('Story:'), false)
  assert.equal(scaffold.title.startsWith('Task:'), false)
  assert.equal(scaffold.title, SCAFFOLD_TITLE)
  assert.equal(scaffold.scaffold, true)
})

test('state comes from the * Status table and from nowhere else', () => {
  const real = 'collapse_instrument_identity_into_trade'
  const text = taskFile('data-oriented-trading-model', real)
  assert.equal(statusOf(text), 'DISCOVERED')
  assert.equal(parseTaskDocument(text, real).state, 'DISCOVERED')
  assert.equal(parseStoryDocument(storyFile('dsh_agile_plugin'), 'dsh_agile_plugin').state, 'STARTED')

  // #+todo: lists the states a task may take; it is not the task's state.
  const invented = `#+title: Task: X\n#+todo: DISCOVERED BACKLOG STARTED BLOCKED | DONE ABANDONED\n\n* Status\n\n| Field | Value |\n|-------+-------|\n| State | BLOCKED |\n`
  assert.equal(parseTaskDocument(invented, 'x').state, 'BLOCKED')
  const withoutStatus = invented.replace(/^\|\s*State\s*\|.*$/m, '')
  assert.equal(parseTaskDocument(withoutStatus, 'x').state, 'UNKNOWN')
})

test('a story with no Status table is UNKNOWN, and UNKNOWN sorts last', () => {
  assert.equal(normalizeState('Started'), 'STARTED')
  assert.equal(normalizeState(' FAILED '), 'UNKNOWN')
  assert.equal(normalizeState(''), 'UNKNOWN')
  assert.equal(normalizeState(undefined), 'UNKNOWN')
  assert.deepEqual(STATE_IDS, [
    'DISCOVERED',
    'BACKLOG',
    'STARTED',
    'BLOCKED',
    'DONE',
    'ABANDONED',
    'UNKNOWN',
  ])
  // The ordering the board and the task sort both read.
  assert.deepEqual(STATE_IDS.map(columnOrder), [0, 1, 2, 3, 4, 5, 6])
})

test('a task carries its own branch, pr, owner and environment', () => {
  const implement = parseTaskDocument(implementText, 'implement_dsh_agile_plugin')
  assert.equal(implement.id, IMPLEMENT_ID)
  assert.equal(implement.title, IMPLEMENT_TITLE)
  assert.equal(implement.type, 'task')
  assert.equal(implement.branch, BRANCH)
  assert.equal(implement.pr, '')
  assert.equal(implement.owner, 'marco')
  assert.equal(implement.environment, 'bright_faraday')
  assert.equal(implement.blockedOn, '')
  assert.equal(implement.blockedSince, '')
  assert.equal(implement.created, '2026-09-23')
  assert.equal(implement.updated, '2026-09-23')
  assert.equal(implement.scaffold, false)
})

test('the sprint document carries its own dates and the story themes', () => {
  const sprint = parseSprintDocument(sprintText)
  assert.equal(sprint.title, 'Sprint 25')
  assert.equal(sprint.startDate, '2026-08-03')
  assert.equal(sprint.endDate, '2026-08-10')
  assert.equal(sprint.state, 'STARTED')
  assert.equal(sprint.epics.get(STORY_ID), 'Hotfixes')
})

test('a single-token keyword is cut at its first whitespace', () => {
  const doubled = readFileSync(join(SPRINT_DIR, 'close-systemic-codegen-gaps/story.org'), 'utf8')
  assert.equal(parseStoryDocument(doubled, 'close-systemic-codegen-gaps').environment, 'brave_hopper')
  const padded =
    '#+title: Story: X\n#+environment:   brave_hopper   brave_hopper\n#+owner: marco  rossi\n'
  const parsed = parseStoryDocument(padded, 'x')
  assert.equal(parsed.environment, 'brave_hopper')
  assert.equal(parsed.owner, 'marco')
  const multi = '#+title: Story: X\n#+description:  two   spaces   collapse\n'
  assert.equal(parseStoryDocument(multi, 'x').description, 'two spaces collapse')
})

test('the epic is the ** group heading the story row sits under', () => {
  const model = realModel()
  assert.equal(sprintStory('dsh_agile_plugin').epic, 'Hotfixes')
  assert.equal(sprintStory('acme_corporation_followups').epic, 'Product')
  assert.deepEqual(model.filters.epics, ['Hotfixes', 'Product'])
  assert.deepEqual(
    [...new Set(model.stories.map((story) => story.epic))].sort(),
    ['', 'Hotfixes', 'Product'],
  )
  // Two story directories carry no row in the sprint's Stories tables, so no
  // group heading encloses them and their epic is "".
  assert.deepEqual(
    model.stories.filter((story) => story.epic === '').map((story) => story.slug).sort(),
    ['fix_provisioning_base_bundle', 'sprint_health_review'],
  )
})

test('dayOfSprint is the true day, never clamped to the sprint span', () => {
  const model = realModel({ today: '2026-09-23' })
  assert.equal(model.sprint.startDate, '2026-08-03')
  assert.equal(model.sprint.totalDays, 8)
  assert.equal(model.sprint.dayOfSprint, 52)
  assert.ok(model.sprint.dayOfSprint > model.sprint.totalDays)
  assert.equal(realModel({ today: '2026-08-03' }).sprint.dayOfSprint, 1)
  assert.equal(realModel({ today: '2026-08-10' }).sprint.dayOfSprint, 8)
  assert.equal(realModel({ today: '2026-08-11' }).sprint.dayOfSprint, 9)
})

test('a sprint without dates reports null days, never zero', () => {
  const model = buildModel({
    doc: parseSprintDocument('#+title: Sprint 99\n'),
    dirs: [],
    options: {
      version: 'v0',
      sprintName: 'sprint_99',
      sprintPath: 'doc/agile/versions/v0/sprint_99/sprint.org',
      today: '2026-09-23',
    },
  })
  assert.equal(model.sprint.totalDays, null)
  assert.equal(model.sprint.dayOfSprint, null)
})

test('progress counts done, abandoned and total against the real task files', () => {
  const slug = 'ir-rates-followups'
  const files = taskFiles(slug)
  const states = files.map((file) => statusOf(taskFile(slug, file)))
  const story = sprintStory(slug)
  assert.equal(files.length, 20)
  assert.equal(states.filter((state) => state === 'DONE').length, 11)
  assert.equal(states.filter((state) => state === 'ABANDONED').length, 2)
  assert.deepEqual(story.progress, { done: 11, total: 20, abandoned: 2 })
})

test('the branches and prs unions drop empties and sort', () => {
  const model = realModel()
  const own = sprintStory('dsh_agile_plugin')
  assert.deepEqual(own.branches, [BRANCH])
  assert.deepEqual(own.tasks.map((task) => task.branch), [BRANCH, BRANCH])
  assert.deepEqual(own.prs, [])

  // A story whose tasks carry real PR numbers: nine tasks, nine distinct PRs,
  // one empty #+pr: dropped, and the union sorted ascending.
  const followups = sprintStory('acme_corporation_followups')
  assert.deepEqual(followups.prs, [1824, 1825, 1830, 1839, 1852, 1958, 1968, 1983, 1986])
  assert.equal(followups.tasks.filter((task) => task.pr === '').length, 1)
  assert.deepEqual(
    followups.prs,
    [...new Set(followups.tasks.map((task) => parsePrNumber(task.pr)).filter((pr) => pr !== null))].sort(
      (a, b) => a - b,
    ),
  )
  assert.deepEqual(
    followups.tasks.map((task) => task.pr).filter((pr) => pr === ''),
    [''],
  )
})

test('prs parse only from a value that is exactly #?digits', () => {
  assert.equal(parsePrNumber('2135'), 2135)
  assert.equal(parsePrNumber('#2133'), 2133)
  assert.equal(parsePrNumber(' #2133 '), 2133)
  assert.equal(parsePrNumber('none'), null)
  assert.equal(parsePrNumber(''), null)
  // Prose must not become a link to an unrelated real pull request.
  assert.equal(parsePrNumber('abc123'), null)
  assert.equal(parsePrNumber('12 and 34'), null)
  assert.equal(parsePrNumber('#12, #34'), null)
  assert.equal(parsePrNumber('PR #12'), null)
  assert.equal(parsePrNumber('12a'), null)
  assert.deepEqual(parsePrNumbers(['#1730', 'none', '', '9', '#1730', 'abc7']), [9, 1730])
})

test('tasks sort by state in table order, then by title', () => {
  const dirs = [
    {
      slug: 'sorting',
      story: '#+title: Story: Sorting\n#+type: story\n',
      tasks: [
        { slug: 'done_b', text: '#+title: Task: done_b\n\n* Status\n\n| State | DONE |\n' },
        { slug: 'blocked_a', text: '#+title: Task: blocked_a\n\n* Status\n\n| State | BLOCKED |\n' },
        { slug: 'done_a', text: '#+title: Task: done_a\n\n* Status\n\n| State | DONE |\n' },
        {
          slug: 'discovered_z',
          text: '#+title: Task: discovered_z\n\n* Status\n\n| State | DISCOVERED |\n',
        },
        { slug: 'no_status', text: '#+title: Task: no_status\n' },
      ],
    },
  ]
  const model = buildModel({
    doc: parseSprintDocument('#+title: Sprint 99\n'),
    dirs,
    options: { version: 'v0', sprintName: 'sprint_99', sprintPath: 'doc/agile/versions/v0/sprint_99/sprint.org' },
  })
  assert.deepEqual(model.stories[0].tasks.map((task) => task.slug), [
    'discovered_z',
    'blocked_a',
    'done_a',
    'done_b',
    'no_status',
  ])
  assert.deepEqual(model.stories[0].tasks.map((task) => task.state), [
    'DISCOVERED',
    'BLOCKED',
    'DONE',
    'DONE',
    'UNKNOWN',
  ])
  const real = sprintStory('dsh_agile_plugin')
  const ranked = [...real.tasks].sort((a, b) => {
    const order = columnOrder(a.state) - columnOrder(b.state)
    return order !== 0 ? order : a.title < b.title ? -1 : a.title > b.title ? 1 : 0
  })
  assert.deepEqual(real.tasks.map((task) => task.slug), ranked.map((task) => task.slug))
})

test('every story directory in the sprint becomes a card', () => {
  const model = realModel()
  const dirs = readSprintDirs()
  assert.equal(model.stories.length, 103)
  assert.equal(model.stories.length, dirs.length)
  assert.equal(model.counts.stories, 103)
  assert.deepEqual(
    model.stories.map((story) => story.slug).sort(),
    dirs.map((dir) => dir.slug).sort(),
  )
  for (const card of model.stories) {
    assert.ok(STATE_IDS.includes(card.state))
    assert.equal(card.state, statusOf(storyFile(card.slug)))
    assert.equal(card.id.length, 36)
  }
})

test('the theme comes from the ** group the row sits under, not a deeper heading', () => {
  const text = [
    '* Stories',
    '',
    '** Tooling',
    '',
    '*** Epic: DSH integration',
    '',
    '| Story | State | Start | End | Description |',
    '|-------+-------+-------+-----+-------------|',
    `| [[id:${STORY_ID}][${STORY_TITLE}]] | STARTED | | | desc |`,
    '',
    '** Hotfixes',
    '',
    '| Story | State | Start | End | Description |',
    '|-------+-------+-------+-----+-------------|',
    `| [[id:${SCAFFOLD_ID}][under hotfixes]] | DONE | | | desc |`,
    '',
    '* Achievements',
    '',
    '** Not a theme',
    '',
    '| Story | State | Start | End | Description |',
    '|-------+-------+-------+-----+-------------|',
    `| [[id:${IMPLEMENT_ID}][outside Stories]] | DONE | | | desc |`,
  ].join('\n')
  const doc = parseSprintDocument(text)
  assert.equal(doc.epics.get(STORY_ID), 'Tooling')
  assert.equal(doc.epics.get(SCAFFOLD_ID), 'Hotfixes')
  assert.equal(doc.epics.has(IMPLEMENT_ID), false)
  const bare = parseSprintDocument(sprintText)
  const uniqueIds = new Set(
    [...sprintText.matchAll(/^\|\s*\[\[id:([0-9A-Fa-f-]+)\]/gm)].map((match) => match[1].toUpperCase()),
  )
  assert.equal(bare.epics.size, uniqueIds.size)
  assert.equal(sprintStory('dsh_agile_plugin').epic, 'Hotfixes')
})

test('the columns, counts and filters describe the whole sprint', () => {
  const model = realModel()
  assert.deepEqual(model.sprint, {
    version: 'v0',
    name: 'sprint_25',
    title: 'Sprint 25',
    startDate: '2026-08-03',
    endDate: '2026-08-10',
    dayOfSprint: 52,
    totalDays: 8,
    path: SPRINT_PATH,
  })
  assert.deepEqual(model.columns, [
    { id: 'BACKLOG', title: 'Backlog', count: 7 },
    { id: 'STARTED', title: 'Started', count: 32 },
    { id: 'BLOCKED', title: 'Blocked', count: 1 },
    { id: 'DONE', title: 'Done', count: 63 },
    { id: 'ABANDONED', title: 'Abandoned', count: 0 },
  ])
  assert.deepEqual(model.counts, {
    stories: 103,
    storiesDone: 63,
    storiesStarted: 32,
    storiesBlocked: 1,
    tasks: 447,
    tasksDone: 314,
  })
  assert.equal(
    model.columns.reduce((total, column) => total + column.count, 0),
    model.counts.stories,
  )
  assert.deepEqual(model.filters.epics, ['Hotfixes', 'Product'])
  assert.deepEqual(model.filters.environments, [
    'brave_hopper',
    'bright_faraday',
    'clever_dijkstra',
    'eager_maxwell',
    'festive_dijkstra',
    'jolly_knuth',
    'merry_newton',
    'prime_origin',
    'solid_dirac',
    'swift_curie',
  ])
})

test('DISCOVERED cards fold into the BACKLOG column', () => {
  const model = buildModel({
    doc: parseSprintDocument(sprintText),
    dirs: [
      { slug: 'a', story: '#+title: Story: A\n\n* Status\n\n| State | DISCOVERED |\n', tasks: [] },
      { slug: 'b', story: '#+title: Story: B\n\n* Status\n\n| State | BACKLOG |\n', tasks: [] },
    ],
    options: { version: 'v0', sprintName: 'sprint_25', sprintPath: SPRINT_PATH, today: '2026-09-23' },
  })
  assert.deepEqual(model.stories.map((story) => story.state), ['DISCOVERED', 'BACKLOG'])
  assert.deepEqual(model.columns, [
    { id: 'BACKLOG', title: 'Backlog', count: 2 },
    { id: 'STARTED', title: 'Started', count: 0 },
    { id: 'BLOCKED', title: 'Blocked', count: 0 },
    { id: 'DONE', title: 'Done', count: 0 },
    { id: 'ABANDONED', title: 'Abandoned', count: 0 },
  ])
})

test('the UNKNOWN column appears only when a card lands in it', () => {
  assert.equal(realModel().columns.some((column) => column.id === 'UNKNOWN'), false)
  assert.deepEqual(realModel().columns.map((column) => column.id), [
    'BACKLOG',
    'STARTED',
    'BLOCKED',
    'DONE',
    'ABANDONED',
  ])
  const model = buildModel({
    doc: parseSprintDocument(sprintText),
    dirs: [{ slug: 'a', story: '#+title: Story: A\n#+type: story\n', tasks: [] }],
    options: { version: 'v0', sprintName: 'sprint_25', sprintPath: SPRINT_PATH, today: '2026-09-23' },
  })
  assert.equal(model.stories[0].state, 'UNKNOWN')
  assert.deepEqual(model.columns, [
    { id: 'BACKLOG', title: 'Backlog', count: 0 },
    { id: 'STARTED', title: 'Started', count: 0 },
    { id: 'BLOCKED', title: 'Blocked', count: 0 },
    { id: 'DONE', title: 'Done', count: 0 },
    { id: 'ABANDONED', title: 'Abandoned', count: 0 },
    { id: 'UNKNOWN', title: 'Unknown', count: 1 },
  ])
})

test('every field the model exposes is populated from the files, not defaulted', () => {
  const model = realModel()
  const own = sprintStory('dsh_agile_plugin')
  assert.equal(own.description.length > 40, true)
  assert.equal(own.title, STORY_TITLE)
  assert.equal(own.slug, 'dsh_agile_plugin')
  assert.equal(own.environment, 'bright_faraday')
  assert.equal(own.created, '2026-09-23')
  assert.equal(own.updated, '2026-09-23')
  assert.equal(own.path, 'doc/agile/versions/v0/sprint_25/dsh_agile_plugin/story.org')
  assert.equal(own.tasks.length, 2)
  for (const card of own.tasks) {
    assert.notEqual(card.id, '')
    assert.notEqual(card.title, '')
    assert.notEqual(card.owner, '')
    assert.notEqual(card.environment, '')
    assert.notEqual(card.created, '')
    assert.notEqual(card.updated, '')
    assert.equal(card.path, `doc/agile/versions/v0/sprint_25/dsh_agile_plugin/task_${card.slug}.org`)
    assert.equal(card.state, statusOf(taskFile('dsh_agile_plugin', card.slug)))
  }
  const scaffold = own.tasks.find((card) => card.slug === 'scaffold_dsh_agile_plugin')
  assert.equal(scaffold.id, SCAFFOLD_ID)
  assert.equal(scaffold.title, SCAFFOLD_TITLE)
  assert.equal(scaffold.scaffold, true)
  assert.equal(model.sprint.title, 'Sprint 25')
  assert.deepEqual(model.counts, {
    stories: 103,
    storiesDone: 63,
    storiesStarted: 32,
    storiesBlocked: 1,
    tasks: 447,
    tasksDone: 314,
  })
})

test('environments that appear only on tasks still reach the filter', () => {
  const model = buildModel({
    doc: parseSprintDocument('#+title: Sprint 99\n'),
    dirs: [
      {
        slug: 'a',
        story: '#+title: Story: A\n#+environment: bright_faraday\n',
        tasks: [
          {
            slug: 'b',
            text: '#+title: Task: B\n#+environment: eager_maxwell\n\n* Status\n\n| State | DONE |\n',
          },
          {
            slug: 'c',
            text: '#+title: Task: C\n#+environment: eager_maxwell\n\n* Status\n\n| State | DONE |\n',
          },
        ],
      },
      { slug: 'd', story: '#+title: Story: D\n', tasks: [] },
    ],
    options: { version: 'v0', sprintName: 'sprint_99', sprintPath: 'doc/agile/versions/v0/sprint_99/sprint.org' },
  })
  assert.deepEqual(model.filters.environments, ['bright_faraday', 'eager_maxwell'])
  assert.deepEqual(model.stories.find((story) => story.slug === 'a').progress, {
    done: 2,
    total: 2,
    abandoned: 0,
  })
})

test('the work item resolves by branch, then by journal task id', () => {
  const entry = { date: '2026-09-23 11:09', taskId: IMPLEMENT_ID }
  const scaffoldEntry = { date: '2026-09-23 10:56', taskId: SCAFFOLD_ID }
  assert.deepEqual(resolveItem(realTasks(), BRANCH, entry), {
    storyId: STORY_ID,
    taskId: IMPLEMENT_ID,
    by: 'branch',
  })
  // The journal wins over the fallback even when its task is the earlier one.
  assert.deepEqual(resolveItem(realTasks(), BRANCH, scaffoldEntry), {
    storyId: STORY_ID,
    taskId: SCAFFOLD_ID,
    by: 'branch',
  })
  assert.deepEqual(resolveItem(realTasks(), 'feature/nothing-here', entry), {
    storyId: '',
    taskId: '',
    by: 'none',
  })
  assert.deepEqual(resolveItem(realTasks(), '', entry), { storyId: '', taskId: '', by: 'none' })
})

test('with no journal it takes the latest update, then the first path', () => {
  const tasks = [
    task({ id: 'B', slug: 'b', branch: 'feature/x', updated: '2026-09-01', path: 'z/task_b.org' }),
    task({ id: 'C', slug: 'c', branch: 'feature/x', updated: '2026-09-05', path: 'a/task_c.org' }),
    task({ id: 'D', slug: 'd', branch: 'feature/x', updated: '2026-09-05', path: 'b/task_d.org' }),
    task({ id: 'E', slug: 'e', branch: 'feature/other', updated: '2026-09-09', path: 'a/task_e.org' }),
  ]
  // Latest update wins; `c` and `d` share one, and the first path decides.
  assert.deepEqual(resolveItem(tasks, 'feature/x', null), { storyId: '', taskId: 'C', by: 'branch' })
  assert.deepEqual(resolveItem(tasks, 'feature/x', { taskId: 'not-a-candidate' }), {
    storyId: '',
    taskId: 'C',
    by: 'branch',
  })
  // A journal that names a candidate beats both.
  assert.deepEqual(resolveItem(tasks, 'feature/x', { taskId: 'B' }), {
    storyId: '',
    taskId: 'B',
    by: 'branch',
  })
  assert.deepEqual(resolveItem(tasks, 'feature/absent', null), { storyId: '', taskId: '', by: 'none' })
})

test('the tree object describes the session work tree and its own item', () => {
  const model = realModel({
    tree: { ...TREE, entry: { taskId: IMPLEMENT_ID } },
    source: 'session',
  })
  assert.deepEqual(model.tree, {
    root: '/w/ores_dev_bright_faraday',
    name: 'ores_dev_bright_faraday',
    label: 'bright_faraday',
    branch: BRANCH,
    detached: false,
    dirty: false,
    currentStoryId: STORY_ID,
    currentTaskId: IMPLEMENT_ID,
    by: 'branch',
    source: 'session',
  })
  assert.equal('session' in model, false)
  assert.equal('isSession' in model.tree, false)
  assert.equal(realModel({ tree: { ...TREE }, source: 'cwd' }).tree.source, 'cwd')
})

test('tree.by separates sprint-only from none', () => {
  const matched = realModel({ tree: { ...TREE, entry: null } })
  assert.equal(matched.tree.by, 'branch')

  const noSprint = buildModel({
    doc: parseSprintDocument('#+title: Sprint 99\n'),
    dirs: [],
    options: {
      version: 'v0',
      sprintName: '',
      sprintPath: '',
      tree: { ...TREE, entry: null },
      source: 'session',
    },
  })
  assert.equal(noSprint.tree.by, 'none')

  const sprintOnly = realModel({ tree: { ...TREE, branch: 'feature/nothing-here', entry: null } })
  assert.equal(sprintOnly.tree.by, 'sprint-only')
  assert.equal(sprintOnly.tree.currentStoryId, '')
  assert.equal(sprintOnly.tree.currentTaskId, '')
})

test('trees lists every work tree, sorted, each with its own work item', () => {
  const trees = [
    {
      ...TREE,
      root: '/w/ores_dev_brave_hopper',
      name: 'ores_dev_brave_hopper',
      label: 'brave_hopper',
      branch: 'feature/health-review-2',
      entry: null,
    },
    { ...TREE, entry: { taskId: IMPLEMENT_ID } },
    {
      name: 'ores_dev_jolly_knuth',
      root: '/w/ores_dev_jolly_knuth',
      label: 'jolly_knuth',
      branch: '',
      entry: null,
    },
  ]
  const model = realModel({ tree: { ...TREE, entry: { taskId: IMPLEMENT_ID } }, trees })
  assert.deepEqual(model.trees.map((tree) => tree.label), [
    'brave_hopper',
    'bright_faraday',
    'jolly_knuth',
  ])
  assert.deepEqual(model.trees[0], {
    label: 'brave_hopper',
    name: 'ores_dev_brave_hopper',
    root: '/w/ores_dev_brave_hopper',
    branch: 'feature/health-review-2',
    isSession: false,
    currentStoryId: '',
    currentTaskId: '',
    storyTitle: '',
    taskTitle: '',
    state: 'UNKNOWN',
    pr: '',
  })
  assert.equal(model.trees[1].isSession, true)
  assert.equal(model.trees[1].currentStoryId, STORY_ID)
  assert.equal(model.trees[1].currentTaskId, IMPLEMENT_ID)
  assert.equal(model.trees[1].storyTitle, STORY_TITLE)
  assert.equal(model.trees[1].taskTitle, IMPLEMENT_TITLE)
  assert.equal(model.trees[1].state, statusOf(implementText))
  assert.equal(model.trees[2].label, 'jolly_knuth')
  // The fleet is a report: it never decides what the board shows.
  assert.equal(model.tree.label, 'bright_faraday')
  assert.equal(model.stories.length, 103)
})

test('the trees list is capped at 40 rows', () => {
  const trees = Array.from({ length: 45 }, (unused, index) => ({
    label: `t${String(index).padStart(2, '0')}`,
    name: `ores_dev_t${index}`,
    root: `/w/ores_dev_t${index}`,
    branch: 'feature/x',
    entry: null,
  }))
  const model = realModel({ tree: { ...TREE, entry: null }, trees })
  assert.equal(model.trees.length, 40)
  assert.equal(model.trees[0].label, 't00')
  assert.equal(model.trees[39].label, 't39')
  assert.equal(model.trees.some((tree) => tree.label === 't44'), false)
})

test('a work tree whose journal names a task this sprint lacks still reports it', () => {
  const model = realModel({
    tree: { ...TREE, entry: null },
    trees: [
      {
        ...TREE,
        label: 'brave_hopper',
        name: 'ores_dev_brave_hopper',
        root: '/w/ores_dev_brave_hopper',
        branch: 'feature/health-review-2',
        entry: {
          taskId: ELSEWHERE_TASK_ID,
          storyTitle: 'Sprint 25 closure: story cleanup and reset for sprint 26',
          taskTitle: 'Clean up the sprint 25 stories for close',
          state: 'DONE',
          pr: '2133',
        },
      },
    ],
  })
  assert.deepEqual(model.trees[0], {
    label: 'brave_hopper',
    name: 'ores_dev_brave_hopper',
    root: '/w/ores_dev_brave_hopper',
    branch: 'feature/health-review-2',
    isSession: false,
    currentStoryId: '',
    currentTaskId: ELSEWHERE_TASK_ID,
    storyTitle: 'Sprint 25 closure: story cleanup and reset for sprint 26',
    taskTitle: 'Clean up the sprint 25 stories for close',
    state: 'DONE',
    pr: '2133',
  })
})

test('a journal PR reaches the row, and none reads as an empty string', () => {
  const model = realModel({
    tree: { ...TREE, entry: null },
    trees: [
      {
        ...TREE,
        label: 'a',
        root: '/w/ores_dev_a',
        entry: { taskId: ELSEWHERE_TASK_ID, state: 'DONE', pr: parsePrNumber('#2103') },
      },
      { ...TREE, label: 'b', root: '/w/ores_dev_b', entry: { taskId: ELSEWHERE_TASK_ID, state: 'DONE', pr: '' } },
    ],
  })
  assert.equal(model.trees[0].pr, '2103')
  assert.equal(model.trees[1].pr, '')
})

test('a work tree with no journal still gets a row', () => {
  const model = realModel({
    tree: { ...TREE, entry: null },
    trees: [
      {
        label: 'prime_origin',
        name: 'ores_dev_prime_origin',
        root: '/w/ores_dev_prime_origin',
        branch: 'main',
        entry: null,
      },
    ],
  })
  assert.deepEqual(model.trees, [
    {
      label: 'prime_origin',
      name: 'ores_dev_prime_origin',
      root: '/w/ores_dev_prime_origin',
      branch: 'main',
      isSession: false,
      currentStoryId: '',
      currentTaskId: '',
      storyTitle: '',
      taskTitle: '',
      state: 'UNKNOWN',
      pr: '',
    },
  ])
  // The row carries no working-state fields: nothing renders them.
  assert.equal('dirty' in model.trees[0], false)
  assert.equal('detached' in model.trees[0], false)
})

test('a journal entry parses into the fields the tree row carries', () => {
  const entry = parseJournalEntry(
    [
      `* 2026-09-23 11:09 — [[id:${STORY_ID}][${STORY_TITLE}]]`,
      `  - Task :: [[id:${IMPLEMENT_ID}][${IMPLEMENT_TITLE}]]`,
      '  - State :: STARTED',
      `  - Branch :: ${BRANCH}`,
      '  - PR :: #2135',
    ].join('\n'),
  )
  assert.deepEqual(entry, {
    date: '2026-09-23 11:09',
    storyId: STORY_ID,
    storyTitle: STORY_TITLE,
    taskId: IMPLEMENT_ID,
    taskTitle: IMPLEMENT_TITLE,
    state: 'STARTED',
    branch: BRANCH,
    pr: '2135',
  })
})
