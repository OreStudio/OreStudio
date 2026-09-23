import assert from 'node:assert/strict'
import test from 'node:test'
import { existsSync, readFileSync, readdirSync } from 'node:fs'
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

// Two sources of truth, chosen by what the assertion can survive.
//
// `test/fixtures/` holds verbatim copies of the agile documents taken from this
// work tree. Every literal expected value is read from a fixture: CI checks out
// the pull request merged with the base branch, so the live sprint directory is
// whatever other work has made it, and a number read from there fails for
// reasons that have nothing to do with this plugin.
//
// The live directory is still read, but only for invariants that hold whatever
// the data says. The live block at the foot of this file skips itself when the
// directory is not present, so the fixture assertions stay hermetic.
const HERE = dirname(fileURLToPath(import.meta.url))
const FIXTURE_DIR = join(HERE, 'fixtures/sprint_25')
const DECK_DIR = join(HERE, 'fixtures/card_deck')
const SPRINT_PATH = 'doc/agile/versions/v0/sprint_25/sprint.org'
const LIVE_DIR = join(HERE, '..', '..', '..', 'doc/agile/versions/v0/sprint_25')

const STORY_ID = '3085B911-3030-4AB8-976F-0F037D4332E7'
const SCAFFOLD_ID = '8A81CB08-A606-4E30-A717-88B810308137'
const IMPLEMENT_ID = '2612E669-545E-4482-8115-E9C1A3278770'
const STORY_TITLE = 'Show the current agile work item inside DSH'
const SCAFFOLD_TITLE = `Scaffold story: ${STORY_TITLE}`
const IMPLEMENT_TITLE = 'Build the ORE Studio kanban plugin for DSH'
const BRANCH = 'feature/dsh-agile-plugin'

const BRANCHABLE = 'ir-rates-followups'
const PR_STORY = 'acme_corporation_followups'
const PR_STORY_PRS = [1824, 1825, 1830, 1839, 1852, 1958, 1968, 1983, 1986]

const TREE = {
  root: '/w/ores_dev_bright_faraday',
  name: 'ores_dev_bright_faraday',
  label: 'bright_faraday',
  branch: BRANCH,
  detached: false,
  dirty: false,
}

function fixtureDir(root, slug) {
  return {
    slug,
    story: readFileSync(join(root, slug, 'story.org'), 'utf8'),
    tasks: readdirSync(join(root, slug))
      .map((name) => /^task_(.+)\.org$/.exec(name))
      .filter((match) => match !== null)
      .map((match) => ({ slug: match[1], text: readFileSync(join(root, slug, match[0]), 'utf8') })),
  }
}

function dirsOf(root) {
  return readdirSync(root, { withFileTypes: true })
    .filter((entry) => entry.isDirectory())
    .map((entry) => fixtureDir(root, entry.name))
}

function fixtureModel(options = {}) {
  return buildModel({
    doc: parseSprintDocument(fixtureText('sprint.org')),
    dirs: dirsOf(FIXTURE_DIR),
    options: {
      version: 'v0',
      sprintName: 'sprint_25',
      sprintPath: SPRINT_PATH,
      today: '2026-09-23',
      ...options,
    },
  })
}

function fixtureText(...parts) {
  return readFileSync(join(FIXTURE_DIR, ...parts), 'utf8')
}

function fixtureTasks() {
  return fixtureModel().stories.flatMap((story) => story.tasks)
}

function fixtureStory(slug) {
  return fixtureModel().stories.find((story) => story.slug === slug)
}

function liveModel(options = {}) {
  if (!existsSync(LIVE_DIR)) return null
  return buildModel({
    doc: parseSprintDocument(readFileSync(join(LIVE_DIR, 'sprint.org'), 'utf8')),
    dirs: dirsOf(LIVE_DIR),
    options: { version: 'v0', sprintName: 'sprint_25', sprintPath: SPRINT_PATH, ...options },
  })
}

// The `#+keyword:` value as the file literally carries it.
function keywordOf(text, name) {
  const row = new RegExp(`^#\\+${name}:[ \\t]*(.*)$`, 'm').exec(text)
  return row ? row[1].trim() : ''
}

// A single-token keyword is read up to its first whitespace, because upstream
// carries `#+environment: brave_hopper brave_hopper`.
function firstTokenOf(text, name) {
  return (keywordOf(text, name).replace(/\s+/g, ' ').split(' ')[0] ?? '')
}

// The Status row as the file literally carries it, which is the only thing an
// assertion about `state` may be compared against.
function statusOf(text) {
  const row = /^\|\s*State\s*\|(.*)\|$/m.exec(text)
  return row ? row[1].trim().replace(/\s+/g, ' ').toUpperCase() : 'UNKNOWN'
}

// The `**` group heading the story's row sits under in the sprint document.
function epicGroupOf(text, id) {
  let group = ''
  let inStories = false
  for (const line of text.split(/\r?\n/)) {
    const heading = /^(\*+)\s+(.*)$/.exec(line)
    if (heading) {
      if (heading[1].length === 1) inStories = heading[2].trim().toLowerCase() === 'stories'
      else if (inStories && heading[1].length === 2) group = heading[2].trim()
      continue
    }
    if (inStories && line.includes(`[[id:${id}]`)) return group
  }
  return ''
}

function task(overrides = {}) {
  return {
    id: '00000000-0000-0000-0000-00000000000B',
    slug: 'b',
    title: 'A task',
    state: 'BACKLOG',
    branch: '',
    pr: '',
    environment: '',
    created: '',
    updated: '',
    path: '',
    storyId: '',
    scaffold: false,
    ...overrides,
  }
}

test('a story carries the literal fields of its own org file', () => {
  const parsed = parseStoryDocument(fixtureText('dsh_agile_plugin', 'story.org'), 'dsh_agile_plugin')
  assert.equal(parsed.id, STORY_ID)
  assert.equal(parsed.title, STORY_TITLE)
  assert.equal(parsed.type, 'story')
  assert.equal(parsed.environment, 'bright_faraday')
  assert.equal(parsed.created, '2026-09-23')
  assert.equal(parsed.updated, '2026-09-23')
  assert.equal(
    parsed.description,
    "A DSH plugin that renders the sprint's stories and tasks as a kanban board, read from the org tree, with the work tree properties visible on every card.",
  )
})

test('the Story: and Task: title prefixes are stripped once', () => {
  const parsedStory = parseStoryDocument(fixtureText('dsh_agile_plugin', 'story.org'), 'dsh_agile_plugin')
  const scaffold = parseTaskDocument(
    fixtureText('dsh_agile_plugin', 'task_scaffold_dsh_agile_plugin.org'),
    'scaffold_dsh_agile_plugin',
  )
  assert.equal(parsedStory.title.startsWith('Story:'), false)
  assert.equal(scaffold.title.startsWith('Task:'), false)
  assert.equal(scaffold.title, SCAFFOLD_TITLE)
  assert.equal(scaffold.scaffold, true)
})

test('state comes from the * Status table and from nowhere else', () => {
  const real = 'collapse_instrument_identity_into_trade'
  const text = fixtureText('data-oriented-trading-model', `task_${real}.org`)
  assert.equal(statusOf(text), 'DISCOVERED')
  assert.equal(parseTaskDocument(text, real).state, 'DISCOVERED')
  assert.equal(
    parseStoryDocument(fixtureText('dsh_agile_plugin', 'story.org'), 'dsh_agile_plugin').state,
    'DONE',
  )

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

test('a task carries its own branch, pr and environment', () => {
  const implement = parseTaskDocument(
    fixtureText('dsh_agile_plugin', 'task_implement_dsh_agile_plugin.org'),
    'implement_dsh_agile_plugin',
  )
  assert.equal(implement.id, IMPLEMENT_ID)
  assert.equal(implement.title, IMPLEMENT_TITLE)
  assert.equal(implement.type, 'task')
  assert.equal(implement.branch, BRANCH)
  assert.equal(implement.pr, '2135')
  assert.equal(implement.environment, 'bright_faraday')
  assert.equal(implement.blockedOn, '')
  assert.equal(implement.blockedSince, '')
  assert.equal(implement.created, '2026-09-23')
  assert.equal(implement.updated, '2026-09-23')
  assert.equal(implement.scaffold, false)
})

test('the sprint document carries its own dates and the story themes', () => {
  const sprint = parseSprintDocument(fixtureText('sprint.org'))
  assert.equal(sprint.title, 'Sprint 25')
  assert.equal(sprint.startDate, '2026-08-03')
  assert.equal(sprint.endDate, '2026-08-10')
  assert.equal(sprint.state, 'STARTED')
  assert.equal(sprint.epics.get(STORY_ID), 'Hotfixes')
})

// The upstream doubled token is preserved in the fixture, because it is the only
// shape that exercises the cut.
test('a single-token keyword is cut at its first whitespace', () => {
  const doubled = parseStoryDocument(fixtureText('close-systemic-codegen-gaps', 'story.org'), 'x')
  assert.equal(doubled.environment, 'brave_hopper')
  const padded = '#+title: Story: X\n#+environment:   brave_hopper   brave_hopper\n'
  assert.equal(parseStoryDocument(padded, 'x').environment, 'brave_hopper')
  const multi = '#+title: Story: X\n#+description:  two   spaces   collapse\n'
  assert.equal(parseStoryDocument(multi, 'x').description, 'two spaces collapse')
})

test('the epic is the ** group heading the story row sits under', () => {
  const sprintText = fixtureText('sprint.org')
  const model = fixtureModel()
  assert.equal(fixtureStory('dsh_agile_plugin').epic, epicGroupOf(sprintText, STORY_ID))
  assert.equal(fixtureStory('dsh_agile_plugin').epic, 'Hotfixes')
  assert.equal(fixtureStory(PR_STORY).epic, 'Product')
  assert.deepEqual(model.filters.epics, ['Hotfixes', 'Product'])
  // Every story row in the sprint document is under a theme, and no story card
  // carries a theme the document does not name.
  assert.deepEqual(
    [...new Set(model.stories.map((story) => story.epic))].sort(),
    ['Hotfixes', 'Product'],
  )

  const synthetic = [
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
  const doc = parseSprintDocument(synthetic)
  assert.equal(doc.epics.get(STORY_ID), 'Tooling')
  assert.equal(doc.epics.get(SCAFFOLD_ID), 'Hotfixes')
  assert.equal(doc.epics.has(IMPLEMENT_ID), false)
  const uniqueIds = new Set(
    [...sprintText.matchAll(/^\|\s*\[\[id:([0-9A-Fa-f-]+)\]/gm)].map((match) => match[1].toUpperCase()),
  )
  assert.equal(parseSprintDocument(sprintText).epics.size, uniqueIds.size)
})

test('dayOfSprint is the true day, never clamped to the sprint span', () => {
  const model = fixtureModel({ today: '2026-09-23' })
  assert.equal(model.sprint.startDate, '2026-08-03')
  assert.equal(model.sprint.totalDays, 8)
  assert.equal(model.sprint.dayOfSprint, 52)
  assert.ok(model.sprint.dayOfSprint > model.sprint.totalDays)
  assert.equal(fixtureModel({ today: '2026-08-03' }).sprint.dayOfSprint, 1)
  assert.equal(fixtureModel({ today: '2026-08-10' }).sprint.dayOfSprint, 8)
  assert.equal(fixtureModel({ today: '2026-08-11' }).sprint.dayOfSprint, 9)
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

test('progress counts done, abandoned and total against the story task files', () => {
  const dir = fixtureDir(FIXTURE_DIR, BRANCHABLE)
  const states = dir.tasks.map((task) => statusOf(task.text))
  const story = fixtureStory(BRANCHABLE)
  assert.equal(dir.tasks.length, 20)
  assert.equal(states.filter((state) => state === 'DONE').length, 11)
  assert.equal(states.filter((state) => state === 'ABANDONED').length, 2)
  assert.deepEqual(story.progress, { done: 11, total: 20, abandoned: 2 })
})

test('the branches and prs unions drop empties and sort', () => {
  const own = fixtureStory('dsh_agile_plugin')
  assert.deepEqual(own.branches, [BRANCH])
  assert.deepEqual(own.tasks.map((task) => task.branch), [BRANCH, BRANCH])
  assert.deepEqual(own.prs, [2135])

  // Nine tasks carry a real PR number, one file is empty, and two tasks share
  // PR 1958, so the union is sorted, deduplicated and free of the empty value.
  const followups = fixtureStory(PR_STORY)
  assert.deepEqual(followups.prs, PR_STORY_PRS)
  assert.equal(followups.tasks.length, 11)
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
  const real = fixtureStory('dsh_agile_plugin')
  const ranked = [...real.tasks].sort((a, b) => {
    const order = columnOrder(a.state) - columnOrder(b.state)
    return order !== 0 ? order : a.title < b.title ? -1 : a.title > b.title ? 1 : 0
  })
  assert.deepEqual(real.tasks.map((task) => task.slug), ranked.map((task) => task.slug))
})

test('every story directory in the fixture deck becomes exactly one card', () => {
  const dirs = dirsOf(DECK_DIR)
  const model = buildModel({
    doc: parseSprintDocument(fixtureText('sprint.org')),
    dirs,
    options: { version: 'v0', sprintName: 'sprint_25', sprintPath: SPRINT_PATH, today: '2026-09-23' },
  })
  assert.equal(model.stories.length, dirs.length)
  assert.equal(model.counts.stories, dirs.length)
  assert.deepEqual(
    model.stories.map((story) => story.slug).sort(),
    dirs.map((dir) => dir.slug).sort(),
  )
  for (const card of model.stories) {
    assert.ok(STATE_IDS.includes(card.state))
    assert.equal(card.state, statusOf(readFileSync(join(DECK_DIR, card.slug, 'story.org'), 'utf8')))
    assert.ok(card.id.length > 0)
    assert.ok(card.title.length > 0)
  }
})

test('the columns are the state table, and count the cards', () => {
  const model = fixtureModel()
  assert.deepEqual(model.columns, [
    { id: 'BACKLOG', title: 'Backlog', count: 0 },
    { id: 'STARTED', title: 'Started', count: 3 },
    { id: 'BLOCKED', title: 'Blocked', count: 0 },
    { id: 'DONE', title: 'Done', count: 2 },
    { id: 'ABANDONED', title: 'Abandoned', count: 0 },
  ])
  assert.equal(
    model.columns.reduce((total, column) => total + column.count, 0),
    model.counts.stories,
  )
})

test('the columns, counts and filters describe the fixture sprint', () => {
  const model = fixtureModel()
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
  assert.deepEqual(model.counts, {
    stories: 5,
    storiesDone: 2,
    storiesStarted: 3,
    storiesBlocked: 0,
    tasks: 55,
    tasksDone: 37,
  })
  assert.equal(
    model.columns.reduce((total, column) => total + column.count, 0),
    model.counts.stories,
  )
  assert.deepEqual(
    model.filters.epics,
    [...new Set(model.stories.map((story) => story.epic))].filter((epic) => epic !== '').sort(),
  )
  assert.deepEqual(model.filters.epics, ['Hotfixes', 'Product'])
  assert.deepEqual(model.filters.environments, [
    'brave_hopper',
    'bright_faraday',
    'eager_maxwell',
    'merry_newton',
    'prime_origin',
  ])
})

test('DISCOVERED cards fold into the BACKLOG column', () => {
  const model = buildModel({
    doc: parseSprintDocument(fixtureText('sprint.org')),
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
  assert.equal(fixtureModel().columns.some((column) => column.id === 'UNKNOWN'), false)
  const model = buildModel({
    doc: parseSprintDocument(fixtureText('sprint.org')),
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
  const own = fixtureStory('dsh_agile_plugin')
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
    assert.notEqual(card.environment, '')
    assert.notEqual(card.created, '')
    assert.notEqual(card.updated, '')
    assert.equal(card.path, `doc/agile/versions/v0/sprint_25/dsh_agile_plugin/task_${card.slug}.org`)
    assert.equal(card.state, statusOf(fixtureText('dsh_agile_plugin', `task_${card.slug}.org`)))
  }
  const scaffold = own.tasks.find((card) => card.slug === 'scaffold_dsh_agile_plugin')
  assert.equal(scaffold.id, SCAFFOLD_ID)
  assert.equal(scaffold.title, SCAFFOLD_TITLE)
  assert.equal(scaffold.scaffold, true)
  assert.equal(fixtureModel().sprint.title, 'Sprint 25')
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
  assert.deepEqual(resolveItem(fixtureTasks(), BRANCH, null), {
    storyId: STORY_ID,
    taskId: IMPLEMENT_ID,
    by: 'branch',
  })
  assert.deepEqual(resolveItem(fixtureTasks(), BRANCH, entry), {
    storyId: STORY_ID,
    taskId: IMPLEMENT_ID,
    by: 'branch',
  })
  // The journal wins over the fallback even when its task is the earlier one.
  assert.deepEqual(resolveItem(fixtureTasks(), BRANCH, scaffoldEntry), {
    storyId: STORY_ID,
    taskId: SCAFFOLD_ID,
    by: 'branch',
  })
  assert.deepEqual(resolveItem(fixtureTasks(), 'feature/nothing-here', entry), {
    storyId: '',
    taskId: '',
    by: 'none',
  })
  assert.deepEqual(resolveItem(fixtureTasks(), '', entry), { storyId: '', taskId: '', by: 'none' })
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
  const model = fixtureModel({
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
  assert.equal(fixtureModel({ tree: { ...TREE }, source: 'cwd' }).tree.source, 'cwd')
})

test('tree.by separates sprint-only from none', () => {
  const matched = fixtureModel({ tree: { ...TREE, entry: null } })
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

  const sprintOnly = fixtureModel({ tree: { ...TREE, branch: 'feature/nothing-here', entry: null } })
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
  const model = fixtureModel({ tree: { ...TREE, entry: { taskId: IMPLEMENT_ID } }, trees })
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
  assert.equal(
    model.trees[1].state,
    statusOf(fixtureText('dsh_agile_plugin', 'task_implement_dsh_agile_plugin.org')),
  )
  assert.equal(model.trees[2].label, 'jolly_knuth')
  // The fleet is a report: it never decides what the board shows.
  assert.equal(model.tree.label, 'bright_faraday')
  assert.equal(model.stories.length, dirsOf(FIXTURE_DIR).length)
})

test('the trees list is capped at 40 rows', () => {
  const trees = Array.from({ length: 45 }, (unused, index) => ({
    label: `t${String(index).padStart(2, '0')}`,
    name: `ores_dev_t${index}`,
    root: `/w/ores_dev_t${index}`,
    branch: 'feature/x',
    entry: null,
  }))
  const model = fixtureModel({ tree: { ...TREE, entry: null }, trees })
  assert.equal(model.trees.length, 40)
  assert.equal(model.trees[0].label, 't00')
  assert.equal(model.trees[39].label, 't39')
  assert.equal(model.trees.some((tree) => tree.label === 't44'), false)
})

test('a work tree whose journal names a task this sprint lacks still reports it', () => {
  const model = fixtureModel({
    tree: { ...TREE, entry: null },
    trees: [
      {
        ...TREE,
        label: 'brave_hopper',
        name: 'ores_dev_brave_hopper',
        root: '/w/ores_dev_brave_hopper',
        branch: 'feature/health-review-2',
        entry: {
          taskId: '826C9671-AE0D-48CD-B8E6-666DF92DB4AF',
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
    currentTaskId: '826C9671-AE0D-48CD-B8E6-666DF92DB4AF',
    storyTitle: 'Sprint 25 closure: story cleanup and reset for sprint 26',
    taskTitle: 'Clean up the sprint 25 stories for close',
    state: 'DONE',
    pr: '2133',
  })
})

test('a journal PR reaches the row, and none reads as an empty string', () => {
  const model = fixtureModel({
    tree: { ...TREE, entry: null },
    trees: [
      {
        ...TREE,
        label: 'a',
        root: '/w/ores_dev_a',
        entry: { taskId: '826C9671-AE0D-48CD-B8E6-666DF92DB4AF', state: 'DONE', pr: parsePrNumber('#2103') },
      },
      {
        ...TREE,
        label: 'b',
        root: '/w/ores_dev_b',
        entry: { taskId: '826C9671-AE0D-48CD-B8E6-666DF92DB4AF', state: 'DONE', pr: '' },
      },
    ],
  })
  assert.equal(model.trees[0].pr, '2103')
  assert.equal(model.trees[1].pr, '')
})

test('a work tree with no journal still gets a row', () => {
  const model = fixtureModel({
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

// The live sprint directory is a moving document: other work trees close, start
// and abandon stories in it while this suite runs. Everything below holds for
// any data, so it proves the parser against the real tree without failing on an
// edit it did not make. The block skips itself when the tree is absent.
test('every story directory in the live sprint becomes exactly one card', (t) => {
  if (!existsSync(LIVE_DIR)) {
    t.skip('the live sprint directory is not present')
    return
  }
  const model = liveModel()
  const dirs = dirsOf(LIVE_DIR)
  assert.equal(model.stories.length, dirs.length)
  assert.equal(model.counts.stories, dirs.length)
  assert.deepEqual(
    model.stories.map((story) => story.slug).sort(),
    dirs.map((dir) => dir.slug).sort(),
  )
  for (const card of model.stories) {
    assert.ok(STATE_IDS.includes(card.state))
    assert.equal(card.state, statusOf(fixtureDir(LIVE_DIR, card.slug).story))
    assert.notEqual(card.id, '')
    assert.notEqual(card.title, '')
  }
})

test('every live card and task carries the fields the board renders', (t) => {
  if (!existsSync(LIVE_DIR)) {
    t.skip('the live sprint directory is not present')
    return
  }
  const model = liveModel()
  const dirs = new Map(dirsOf(LIVE_DIR).map((dir) => [dir.slug, dir]))
  assert.ok(model.stories.length > 0)

  // Fields the board renders unconditionally must arrive populated; the ones the
  // org documents leave optional must still be the document's own value, never a
  // default the parser invented.
  for (const card of model.stories) {
    const dir = dirs.get(card.slug)
    assert.ok(card.id.length > 0)
    assert.ok(card.title.length > 0)
    assert.equal(typeof card.environment, 'string')
    assert.equal(card.created, keywordOf(dir.story, 'created'))
    assert.equal(card.updated, keywordOf(dir.story, 'updated'))
    assert.equal(card.environment, firstTokenOf(dir.story, 'environment'))
    for (const item of card.tasks) {
      const text = dir.tasks.find((task) => task.slug === item.slug).text
      assert.ok(item.id.length > 0)
      assert.ok(item.title.length > 0)
      assert.equal(typeof item.environment, 'string')
      assert.equal(item.environment, firstTokenOf(text, 'environment'))
      // A task's state comes from its own `* Status` table and nowhere else.
      assert.equal(item.state, statusOf(text))
    }
    // The branch list is the union of the branches its own tasks carry.
    assert.deepEqual(card.branches, [...new Set(card.tasks.map((item) => item.branch))].filter((b) => b !== '').sort())
    assert.equal(card.progress.total, card.tasks.length)
    assert.equal(card.progress.done, card.tasks.filter((item) => item.state === 'DONE').length)
    assert.equal(card.progress.abandoned, card.tasks.filter((item) => item.state === 'ABANDONED').length)
  }
})

test('the live columns come from the state table and count the live cards', (t) => {
  if (!existsSync(LIVE_DIR)) {
    t.skip('the live sprint directory is not present')
    return
  }
  const model = liveModel()
  const canonical = [
    { id: 'BACKLOG', title: 'Backlog' },
    { id: 'STARTED', title: 'Started' },
    { id: 'BLOCKED', title: 'Blocked' },
    { id: 'DONE', title: 'Done' },
    { id: 'ABANDONED', title: 'Abandoned' },
  ]
  assert.deepEqual(model.columns.map(({ id, title }) => ({ id, title })), canonical)
  assert.equal(
    model.columns.reduce((total, column) => total + column.count, 0),
    model.counts.stories,
  )
  for (const column of model.columns) {
    assert.equal(
      column.count,
      model.stories.filter((story) => {
        const state = story.state
        return state === column.id || (column.id === 'BACKLOG' && state === 'DISCOVERED')
      }).length,
    )
  }
  assert.deepEqual(
    model.filters.epics,
    [...new Set(model.stories.map((story) => story.epic))].filter((epic) => epic !== '').sort(),
  )
  assert.deepEqual(
    model.filters.environments,
    [
      ...new Set(
        model.stories
          .flatMap((story) => [story.environment, ...story.tasks.map((item) => item.environment)])
          .filter((environment) => environment !== ''),
      ),
    ].sort(),
  )
})

test('the live work item resolves by branch to a task of the live sprint', (t) => {
  if (!existsSync(LIVE_DIR)) {
    t.skip('the live sprint directory is not present')
    return
  }
  const model = liveModel()
  const tasks = model.stories.flatMap((story) => story.tasks)
  assert.ok(tasks.length > 0)

  // Any branch the live sprint still carries is a fair input: the resolution it
  // produces must be a real task of this sprint, sitting on that branch.
  const branch = tasks.find((item) => item.branch !== '').branch
  const resolved = resolveItem(tasks, branch, null)
  assert.equal(resolved.by, 'branch')
  assert.notEqual(resolved.taskId, '')
  const carrying = tasks.find((item) => item.id === resolved.taskId)
  assert.equal(carrying.branch, branch)

  // A branch no task carries resolves to nothing rather than to a near match.
  assert.deepEqual(resolveItem(tasks, 'feature/no-such-branch-here', null), {
    storyId: '',
    taskId: '',
    by: 'none',
  })
})
