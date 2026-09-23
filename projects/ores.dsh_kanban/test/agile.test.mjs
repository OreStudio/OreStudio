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

// The live org tree is the fixture: these parsers are only worth anything
// against the files they read in production. Other agents close, start and
// abandon tasks in this tree while the suite runs, so a test asserts a literal
// only for a field those edits cannot move.
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

function statusRow(text) {
  const row = /^\|\s*State\s*\|(.*)\|$/m.exec(text)
  return row ? row[1].trim().toUpperCase() : 'UNKNOWN'
}

function readSprintDirs() {
  return readdirSync(SPRINT_DIR, { withFileTypes: true })
    .filter((entry) => entry.isDirectory())
    .map((entry) => {
      const dir = join(SPRINT_DIR, entry.name)
      const tasks = readdirSync(dir, { withFileTypes: true })
        .map((task) => /^task_(.+)\.org$/.exec(task.name))
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

test('a story carries the literal fields of its own org file', () => {
  const story = parseStoryDocument(storyText, 'dsh_agile_plugin')
  assert.equal(story.id, STORY_ID)
  assert.equal(story.title, STORY_TITLE)
  assert.equal(story.type, 'story')
  assert.equal(story.environment, 'bright_faraday')
  assert.equal(story.created, '2026-09-23')
  assert.equal(story.updated, '2026-09-23')
  assert.equal(story.waiting, 'Nothing.')
  assert.equal(story.owner, '')
  assert.equal(
    story.description,
    "A DSH plugin that renders the sprint's stories and tasks as a kanban board, read from the org tree, with the work tree properties visible on every card.",
  )
})

test('the Story: and Task: title prefixes are stripped once', () => {
  const story = parseStoryDocument(storyText, 'dsh_agile_plugin')
  const scaffold = parseTaskDocument(scaffoldText, 'scaffold_dsh_agile_plugin')
  assert.equal(story.title.startsWith('Story:'), false)
  assert.equal(scaffold.title.startsWith('Task:'), false)
  assert.equal(scaffold.title, SCAFFOLD_TITLE)
  assert.equal(scaffold.scaffold, true)
})

test('state comes from the * Status table, and nothing else', () => {
  const story = parseStoryDocument(storyText, 'dsh_agile_plugin')
  const scaffold = parseTaskDocument(scaffoldText, 'scaffold_dsh_agile_plugin')
  assert.equal(story.state, statusRow(storyText))
  assert.equal(scaffold.state, statusRow(scaffoldText))
  const withoutStatus = scaffoldText.replace(/^\|\s*State\s*\|.*$/m, '')
  assert.equal(parseTaskDocument(withoutStatus, 'x').state, 'UNKNOWN')
})

test('an unknown state folds to UNKNOWN and sorts last', () => {
  assert.equal(normalizeState('Started'), 'STARTED')
  assert.equal(normalizeState(' FAILED '), 'UNKNOWN')
  assert.equal(normalizeState(''), 'UNKNOWN')
  assert.equal(normalizeState(undefined), 'UNKNOWN')
  assert.ok(columnOrder('UNKNOWN') > columnOrder('ABANDONED'))
})

test('the state table is the one ordered vocabulary', () => {
  assert.deepEqual(STATE_IDS, [
    'DISCOVERED',
    'BACKLOG',
    'STARTED',
    'BLOCKED',
    'DONE',
    'ABANDONED',
    'UNKNOWN',
  ])
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
  const story = parseStoryDocument(padded, 'x')
  assert.equal(story.environment, 'brave_hopper')
  assert.equal(story.owner, 'marco')
  const multi = '#+title: Story: X\n#+description:  two   spaces   collapse\n'
  assert.equal(parseStoryDocument(multi, 'x').description, 'two spaces collapse')
})

test('the epic is the ** group heading the story row sits under', () => {
  const model = realModel()
  const under = (slug) => model.stories.find((story) => story.slug === slug).epic
  assert.equal(under('dsh_agile_plugin'), 'Hotfixes')
  assert.equal(under('acme_corporation_followups'), 'Product')
  assert.deepEqual(model.filters.epics, ['Hotfixes', 'Product'])
  assert.deepEqual(
    [...new Set(model.stories.map((story) => story.epic))].sort(),
    ['', 'Hotfixes', 'Product'],
  )
  // Two story directories on disk carry no row in the sprint's Stories tables,
  // so they have no enclosing group and their epic is "".
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

test('story progress counts done, abandoned and total', () => {
  const story = realModel().stories.find((s) => s.id === STORY_ID)
  const states = story.tasks.map((task) => task.state)
  assert.equal(story.tasks.length, 2)
  assert.deepEqual([...story.tasks.map((task) => task.id)].sort(), [IMPLEMENT_ID, SCAFFOLD_ID].sort())
  assert.deepEqual(story.progress, {
    done: states.filter((state) => state === 'DONE').length,
    total: 2,
    abandoned: states.filter((state) => state === 'ABANDONED').length,
  })
})

test('the branches and prs unions drop empties and sort', () => {
  const story = realModel().stories.find((s) => s.id === STORY_ID)
  assert.deepEqual(story.branches, [BRANCH])
  assert.deepEqual(story.tasks.map((task) => task.branch), [BRANCH, BRANCH])
  assert.deepEqual(story.prs, [])
  assert.deepEqual(story.tasks.map((task) => task.pr), ['', ''])
})

test('prs parse to integers and sort ascending', () => {
  assert.equal(parsePrNumber('2135'), 2135)
  assert.equal(parsePrNumber('#2133'), 2133)
  assert.equal(parsePrNumber('none'), null)
  assert.equal(parsePrNumber(''), null)
  assert.deepEqual(parsePrNumbers(['#1730', 'none', '', '9', '#1730']), [9, 1730])
})

test('tasks sort by state in table order, then by title', () => {
  const story = realModel().stories.find((s) => s.id === STORY_ID)
  const ranked = [...story.tasks].sort((a, b) => {
    const order = columnOrder(a.state) - columnOrder(b.state)
    return order !== 0 ? order : a.title < b.title ? -1 : a.title > b.title ? 1 : 0
  })
  assert.deepEqual(story.tasks.map((task) => task.slug), ranked.map((task) => task.slug))
})

test('every story in the sprint is a card with a state', () => {
  const model = realModel()
  const dirs = readSprintDirs()
  assert.equal(model.stories.length, dirs.length)
  assert.equal(model.counts.stories, dirs.length)
  assert.deepEqual(
    model.stories.map((story) => story.slug).sort(),
    dirs.map((dir) => dir.slug).sort(),
  )
  for (const story of model.stories) {
    assert.ok(STATE_IDS.includes(story.state))
    assert.equal(story.state, columnOrder(story.state) === STATE_IDS.length - 1 ? 'UNKNOWN' : story.state)
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
    '',
    '* Achievements',
    '',
    '** Not a theme',
    '',
    '| Story | State | Start | End | Description |',
    '|-------+-------+-------+-----+-------------|',
    `| [[id:${SCAFFOLD_ID}][elsewhere]] | DONE | | | desc |`,
  ].join('\n')
  const doc = parseSprintDocument(text)
  assert.equal(doc.epics.get(STORY_ID), 'Tooling')
  assert.equal(doc.epics.has(SCAFFOLD_ID), false)
  const bare = parseSprintDocument(sprintText)
  const uniqueIds = new Set(
    [...sprintText.matchAll(/^\|\s*\[\[id:([0-9A-Fa-f-]+)\]/gm)].map((match) => match[1].toUpperCase()),
  )
  assert.equal(bare.epics.size, uniqueIds.size)
  assert.equal(realModel().stories.find((s) => s.id === STORY_ID).epic, 'Hotfixes')
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

  // The five canonical columns are always sent, in table order, empty included.
  assert.deepEqual(model.columns.map((column) => column.id), [
    'BACKLOG',
    'STARTED',
    'BLOCKED',
    'DONE',
    'ABANDONED',
  ])
  assert.deepEqual(model.columns.map((column) => column.title), [
    'Backlog',
    'Started',
    'Blocked',
    'Done',
    'Abandoned',
  ])
  for (const column of model.columns) {
    assert.equal(
      column.count,
      model.stories.filter((story) =>
        column.id === 'BACKLOG'
          ? story.state === 'BACKLOG' || story.state === 'DISCOVERED'
          : story.state === column.id,
      ).length,
    )
  }
  assert.deepEqual(model.counts, {
    stories: model.stories.length,
    storiesDone: model.stories.filter((story) => story.state === 'DONE').length,
    storiesStarted: model.stories.filter((story) => story.state === 'STARTED').length,
    storiesBlocked: model.stories.filter((story) => story.state === 'BLOCKED').length,
    tasks: model.stories.flatMap((story) => story.tasks).length,
    tasksDone: model.stories.flatMap((story) => story.tasks).filter((task) => task.state === 'DONE').length,
  })
  assert.equal(
    model.columns.reduce((total, column) => total + column.count, 0),
    model.counts.stories,
  )
  assert.deepEqual(model.filters.epics, ['Hotfixes', 'Product'])
  assert.ok(model.filters.environments.includes('bright_faraday'))
  assert.equal(model.filters.environments.includes('brave_hopper brave_hopper'), false)
  assert.deepEqual(model.filters.environments, [...new Set(model.filters.environments)].sort())
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
  assert.equal(realModel().stories.some((story) => story.state === 'UNKNOWN'), false)
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
  const story = model.stories.find((s) => s.id === STORY_ID)
  assert.equal(story.description.length > 40, true)
  assert.equal(story.title, STORY_TITLE)
  assert.equal(story.slug, 'dsh_agile_plugin')
  assert.equal(story.environment, 'bright_faraday')
  assert.equal(story.created, '2026-09-23')
  assert.equal(story.updated, '2026-09-23')
  assert.equal(story.path, 'doc/agile/versions/v0/sprint_25/dsh_agile_plugin/story.org')
  for (const task of story.tasks) {
    assert.notEqual(task.id, '')
    assert.notEqual(task.title, '')
    assert.notEqual(task.owner, '')
    assert.notEqual(task.environment, '')
    assert.notEqual(task.created, '')
    assert.notEqual(task.updated, '')
    assert.equal(task.path, `doc/agile/versions/v0/sprint_25/dsh_agile_plugin/task_${task.slug}.org`)
  }
  const scaffold = story.tasks.find((task) => task.slug === 'scaffold_dsh_agile_plugin')
  assert.equal(scaffold.id, SCAFFOLD_ID)
  assert.equal(scaffold.title, SCAFFOLD_TITLE)
  assert.equal(scaffold.scaffold, true)
  assert.equal(model.sprint.title, 'Sprint 25')
  assert.equal(model.counts.tasks > 400, true)
  assert.equal(model.counts.stories > 90, true)
  assert.equal(model.stories.some((s) => s.tasks.length > 0), true)
})

test('the work item resolves by branch, then by journal task id', () => {
  const entry = { date: '2026-09-23 11:09', taskId: IMPLEMENT_ID }
  const scaffoldEntry = { date: '2026-09-23 10:56', taskId: SCAFFOLD_ID }
  assert.deepEqual(resolveItem(realTasks(), BRANCH, entry), {
    storyId: STORY_ID,
    taskId: IMPLEMENT_ID,
    by: 'branch',
  })
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

test('the tree object describes the session work tree and its own item', () => {
  const model = realModel({ tree: { ...TREE, entry: { taskId: IMPLEMENT_ID } } })
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
  })
  assert.equal('session' in model, false)
  assert.equal('isSession' in model.tree, false)
})

test('tree.by reports sprint-only when the branch matches no task', () => {
  const model = realModel({ tree: { ...TREE, branch: 'feature/nothing-here', entry: null } })
  assert.equal(model.tree.by, 'sprint-only')
  assert.equal(model.tree.currentStoryId, '')
  assert.equal(model.tree.currentTaskId, '')
})

test('trees lists every work tree, sorted, each with its own work item', () => {
  const trees = [
    {
      ...TREE,
      root: '/w/ores_dev_brave_hopper',
      name: 'ores_dev_brave_hopper',
      label: 'brave_hopper',
      branch: 'feature/health-review-2',
      dirty: true,
      entry: null,
    },
    { ...TREE, entry: { taskId: IMPLEMENT_ID } },
    {
      label: 'jolly_knuth',
      name: 'ores_dev_jolly_knuth',
      root: '/w/ores_dev_jolly_knuth',
      branch: '',
      detached: true,
      dirty: false,
      entry: null,
    },
  ]
  const model = realModel({ tree: { ...TREE, entry: { taskId: IMPLEMENT_ID } }, trees })
  assert.deepEqual(model.trees.map((tree) => tree.label), [
    'brave_hopper',
    'bright_faraday',
    'jolly_knuth',
  ])
  assert.equal(model.trees[0].isSession, false)
  assert.equal(model.trees[0].dirty, true)
  assert.equal(model.trees[0].currentStoryId, '')
  assert.equal(model.trees[0].taskTitle, '')
  assert.equal(model.trees[1].isSession, true)
  assert.equal(model.trees[1].currentStoryId, STORY_ID)
  assert.equal(model.trees[1].currentTaskId, IMPLEMENT_ID)
  assert.equal(model.trees[1].storyTitle, STORY_TITLE)
  assert.equal(model.trees[1].taskTitle, IMPLEMENT_TITLE)
  assert.equal(model.trees[1].state, statusRow(implementText))
  assert.equal(model.trees[1].pr, '')
  assert.equal(model.trees[2].detached, true)
  assert.equal(model.trees[2].currentTaskId, '')
  assert.equal(model.trees[2].state, 'UNKNOWN')
  // The fleet is a report: it never decides what the board shows.
  assert.equal(model.tree.label, 'bright_faraday')
  assert.equal(model.stories.length, readSprintDirs().length)
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
  assert.equal(model.trees[0].currentTaskId, ELSEWHERE_TASK_ID)
  assert.equal(model.trees[0].currentStoryId, '')
  assert.equal(model.trees[0].taskTitle, 'Clean up the sprint 25 stories for close')
  assert.equal(model.trees[0].storyTitle, 'Sprint 25 closure: story cleanup and reset for sprint 26')
  assert.equal(model.trees[0].state, 'DONE')
  assert.equal(model.trees[0].pr, '2133')
  // No journal for this tree, so its own branch decides, as for the session.
  assert.equal(model.tree.currentTaskId, IMPLEMENT_ID)
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
        detached: false,
        dirty: false,
        entry: null,
      },
    ],
  })
  assert.deepEqual(model.trees[0], {
    label: 'prime_origin',
    name: 'ores_dev_prime_origin',
    root: '/w/ores_dev_prime_origin',
    branch: 'main',
    detached: false,
    dirty: false,
    isSession: false,
    currentStoryId: '',
    currentTaskId: '',
    storyTitle: '',
    taskTitle: '',
    state: 'UNKNOWN',
    pr: '',
  })
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
