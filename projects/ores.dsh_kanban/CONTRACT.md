# ores-dsh_kanban design contract

Frozen interface between the host half and the client half. Both halves are
written against this file. Change it only by agreement, and update both halves in
the same commit.

## What this is

A DSH web-client plugin, private to ORE Studio. It renders the current sprint as
a kanban board inside a live session, and it shows the properties the org files
already carry: the environment working each story and task, the state, the
branch, the PR, the epic, and the task progress.

The board reads the org tree on disk. It is never a mirror, so it cannot go
stale, and it has nothing to synchronise.

Read-only. The plugin never writes an org file. `compass add` and `compass task
start` remain the only way agile artefacts change.

## Many work trees in one instance

One DSH web instance hosts sessions from every work tree, and each work tree is
its own checkout of `doc/agile/` on its own branch. The same story therefore has
a different state, a different task set, and often no presence at all from one
work tree to the next.

The board always shows the work tree of the session it is rendered in, and only
that work tree. No control points it somewhere else. To see another work tree's
board, switch to a session in that work tree. Keeping the board and the session
in agreement is the point, because a board showing one work tree's data inside
another work tree's session is the confusing case.

Every board is read from its own work tree's files. Nothing is merged across work
trees, because a merged state would be a fiction.

A read-only fleet strip lists the other work trees and what each is currently on.
It reports; it never changes what the board shows.

## Where the data comes from

The org tree is the single source of truth. Do not use the site's
`graphdata.json`: an Emacs job rebuilds it whole-repo, it goes stale between site
builds, and it currently cannot see the newest stories.

Layout walked, relative to the work tree root:

```
doc/agile/versions/<version>/<sprint_NN>/sprint.org
doc/agile/versions/<version>/<sprint_NN>/<story_dir>/story.org
doc/agile/versions/<version>/<sprint_NN>/<story_dir>/task_<slug>.org
<worktree>/.journal.org                 (one file per work tree)
```

The board covers one work tree and one sprint within it: the sprint the session's
work tree is on when that resolves, otherwise the newest `sprint_NN` directory.
Every `story.org` under that sprint is a card, and every `task_*.org` beside it
belongs to that card.

The set of work trees comes from `git worktree list --porcelain` in the session's
tree. A work tree outside this repository is never read.

## Org fields read

Only these, from `#+keyword:` lines, the first `:PROPERTIES:` drawer, and the
`* Status` field table.

| Field | Source |
|---|---|
| id | `:ID:` in the first `:PROPERTIES:` drawer, upper-cased |
| title | `#+title:`, with a leading `Story:` or `Task:` stripped |
| type | `#+type:` |
| description | `#+description:` |
| environment | `#+environment:` |
| branch | `#+branch:` on tasks only |
| pr | `#+pr:` on tasks only |
| blocked_on, blocked_since | `#+blocked_on:` and `#+blocked_since:` |
| created, updated | `#+created:` and `#+updated:` |
| start_date, end_date | `sprint.org` only |
| state | the `State` row of the `* Status` field table, upper-cased |
| waiting | the `Waiting on` row of the same table |

Missing values become `""`, never `null`, except where this file says otherwise.
Never read state from `#+todo:` or from heading cookies.

### The state vocabulary

`DISCOVERED`, `BACKLOG`, `STARTED`, `BLOCKED`, `DONE`, `ABANDONED`. Anything else,
including a missing `* Status` table, becomes `UNKNOWN`. Put this in one ordered
table in the host, use it to build the columns, the counts, and the ordering, and
send the result to the client. The client must not repeat the list.

`DISCOVERED` folds into the `BACKLOG` column. The five canonical columns are
always sent, in table order, even when one of them has no cards, so the board
does not rearrange itself as cards move. `UNKNOWN` breaks that rule in one
direction only: it is sent last, and only when at least one card is in it.

## Host route

```
GET /plugins/ores-dsh-kanban/state?session=<sessionId>&cwd=<optional absolute path>
```

The board belongs to the session's work tree. Resolve it in this order, and
report which rung was taken in `tree.source`.

1. The session's own directory, `ctx.sessions.get(sessionId)?.header?.cwd`.
   Because that field means "the directory the session was created in", accept it
   only when it resolves to a known work tree root. Report `source: "session"`.
2. The `cwd` query parameter, accepted only when it resolves to a known work tree
   root of `git worktree list --porcelain`. Report `source: "cwd"`. The client
   sends the session's workspace root here, read from the browser session store,
   so a host session store that has not loaded the session does not blank the
   board. A path that is not a work tree root of this repository never resolves,
   which is what makes a caller-supplied path safe.
3. Otherwise fail. Report `ok: false` with `reason: "unknown-session"` when a
   `session` was supplied and neither rung resolved, and `not-an-agile-tree` when
   none was. Never fall back to `process.cwd()`: the shipped unit starts the
   server with its working directory set to the user's home, so that fallback
   would silently answer for the wrong tree or fail for every session.

`tree.source` exists so a board read from a fallback can never pass for a board
read from the session.

Register with `ctx.webServer.register({ kind: 'exact', path: '/plugins/ores-dsh-kanban/state', handler })`
inside `ctx.effect(...)`. The host plugin declares `export const inject = ['webServer', 'sessions']`.
Both services are declared, because reading the session's work tree needs
`ctx.sessions` and Cordis refuses an undeclared service.

Resolve the work tree root from the session:

```js
const cwd = ctx.sessions.get(sessionId)?.header?.cwd
```

The ladder above governs what happens when that returns nothing usable. `cwd` is
not a debugging affordance; it is the rung that keeps the board working when the
host session store has not loaded the session yet.

Response is always HTTP 200 with `content-type: application/json; charset=utf-8`
and `cache-control: no-store`. The discriminant is `ok`.

```json
{
  "ok": true,
  "generatedAt": "2026-09-23T11:40:00.000Z",
  "tree": {
    "root": "/abs/path", "name": "ores_dev_bright_faraday",
    "label": "bright_faraday", "branch": "feature/dsh-agile-plugin",
    "detached": false, "dirty": false,
    "currentStoryId": "3085B911-…", "currentTaskId": "8A81CB08-…",
    "by": "branch", "source": "session"
  },
  "sprint": {
    "version": "v0", "name": "sprint_25", "title": "Sprint 25",
    "startDate": "2026-09-16", "endDate": "2026-09-29",
    "dayOfSprint": 8, "totalDays": 14,
    "path": "doc/agile/versions/v0/sprint_25/sprint.org"
  },
  "columns": [
    { "id": "BACKLOG", "title": "Backlog", "count": 43 },
    { "id": "STARTED", "title": "Started", "count": 30 },
    { "id": "BLOCKED", "title": "Blocked", "count": 7 },
    { "id": "DONE", "title": "Done", "count": 41 },
    { "id": "ABANDONED", "title": "Abandoned", "count": 1 }
  ],
  "stories": [
    {
      "id": "3085B911-…", "slug": "dsh_agile_plugin",
      "title": "Show the current agile work item inside DSH",
      "state": "STARTED", "epic": "tooling",
      "description": "…",
      "environment": "bright_faraday",
      "created": "2026-09-23", "updated": "2026-09-23",
      "path": "doc/agile/versions/v0/sprint_25/dsh_agile_plugin/story.org",
      "progress": { "done": 1, "total": 2, "abandoned": 0 },
      "branches": ["feature/dsh-agile-plugin"],
      "prs": [2135],
      "tasks": [
        {
          "id": "8A81CB08-…", "slug": "scaffold_dsh_agile_plugin",
          "title": "Scaffold story: Show the current agile work item inside DSH",
          "state": "DONE", "branch": "feature/dsh-agile-plugin",
          "pr": "2135", "blockedOn": "", "blockedSince": "",
          "environment": "bright_faraday",
          "created": "2026-09-23", "updated": "2026-09-23",
          "scaffold": true,
          "path": "doc/agile/versions/v0/sprint_25/dsh_agile_plugin/task_scaffold_dsh_agile_plugin.org"
        }
      ]
    }
  ],
  "trees": [
    {
      "label": "eager_maxwell", "name": "ores_dev_eager_maxwell",
      "root": "/abs/path", "branch": "feature/retire-web-ui-codegen",
      "isSession": false,
      "currentStoryId": "…", "currentTaskId": "…",
      "storyTitle": "…", "taskTitle": "…", "state": "STARTED", "pr": "2133"
    }
  ],
  "counts": {
    "stories": 103, "storiesDone": 41, "storiesStarted": 30,
    "storiesBlocked": 7, "tasks": 447, "tasksDone": 260
  },
  "filters": {
    "environments": ["brave_hopper", "bright_faraday", "…"],
    "epics": ["tooling", "eventing", "…"]
  }
}
```

Rules for the payload:

- `story.slug` and `task.slug` are the directory and file slugs, without the
  `task_` prefix or the `.org` suffix. They are the stable handles for links.
- `story.epic` is the story's theme. The theme is the `**` group heading under
  the sprint's `* Stories` section whose table carries the story's `[[id:…]]`
  row, so `Product` and `Hotfixes` in sprint 25 today. It is `""` when no group
  heading contains the story. Read it from that heading, not from a heading's
  text anywhere else and not from `#+filetags:`.
- Every keyword value is trimmed and collapsed, and a keyword the repository
  documents as a single token is cut at the first whitespace. `#+environment:`
  is a single-token keyword. That rule exists because
  `doc/agile/versions/v0/sprint_25/close-systemic-codegen-gaps/story.org` carries
  `#+environment: brave_hopper brave_hopper` upstream, and the doubled value must
  not become a filter chip.
- `sprint.dayOfSprint` is `today - startDate + 1` with no upper clamp, so an
  overrun sprint reports its true day. Sprint 25 runs 2026-08-03 to 2026-08-10,
  so it reports a day greater than `totalDays`. `totalDays` is
  `endDate - startDate + 1`. Both are `null` when a date is missing.
- `story.environment` comes from the story's own keyword. `#+owner:` is not read
  at all. Ownership sits on tasks and reads the same across the sprint, so it
  earns no space on a card face or in the detail panel.
- `story.branches` is the de-duplicated, sorted union of its tasks' `branch`
  values, empty strings dropped. `story.prs` is the same union of its tasks'
  `pr` values, parsed to integers, sorted ascending.
- `story.tasks` is sorted by state in table order, then by title.
- `task.scaffold` is true when the task's title starts with `Scaffold story:`.
- `progress.done` counts tasks whose state is `DONE`, `progress.abandoned`
  counts `ABANDONED`, and `progress.total` counts all tasks in the story's
  directory.
- `trees` is the read-only fleet overview: every work tree the session's
  repository knows about, from `git worktree list --porcelain`. Sort by label.
  Cap at 40 rows. `label` is the work tree's directory name with the `ores_dev_`
  prefix removed, and it is the same token that appears in `#+environment:`.
  `isSession` marks the session's own work tree. `currentStoryId`,
  `currentTaskId`, `storyTitle`, `taskTitle`, `state`, and `pr` describe that
  work tree's own work item, and are `""` when it has none. This data is
  informational. Nothing in it changes what the board shows.
- Rows of `trees` carry no `dirty` and no `detached`. Nothing renders them, and
  computing them costs one `git status` per work tree on every request. Both stay
  on `tree`, which does render them.
- `tree` is the session's work tree, and it is the one the card data was read
  from. `tree.currentStoryId` and `tree.currentTaskId` are its own work item.
  `tree.by` is `"branch"` when its branch matched a task, `"sprint-only"` when a
  sprint resolved without one, and `"none"` otherwise. `tree.source` is
  `"session"` or `"cwd"`, naming the rung of the ladder that resolved the tree.
  `tree.detached` is derived from the branch reading back as `HEAD`, so a git
  call that could not run yields `branch: ""` and `detached: false`, which the
  empty branch distinguishes from a real detached checkout.
- `filters.environments` is the sorted union of every distinct non-empty
  environment across the session's work tree's stories and tasks.
  `filters.epics` is the same for epics.

### Resolving the session's own work item

1. Read the branch with `git rev-parse --abbrev-ref HEAD` in the tree root.
2. Collect every `task_*.org` in the sprint whose `#+branch:` equals that branch.
3. Narrow the candidates, because several tasks may share one feature branch.
   Read the last entry of the tree's `.journal.org` and take the UUID from its
   `Task :: [[id:<UUID>][…]]` line. When that UUID is a candidate, it wins.
   Otherwise take the candidate with the latest `#+updated:`, then the first by
   path.

`.journal.org` format, newest last:

```
* 2026-09-23 10:56 — [[id:3085B911-…][Show the current agile work item inside DSH]]
  - Task :: [[id:8A81CB08-…][Scaffold story: …]]
  - State :: STARTED
  - Branch :: feature/dsh-agile-plugin
  - PR :: none
```

Failure shape, still HTTP 200:

```json
{ "ok": false, "reason": "not-an-agile-tree", "message": "doc/agile/versions not found under /abs/path" }
```

`reason` is one of `not-an-agile-tree`, `no-sprint`, `git-unavailable`,
`unknown-session`.

## Follow-up stage, not in this wave

Comparing one story across work trees is the natural next step and is specified
here so the payload does not have to change shape twice.

```
GET /plugins/ores-dsh-kanban/story?session=<sessionId>&id=<story UUID>
```

It returns one row per work tree that carries the story, with its state, task
counts, environment, and branch there, and `present: false` for a work tree whose
checkout does not have the story at all. The host looks the story up in its own
loaded sprint by id, so no caller-supplied path is ever read. The card detail
panel renders the rows as an "across work trees" table.

Do not build this yet.

## Client half

One file, `lib/client.js`, no build step. It is a lazy-CJS closure factory:

```js
window.__ModuleLoader__.load({ id: 'ores-dsh-kanban', factory: (require) => { … return module.exports } })
```

`require` accepts only platform modules. Use `react` alone, and call
`React.createElement` through a local `const h = React.createElement`. Do not
require anything else: `react-dom`, `react/jsx-runtime`, `@deepseek-ai/cordis`,
`@deepseek-ai/dsh-client-ui-primitives`, `@deepseek-ai/dsh-client-store`,
`@deepseek-ai/dsh-client-ui-slots`, `@deepseek-ai/dsh-client-ui-dockkit`,
`@deepseek-ai/dsh-api-gateway/client` and `url` are also available, but nothing
here needs them.

The exported plugin object is `{ name: 'ores-dsh-kanban', inject: ['slots'], apply(ctx) }`.
Register every seat inside its own `ctx.effect(() => ctx.slots.inject(…), 'label')`.

Every request to the state route carries the session's directory as `cwd`. Read it
from the browser session store, which is the SESSION's workspace root and not the
host process's directory:

```js
const cwd = ctx.get('sessions')?.list?.getSnapshot()?.byId?.[sessionId]?.cwd
```

That is not optional. It is the rung that keeps the board working when the host
session store has not loaded the session, and the host accepts it only when it is
a work tree root of this repository. When it is absent, send no `cwd` at all
rather than a guess.

### Seat 1. The always-visible readout

`conversation.session.header.actions`, `id: 'ores-dsh-kanban-now'`, `order: 20`.

One compact button naming this session's work item: `tree.label` in a monospace
chip, a state dot in the state colour, the story title truncated to about 40
characters, then the task title truncated to about 30. `title` carries the full
text.

Clicking toggles a popover under the chip listing the current story's tasks:
state dot, title, the environment, and a `current` marker on the one this work
tree is on. Close on `Escape` and on a click outside. `aria-expanded` on the
button, `role="dialog"` on the popover.

When `ok` is false or `tree.currentStoryId` is empty, the chip reads `No work
item` in the muted colour and the popover says why.

### Seat 2. The board

`conversation.view`, `id: 'kanban'`, `order: 25`, `label: () => 'Kanban'`, after
the pattern `@alpacachen/dsh-kanban` uses at `src/client/plugin.ts`.

A full-width, read-only board, in this order:

1. **Sprint line.** `tree.label` and its branch, the sprint title, `Day X of Y`,
   the card count, and a `Refresh` button.
2. **Fleet strip.** A read-only row, one chip per row of `trees`: the label in
   monospace, the state dot, and that work tree's current task title truncated to
   about 28 characters, or `idle` when it has none. The chip for the session's
   own work tree carries a `this tree` marker. Chips are not buttons and do not
   change the board. The strip answers "which work tree is on which story and
   task" at a glance; switching work trees happens by switching session.
3. **Controls.** A search input filtering on story title, story id, epic, task
   title, environment, and branch, and a row of epic chips built from
   `filters.epics`. Active filters stay visible and individually clearable, with
   one `Clear` control when any is on.
4. **Tiles.** Four tiles, each an uppercase letter-spaced label with a large
   value: `stories`, `in flight`, `blocked`, `tasks done` as `done/total`.
5. **Board.** One column per entry of `columns`, in the order sent. Each column
   header carries the column title and its count, coloured by state. Cards are
   the stories whose state maps to that column and that pass the filters.
6. **Card face.** The story title, an environment chip in monospace, the epic,
   a task progress bar with `done/total`, and `open Nd` age from
   `created`. A 3px left border in the story's epic colour. A task count line
   such as `2 tasks · 1 done`. The card for `tree.currentStoryId` carries an
   accent outline and a `current` marker, and is scrolled into view when the view
   first mounts.

   Branches and pull requests do not appear on the card face. A story can carry
   several of each, so they crowd the face without answering what the board is
   for. Both live in the card detail panel, which is where someone who wants them
   is already looking.
7. **Card detail.** Clicking a card opens a panel on the right of the view: the
   story's id, state badge, environment, epic, created and updated dates,
   description, branches, and PR list, then every task as a row with a state
   badge, environment, branch, and PR. Clicking a task row expands
   `blocked on`, `blocked since`, `created`, `updated`, and its file path.
   `Escape` and a close button dismiss the panel.

### Loading and failure

Fetch on mount and on `Refresh`, keyed by `sessionId`. Guard every response:
treat a missing `ok` as failure, treat non-array `stories`, `columns`, `trees`,
`tasks`, `branches`, and `prs` as empty, and never index a missing object. Default
every field the contract names to `""`, `0`, `[]`, or `false` before rendering.

Keep the last good snapshot when a refresh fails, and mark it `stale` next to the
refresh control. Show a skeleton on first load, never a blank view. Render an
empty-board message naming the sprint and the work tree when `stories` is empty.

### Look

Use the DSH theme alias tokens so the panel follows the active theme:
`var(--dsw-alias-state-error-primary)`, `var(--dsw-alias-state-warn-primary)`,
`var(--dsw-alias-state-business-primary)`, `var(--dsw-alias-label-tertiary)`,
plus the shell's own text, background, and border tokens where one fits. Do not
hard-code the panel's own background or text colour.

Define the agile state palette once, as a lookup table keyed by the same state
ids the host sends:

| state | colour |
|---|---|
| BACKLOG, DISCOVERED | `#6e7681` |
| STARTED | `#58a6ff` |
| BLOCKED | `#d29922` |
| DONE | `#3fb950` |
| ABANDONED | `#f85149` |
| UNKNOWN | `#6e7681` |

Density is compact. Labels are `0.72rem` uppercase with `0.06em` letter spacing,
badges are `0.68rem` bold, monospace chips use the shell's monospace stack, and
the view uses `rem` units with `box-sizing: border-box`. The column grid scrolls
horizontally; each column is a fixed-width flex column with its own vertical
scroll.

Inject one `<style data-plugin="ores-dsh-kanban">` element on first mount, guard
against a second, and remove it when the effect disposes.

## Files

| File | Owner |
|---|---|
| `package.json`, `cordis.patch.yml`, `CONTRACT.md` | lead |
| `lib/index.js` | host |
| `lib/agile.js` | host |
| `lib/git.js` | host |
| `test/agile.test.mjs` | host |
| `lib/client.js` | client |

## Verification

Offline. The host half is checked by `node --test test/agile.test.mjs` from the
`projects/ores.dsh_kanban` directory, and by `curl` against a scratch DSH
instance. The bare `node --test` form also runs it; `node --test test/` fails on
the shipped Node 22.22.1 with `Cannot find module '…/test'`. The client half is
checked in a real browser against the same instance, with screenshots written
under `tmp/shots/`.
