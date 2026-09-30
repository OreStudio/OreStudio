# ores-dsh_environment design contract

Frozen interface between the host half and the browser half. Both halves are
written against this file. Change it only by agreement, and update both halves
in the same commit.

## What this is

A DSH web-client plugin, private to ORE Studio. It shows the environment of
the work tree its session runs in: the environment name, when the database was
last restored and how long ago, the schema drift against HEAD, and the state of
every service unit. It starts and stops services and rebuilds the database.

## Where the data comes from

The plugin runs the checkout's own compass and reads its output:

```
<worktree>/compass.sh env status --json
```

compass is the single source of truth, and this is deliberate. The readiness
rule for a service (active, and its log carries `Service ready`), the drift
comparison for the schema, the thresholds that turn a restore age or a
connection count into a level, and the rule that an unknown unit is `missing`
all live in `compass_services` and `compass_db`. A plugin that read systemd,
`psql` and the log files itself would be a second copy of those rules, and the
two copies would drift apart with nothing to detect it. The plugin's own
`CONTRACT.md` therefore carries no thresholds.

Two consequences follow, and both are accepted:

- A work tree whose compass predates `compass env status` reports
  `compass-too-old`. It does not fall back to an older parse, because a
  fallback would be the second copy this design exists to avoid.
- The plugin inherits compass's vocabulary rather than restating it. The
  service states and their order arrive in the payload, per request. The
  browser colours the ids it is sent and keeps a grey fallback for one it has
  never heard of.

## The work tree a session resolves to

One DSH web instance hosts sessions from every work tree, and each work tree is
its own checkout with its own `.env`, its own database and its own service
units. The plugin always shows the work tree of the session it is rendered in,
and only that work tree. To see another work tree's environment, switch to a
session in that work tree.

Resolve it in this order, and report which rung was taken in `tree.source`:

1. The session's own directory, `ctx.sessions.get(sessionId)?.header?.cwd`.
   Report `source: "session"`.
2. The `cwd` parameter of the request. The client sends the session's
   workspace root here, read from the browser session store, so a host session
   store that has not loaded the session does not blank the panel. Report
   `source: "cwd"`.
3. Otherwise fail.

A candidate resolves only when git finds a top level for it *and* that top
level holds both `compass.sh` and `projects/ores.compass/compass.sh`, which is
what establishes that it is an ORE Studio checkout. A caller-supplied path that
is not one never resolves, which is what makes it safe to accept. The process
working directory is never a rung: the shipped unit starts the server with the
user's home as its working directory, so that fallback would answer for a
directory that is not a checkout, or fail for every session.

The plugin never reads an `ORES_*` value from its own process. `dsh-web.service`
starts from the home directory and holds no checkout's environment on purpose,
because a checkout's database name once leaked into another checkout through
the harness. The server's environment is therefore not evidence about any work
tree.

## The payload compass prints

This is compass's shape, reproduced here because the plugin depends on it. The
authority is `projects/ores.compass/src/compass_env_status.py`.

```json
{
  "ok": true,
  "generatedAt": "2026-09-30T12:45:00Z",
  "env": {
    "name": "brave_hopper", "label": "brave_hopper",
    "preset": "linux-clang-debug-make", "worktree": "/abs/path",
    "envVersion": 26, "requiredEnvVersion": 26, "envStale": false,
    "scope": "dsh-subprocess-1108-….scope", "slice": "app.slice",
    "activities": [{"number": 1, "date": "…", "title": "…", "recipeId": "…"}],
    "vcpkgWarning": ""
  },
  "database": {
    "reachable": true, "name": "ores_dev_brave_hopper",
    "restoredAt": "2026-09-28 15:34", "restoredAgeSeconds": 162000,
    "restoredAge": "1d", "restoredLevel": "critical",
    "schemaVersion": "0.0.25", "builtFrom": "5559b012aed",
    "builtAt": "2026/09/28 14:19:46", "driftSeconds": 90000,
    "driftLabel": "1d behind HEAD — drifting", "driftLevel": "warn",
    "bootstrapMode": true, "warning": ""
  },
  "services": {
    "total": 24,
    "counts": {"running": 0, "starting": 0, "stopped": 24, "failed": 0, "missing": 0},
    "nats": {"unit": "…", "label": "nats-server", "state": "stopped", "detail": "inactive"},
    "units": [{"unit": "…", "service": "…", "replica": 0, "label": "web",
               "log": "…", "state": "stopped", "detail": "inactive"}],
    "logDir": "/abs/path"
  },
  "health": {"level": "warn", "reasons": ["24 of 24 services are stopped"]},
  "remedies": {"startServices": "…", "stopServices": "…",
               "recreateDatabase": "…", "configureEnv": "…"}
}
```

`services.nats` is reported apart from `services.counts` and from
`services.total`. nats-server has no readiness log under systemd, so the
readiness rule does not apply to it; counting it with the services would say
something the rule does not support.

`health.level` is compass's, not the plugin's, and `health.reasons` names every
reason that produced it. The plugin renders the level as a colour and the
reasons as text, and never recomputes either.

## Host routes

Both routes answer HTTP 200 with `content-type: application/json; charset=utf-8`
and `cache-control: no-store`. The discriminant is `ok`.

### Read

```
GET /plugins/ores-dsh-environment/state?session=<sessionId>&cwd=<optional absolute path>
```

The response is the transformed model. The transform is `lib/env.js`, and it
owns exactly two rules:

- the tone of each tile, which the browser turns into a colour: `ok`, `warn`,
  `critical` or `unknown`;
- the service state vocabulary, which it reads out of `services.counts` rather
  than restating, so a state compass adds needs no second edit.

```json
{
  "ok": true,
  "generatedAt": "…",
  "env": {"name", "label", "preset", "worktree", "worktreeName", "scope",
          "slice", "envVersion", "requiredEnvVersion", "envStale",
          "activities", "vcpkgWarning"},
  "database": {"reachable", "name", "restoredAt", "restoredAge",
               "restoredAgeSeconds", "restoredLevel", "schemaVersion",
               "builtFrom", "builtAt", "driftLabel", "driftLevel",
               "driftSeconds", "bootstrapMode", "warning", "confirmPhrase"},
  "services": {"total", "counts", "states": [{"id", "title", "count"}],
               "units": [{"unit", "selector", "service", "replica", "label",
                          "state", "detail"}],
               "nats": {"label", "state", "detail"}, "logDir"},
  "health": {"level", "reasons"},
  "tiles": [{"id": "database"|"services"|"config", "label", "value", "tone",
             "detail"}],
  "remedies": {"…"},
  "tree": {"root": "/abs/path", "source": "session"|"cwd"},
  "job": null
}
```

Missing values become `""`, `0`, `[]` or `false`, never `null`, except
`restoredAgeSeconds`, `driftSeconds` and `requiredEnvVersion`, which are `null`
when compass could not read them. The browser defaults every field before
rendering and never indexes a missing object, so a partial payload renders as
an incomplete panel rather than a blank one.

`unit.selector` is the value a Start or Stop posts, and it is the registry
service name, not the label. compass resolves a registry service and never one
replica of it, so every row of a replicated service carries the same selector
while its label stays per replica. It is `""` for nats-server, which is not in
the registry: a row with no selector offers no action.

A read is cached in the host for two seconds, keyed by the resolved work tree
root. A hit older than the cache's own TTL is a miss, and the entry ages from
when the read finished rather than from when the request arrived, so a slow
compass does not shorten its own cache life.

While an action runs the cache is served regardless of its age, and the entry is
dropped when the action settles. The fleet is mid-change during an action, so a
fresh read would report a half-applied state, and it would cost one compass
process every poll for the minutes a rebuild takes.

Failure shape, still HTTP 200:

```json
{"ok": false, "reason": "compass-too-old", "message": "…", "job": null}
```

`reason` is one of `not-an-environment`, `unknown-session`, `compass-too-old`,
`compass-failed`, `compass-unreadable`, `unauthenticated`.

### Both routes need the browser session

The harness fences `/api` with a Host rule and an authority-bound cookie, and
registers a plugin route outside that fence. This plugin therefore applies the
same cookie check itself, on the read as well as the act:

```js
ctx.get('connection')?.browserAuth?.isAuthenticated(req) === true
```

A request without it is refused with `reason: "unauthenticated"` and HTTP 401,
before any other check. The browser already holds the cookie `dsh web` issued
for this authority, and a same-origin fetch sends it, so the client asks for
nothing extra.

This is not decoration. A page that rebinds its own hostname to loopback
reaches this route as a same-origin caller, so the Origin check cannot see it,
and without the cookie a rebound page could stop the fleet. The harness's own
documentation makes the same argument for `/api`. When `connection` is absent,
the plugin refuses rather than allowing: an unauthenticated action route is the
worse failure.

### Act

```
POST /plugins/ores-dsh-environment/action
{"session": "…", "cwd": "…", "action": "…", "service": "…", "confirm": "…",
 "stopServices": true}
```

The action table, and the compass sequence each one runs:

| action | sequence |
|---|---|
| `service-start` | `services start <service>` |
| `service-stop` | `services stop <service>` |
| `fleet-start` | `services start` |
| `fleet-stop` | `services stop` |
| `database-restore` | with `stopServices`: `services stop`, `db recreate -y -k`, `services start`. Without: `db recreate -y -k`. |

`stopServices` defaults to true when absent. A service selector is constrained
to what compass accepts: `^[A-Za-z0-9][A-Za-z0-9._-]*$`.

The reply is `{"ok": true, "job": {…}}`, or a failure with `reason` one of
`unknown-action`, `bad-service`, `not-confirmed`, `busy`, `bad-request`. `busy`
answers 409; the rest answer 400, 403, 405 or 415 as appropriate.

`not-confirmed` is the guard on the one destructive action. A caller may only
rebuild the database whose name the browser was last shown as
`database.confirmPhrase`. The host holds that name from the last successful
read, so a request that names another checkout, or arrives before any read,
is refused. The browser additionally makes the operator type it.

### The job

```
{"id", "kind", "label", "running", "code", "startedAt", "finishedAt",
 "elapsedSeconds",
 "steps": [{"name", "running", "code", "elapsedSeconds"}],
 "tail": ["…"]}
```

One job runs per work tree at a time. It lives in the server process, and the
state route reports it, so the browser shows progress by polling the read route
rather than by holding a request open for the minutes a restore takes.

`tail` is the last 400 lines compass printed, across every step, prefixed by
the command line the job is running. It is compass's own output, not a summary:
compass prints its phases, and a progress meter computed from nothing would be
a guess.

## Client half

One file, `lib/client.js`, no build step. It is a lazy-CJS closure factory:

```js
window.__ModuleLoader__.load({ id: 'ores-dsh-environment', factory: (require) => { … return module.exports } })
```

`require` accepts only platform modules. Use `react` alone, and call
`React.createElement` through a local `const h = React.createElement`.

The exported plugin object is `{ name: 'ores-dsh-environment', inject: ['slots'],
apply(ctx) }`. Register every seat inside its own
`ctx.effect(() => ctx.slots.inject(…), 'label')`.

Every request carries the session's directory as `cwd`, read from the browser
session store, which is the session's workspace root and not the host process's
directory:

```js
const hook = ctx.get('sessions')            // or the seat's `useSessions` prop
const cwd = hook((store) => store?.byId?.[sessionId]?.cwd)
```

`ctx.get('sessions')` is the store hook the shipped `ores.dsh_kanban` plugin uses,
and this half reads it the same way, preferring a `useSessions` prop when the
seat is given one.

When it is absent, send no `cwd` at all rather than a guess.

### Seat 1. The readout that arrives with the session

`conversation.session.header.actions`, `id: 'ores-dsh-environment-now'`,
`order: 21`.

One compact chip: a health dot in the level's colour, the environment label in
a monospace chip, `db <age>`, and `<running>/<total>`. The service count turns
red when any unit is failed or missing. `title` carries the full text, and the
level's reasons, one per line.

Clicking toggles a popover under the chip: the environment name, preset, work
tree name, scope, the database restore time and age, then every reason the
level carries, or `No problems reported.`. Close on `Escape` and on a click
outside. `aria-expanded` on the button, `role="dialog"` on the popover.

When the read fails, the chip reads `no environment` in the muted colour and
the popover says why.

### Seat 2. The view

`conversation.view`, `id: 'environment'`, `order: 30`, `label: () => 'Environment'`.

A full-width view, in this order:

1. **Identity line.** The environment label, the preset and the work tree name,
   the generated time, a `stale` marker when a refresh failed after a good
   read, and a `Refresh` button.
2. **Tiles.** One tile per entry of `tiles`, in the order sent: an uppercase
   label, a large value, and a detail line, with a 3px left border in the tone's
   colour.
3. **Reasons.** Every entry of `health.reasons`, in the level's colour.
4. **Job strip.** Present only while a job exists: what is running, its elapsed
   time or exit code, one marker per step, and the tail in a scrolling block
   that follows the end. No progress bar.
5. **Database panel.** Restored time and age, schema version, the commit and
   date it was built from, the drift label, a bootstrap-mode warning when it is
   on, and a `Restore database…` button.
6. **Services panel.** The counts line, one entry per state in the order sent,
   coloured by state; `nats-server` on its own row marked `starts with the
   fleet`; `Start all` and `Stop all`; three filters (`All`, `Not running`,
   `Failed or missing`) as toggle chips; then one row per unit: a state dot, the
   state badge, the label, the unit name in monospace, the detail, and a
   `Start` or `Stop` button. The log directory closes the panel.

### The state palette

Define it once, as a lookup table keyed by the state ids the host sends.

| state | colour |
|---|---|
| running | `#3fb950` |
| starting | `#d29922` |
| stopped | `#6e7681` |
| failed | `#f85149` |
| missing | `#f85149` |

An unknown state falls back to grey. `stopped` is grey because it is the state
an operator chooses. `failed` and `missing` are both red but for different
reasons: one is a unit systemd tried to run and could not, the other a unit the
manager has never heard of, which usually means the fleet was never deployed in
this work tree.

Tile tones map the same way: `ok` `#3fb950`, `warn` `#d29922`, `critical`
`#f85149`, `unknown` `#6e7681`.

### What asks before it acts

Starting and stopping cost one click each, for one unit and for the fleet, with
no confirmation. Both are cheap and fully reversible, and compass's own verbs
do not ask either. A dialog on a reversible action teaches the operator to
click through the one dialog that matters.

The database restore is the single guarded action, because it drops the
database and its roles. Its dialog names the database, states that every row in
it is lost, offers `Stop the services first, then start them again` checked by
default, and will not proceed until the environment name is typed. `Escape` and
`Cancel` dismiss it.

### Loading and failure

Fetch on mount, on `Refresh`, and every two seconds while `job.running` is
true. Keep the last good snapshot when a refresh fails, and mark it `stale`
next to the refresh control. Show a loading line on first load, never a blank
view.

### Look

Use the DSH theme alias tokens so the panel follows the active theme, and do not
hard-code the panel's own background or text colour. Density is compact: labels
are `0.68rem` uppercase with `0.06em` letter spacing, monospace chips use the
shell's monospace stack, and the view uses `rem` units with
`box-sizing: border-box`.

Inject one `<style data-plugin="ores-dsh-environment">` element on first mount,
guard against a second, reference count it across the two seats, and remove it
when the last seat unmounts.

## Files

| File | Owner |
|---|---|
| `package.json`, `cordis.patch.yml`, `CONTRACT.md` | lead |
| `lib/index.js` | host |
| `lib/compass.js` | host |
| `lib/env.js` | host |
| `test/env.test.mjs` | host |
| `lib/client.js` | client |

## Verification

Four gates. None needs a browser dependency, and none is destructive.

`node --test` from the `projects/ores.dsh_environment` directory covers the
transform and its tile rules, the action table and every validation refusal,
the work-tree resolver, and the client bundle: the bundle is loaded the way the
module loader loads it, the factory is run, `apply` is called against a stub
slot registry, and each seat is rendered once with a stub React. That last test
exists because the bundle is served verbatim, so a mistake in it is invisible
until the panel fails to appear.

`node scripts/check-manifest.mjs` checks the wiring DSH resolves by name: the
manifest fields, the export paths, the two routes, and the two seat names.

`node scripts/verify.mjs` is the host gate against the real compass. It applies
the plugin to a stub host, then drives the captured handlers: the cookie check
and its refusals, the resolution ladder and the rung each request took, the
state contract against a live payload, every failure reason, every action
refusal, and one real action job with its tail and exit code. The action it runs
is `services start` on a name compass does not know, which reaches the registry
lookup and exits 1 without touching systemd. Nothing in it starts a service,
stops a service, or rebuilds a database. It is a local gate, because it reads a
real environment.

A scratch DSH instance is the fourth check, and the only one that exercises the
loader: install the packed tarball into a throwaway `$DSH_HOME`, boot it on its
own port, and confirm the boot log names both routes. Then confirm that an
anonymous read and an anonymous `fleet-stop` are both 401, that the read with
the cookie `dsh web` issued is 200, and that an authenticated action runs and
settles with its tail. That is what proved the cookie check was needed: the
route answered 200 to an anonymous caller until it was added. The scratch
instance is a check, not a deployment, and it runs on its own port.

Not covered: a rendered panel in a real browser, and the rebuild sequence
itself. Playwright is not installed in this checkout, so a browser gate could
not run here, and shipping one that had never executed would be worse than not
shipping one. The rendering rules that can be wrong in a way that matters are
decided in the host, where `node --test` reaches them: which tone a tile carries
and which state is broken. `db recreate -y -k` is destructive, so the plugin's
restore path is verified up to the guard and no further: the sequence is
unit-tested, and the job wrapper it runs in is the same one proven live. The
verb itself is compass's, and the repository runs it routinely.
