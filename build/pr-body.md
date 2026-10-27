## Why

Reporting has no user interface at all. Five journeys describe the work: which reports a party runs, running one and waiting for it, reading what it produced, finding out what went wrong, and setting when it runs.

These prototypes exist to decide what the reporting surface is. Reading the journeys one to one would give five screens for one job, and nobody would build it that way, so the journeys are the design intent and the prototypes interpret them.

## Scope

- `doc/knowledge/journeys/reporting/` holds the group hub and five journeys. Each journey carries a PlantUML wireframe and its PNG.
- `doc/prototypes/reporting/` holds five prototypes, each with `index.html`, `prototype.css`, `prototype.js`, and a design note at `<name>.org`.
- `doc/prototypes/reporting/index.org` is the group page. `doc/prototypes/index.org` gains a Reporting row.
- Each journey's `* Prototype` section now links its prototype and its note, and names the directory with hyphens because the inventory gate requires that form.

Nothing under `projects/` is touched.

## Tradeoffs

One shared A/B/C axis across all five was tried and dropped. The question a catalogue asks is not the question a result asks, so each prototype turns on its own axis and the group page states which one. The method stays shared: three structural variants, one switcher, one voice.

The archive manifest in "read what a run produced" is drawn as a labelled fixture. No operation lists the files inside a run's archive, so drawing the manifest silently would have hidden the gap rather than argued about it.

The run prototype animates the six phases on a timer. No reporting read serves a run's current phase, so the page says the animation is a fixture and that the real ladder is a plan.

## Blast radius

Documentation only. No service, schema, generated artefact or application code changes, so trunk cannot break from this. The application still has no reporting UI when this lands, and the prototypes are the record of what was considered.

## Verification

- `python3 build/scripts/check_prototype_inventory.py` reports 66 prototypes in 19 groups, all listed and published, no React prototype tree.
- `compass check --only org-links --only lint --only prototypes --only backlog-indexes` passes 4 of 4.
- All fifteen variants screenshotted through headless Chrome and opened one by one. Every file is non-zero, and no variant logs a JavaScript error under `--enable-logging=stderr --v=0`.

The screenshots live in `build/proto-shots/`, which is gitignored, so they are not attached here. Every prototype reproduces them: open `doc/prototypes/reporting/<name>/index.html?variant=A` from a `file://` URL. There is no build step and no server.
