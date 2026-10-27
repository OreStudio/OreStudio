## Why

A report definition page shows only the definition. The scope, the analytics, the market and pricing configurations and the conventions a run reads are elsewhere, and the conventions cannot be changed at all. This story analyses the page and draws one page with tabs for all of it.

## Scope

- Story `see-and-change-everything-a-report-runs` with the analysis, the owner decisions and the work table.
- Prototype `doc/prototypes/reporting/see-a-report-and-everything-it-runs/` with six tabs: Definition, Scope, Analytics, Market and pricing, Conventions, History.
- The report currency carries its flag. The definition states end of day or intraday and no date. The Analytics tab lists every engine setting in five groups. The History tab draws the shared timeline.
- Design note `see-a-report-and-everything-it-runs.org`, a row on the group index, and a note on `change-what-a-report-runs.org` that its first panel is replaced.

Nothing under `projects/` is touched.

## Open for the owner

Decisions 2 to 5 in the story: the convention sets model, the behaviour for a kind a set does not cover, who may change a shared configuration, and where the scope editor lives. The History tab needs a server change first, because the reporting service never registers the definition history provider and reads by name.

## Verification

- `python3 build/scripts/check_prototype_inventory.py` passes.
- The Definition and Analytics tabs were rendered in headless Chrome and checked by eye.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
