## Summary

Reporting gets its front door: /reporting lists one row per definition the session party runs, with the health of its latest run, the problems that run recorded and when it fires next, and /reporting/<name> states the definition and offers the fields a person may change. The BFF composes six reads into one row on the server, so the browser draws cells and derives nothing, and a read that fails empties its own cells and says why instead of failing the list.

A run that was not read is not a run that never happened. When the latest-run read fails, the row states `unknown` rather than `never run`, the tally counts it beside the five words, and the detail page says the run could not be checked.

## Changes

- Add the BFF module `reporting.ts` with `GET /api/reporting/reports`, `GET /api/reporting/reports/:name` and `PUT /api/reporting/reports/:name`, registered beside the counterparty routes. The definition read is the spine; the latest run, the health word, the problem counts, the next fire and the party code are decorations read in parallel, and a failed decoration leaves its cells empty and is named in `unread`.
- Grant `reporting::report_definitions:write` to `Member`, the code the put handler checks, which no person role held. Editing a definition is half of what the screen is for, and the row is party-scoped by the same row-level policy as the five reads phase 1 added.
- Add the list screen at `/reporting` and the definition screen at `/reporting/<name>`, with `PATHS.reporting`, the routes, the client methods and the catalogue entries in English, French and Portuguese. The edit form writes the six fields it shows; the BFF rebuilds the rest of the record from a read made at write time, so a save cannot blank a field the screen does not show, and the write carries the version the form was drawn from.
- Leave the Owner column and its read out: nothing records an owner and G10 is open. Leave the on-demand and paused words out: they are G14. Draw no Kind column, because a definition has no kind and a constant column is worse than none.
- Share the HTTP shapes in `wire-protocol/src/reporting.ts`, parse them at the browser boundary, and derive the duration in the browser, which owns the wire timestamp reader.
- Wire the three reads the screen composes into the reporting service's hand-written registrar: `get_latest_report_instances_by_definition`, `get_report_instance_health` and `get_report_error_counts`. The generated per-entity registrars declare them, but the aggregate the service actually runs never subscribed them, so every one of those reads answered 503 and the list showed every row as never run.
- Make an unread run its own state: the BFF leaves health absent when the latest-run read fails instead of reading the absence as never run, records the unread cell as the other decorations do, and counts those rows in a new `unknown` tally bucket. The detail response carries the `unread` reads, the list draws `unknown`, and the detail page says the run could not be checked.

## Traceability

| Artefact | Link | ID |
|----------|------|----|
| Story | [Manage reports from the list a party runs](https://orestudio.github.io/OreStudio/doc/agile/versions/v0/sprint_28/manage-reports-from-the-list-a-party-runs/story.html) | 6F7F7843-0A12-443A-A8FB-1A0FFA25E6CC |
| Task | [Build the reports a party runs screen and its endpoint](https://orestudio.github.io/OreStudio/doc/agile/versions/v0/sprint_28/manage-reports-from-the-list-a-party-runs/task_build_the_reports_a_party_runs_screen.html) | 93EB4EB3-086F-40A7-9A10-3CB7848ECE0E |
| Environment | bright_faraday | |

## Testing

Plan. Rebuild the reporting service with `compass build ores.reporting.service`, restart the fleet, and prove the subjects are served with `strings` on the built libraries and with a live `GET /api/reporting/reports` session through the BFF. Rebuild the web bundles and the BFF, restart them, and re-run `python3 build/capture-reporting.py`, which signs in through the BFF and drives headless Chrome to `build/screenshots/reporting-list.png` and `reporting-detail.png`. Run the BFF, web and wire-protocol suites, the web typecheck and `compass check --only org-links --only lint --exclude-heavy`.

Evidence. The rebuilt service serves all three reads: NATS reports one queue subscription each for `reporting.v1.ops.get_latest_report_instances_by_definition`, `reporting.v1.ops.get_report_instance_health` and `reporting.v1.ops.get_report_error_counts`, and a live `GET /api/reporting/reports` returns 200 with `unread: []` where it used to return a 503 for the latest-run read. @ores/bff: 802 tests passed in 68 files, including 12 reporting cases and a new one where the latest-run read fails (health absent, one unread entry for that cell, the other cells and their values intact). @ores/web: 1667 tests passed in 147 files, including 10 reporting cases and new ones where an absent health word draws `unknown` and not `never run`, and where a failed detail read says the run could not be checked. @ores/wire-protocol: 203 tests passed in 21 files. `npm run build` and `npm run typecheck` in projects/ores.web are clean. Local checks: org-links and lint pass, 2 of 2. The two captures are from the running environment on 20602 as `tenant_admin@acme_corporation`: the list carries no 503 banner, its tally reads 0 failed, 0 completed with errors, 0 running, 78 never run and 0 completed clean over 78 rows, and every row shows an em dash for the latest run, duration and problems; the detail page for Audit Trail Summary carries the `never run` badge and the sentence "This definition has never run. Nothing was checked, so it does not read as clean."

Limitations. ACME has no report run at all, so every row is genuinely `never run` and the healthy path is not visible: the captures show no `completed clean`, `running`, `failed` or `completed with errors` row, and no `unknown` row either, because no read failed after the fix. A later run would fill the latest-run, duration, health and problem cells from the run the reads now answer. The `unknown` path is covered by the three new tests and not by a capture, since producing it live would mean breaking the read on purpose. The write path is covered by a BFF test against canned replies, not against a live service, and the new grant is seeded but not read back from a populated database.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
