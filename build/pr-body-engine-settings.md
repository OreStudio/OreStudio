## Why

The engine settings of a report are 20 columns on the run settings row. The review of that row, and of the ORE user guide and source, shows that the row mixes four field sets, that eight of its settings are not in the official documentation, and that the documentation of the rest has no home in the model. This change records the design and draws the screen as a prototype so the owner can sign it off before any model or screen is built.

## Scope

- Story `see-and-change-everything-a-report-runs` gains the design: the measurements, 14 findings, the comparison with the other named configurations, the central row with three parts, the documentation system, and the open questions.
- Task `design-the-engine-settings` with the goal and acceptance.
- Prototype `doc/prototypes/reporting/see-the-engine-settings-a-party-holds/`, built beside the pricing engines prototype: the list, and a record page with the tabs Details, one tab for each of three groups, Used by and History.
- Design note `see-the-engine-settings-a-party-holds.org` and a row on the group index.

Nothing under `projects/` is touched.

## What the prototype argues

- The engine settings become a named configuration that reports bind. A central row holds three parts: fixings and cash flows, market and trade building, and execution. A write to any part makes the next version of the whole.
- A setting is described only when the ORE user guide describes it. The model holds every documented engine setting, 15 in all. Where the guide and the source disagree, the source wins.
- The documentation of a setting is data of its own, apart from the data model, and holds no table of values. A setting shows its documentation collapsed. Advanced adds the ORE XML location, the release that introduced the setting, the release that removed it and the format.
- A person edits a group in place, not in a dialog. Each control starts with a "Default (…)" option, focusing a control opens its documentation, and the foot of the card states the reach and asks for a reason.
- Add `clean=1` to the address to see the page without the prototype furniture.

## Open for the owner

- Confirmation of the three groups and the 15 settings.
- Who may change a group, and whether the two groups that change the numbers need an approval.
- Where the target ORE build lives in the data model. A report does not state it today, the submit handler picks the highest `engine_version` and compares the versions as text, and the run records no build.
- Whether the currency and the market data kind move to the report definition in this story.

## Verification

- `python3 build/scripts/check_prototype_inventory.py` passes.
- `./compass.sh lint` passes.
- The edit flow was scripted in headless Chrome: edit, change, validation, save, documentation staying open, cancel, and focus opening the documentation.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
