## Summary

Four local checks were red on main, so every branch inherited a failed gate and the site build refused a page. None of the four was a branch's fault. This regenerates the artefacts whose sources had moved and fixes the link whose id case did not match the document that defines it.

While the branch was open, main moved again and brought more of the same staleness with it, so the rebase regenerates those too.

## Changes

- Regenerate six shell library scripts whose recipes had gained a parameter: three under `report_operations`, three under `swap_legs`, the latter for the new `payer` column
- Regenerate the ER diagram, which was missing two columns the schema had gained, `fixings_storage_key` and `payer`
- Spell the asset class catalogue's id in upper case in its own document, so the one link to it resolves
- Close the catch-up story and its two tasks

## Traceability

| Artefact | Link | ID |
|----------|------|----|
| Story | [Catch the generated artefacts up, and fix a dangling id link](https://orestudio.github.io/OreStudio/doc/agile/versions/v0/sprint_27/catch-the-artefacts-up/story.html) | F7E329C8-0127-40E9-84FE-FC8080FA1A25 |
| Task | [Scaffold story: Catch the generated artefacts up, and fix a dangling id link](https://orestudio.github.io/OreStudio/doc/agile/versions/v0/sprint_27/catch-the-artefacts-up/task_scaffold_catch-the-artefacts-up.html) | 584F61BB-4B70-4A09-9BF1-1AA27C7511AF |
| Environment | merry_newton | |

## Testing

Plan. The four checks that were red, plus the doc and drift checks that sit beside them, re-run after each rebase.

Evidence. Eight pass on the rebased tree: `tangle-shell`, `component-drift`, `er-diagram`, `site-page`, `model-drift`, `lint`, `org-links` and `structure-notes`. `pattern-uses` and `prototypes` also passed before the rebase. The page the site build used to refuse, `instruments-rates/design.html`, now returns 200 from the preview.

Limitations. The heavy tier (C++ build, ctest, pgTAP) was not run: this changes generated shell scripts, a generated diagram and one id, none of which those suites read.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
