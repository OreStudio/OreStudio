## Summary

- The report page gains a Configurations card. It lists the kind of each bound configuration and its name, which opens the configuration.
- New read-only configuration page at `/reporting/configurations/:id`: kind, owner, party, last-changed line, analytics (off ones marked) and the reports that bind it.
- The BFF serves both reads (`/api/reporting/reports/:id/configurations`, `/api/reporting/configurations/:id`). It accepts the non-v4 ids the seed gives configurations.
- The reporting service now registers the five generated configuration handlers (configurations, report configurations, configuration types, configuration analytics, analytic types). Nothing answered these subjects before. This also exposes their permission-checked write handlers.
- `Member` gets the five matching read grants in the populate script.
- Strings in English, French and Portuguese. Task doc gap ticked.

## Test plan

- [x] Web tests (1770), BFF tests (828), type checks, `org-links` and `lint` pass
- [x] Probed the live BFF as the ACME administrator: bindings, a configuration with analytics, and a 404
- [ ] Try both pages in a browser
- [ ] Re-run the IAM populate on a fresh database and check the `Member` grants
- [ ] Check the report-type change with bound configurations (only one report type exists today)

🤖 Generated with [Claude Code](https://claude.com/claude-code)
