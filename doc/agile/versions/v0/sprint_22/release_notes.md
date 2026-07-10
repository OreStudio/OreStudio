*July 2026*

![ore_studio-v0.0.22.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/assets/images/ore_studio-v0.0.22.png)

Sprint 22 set out to continue commissioning the ores.refdata entity library and correct codegen C++ generation drift, carrying postponed sprint 21 stories forward. It ran long — 9 days against a 7-day/300-commit budget, closing at 741 commits and 99 merged PRs — but delivered a lot in the process: currency and its four auxiliary reference types (rounding\_type, monetary\_nature, currency\_market\_tier) plus book and book\_status all fully commissioned end to end, a complete new Currency Pairs & Conventions entity with duplicate/ inverted-pair rejection and a full manual chapter, a new `ores.orgmode` component and the QA Validation Runner built on it, a proven-out org-mode literate codegen model, working temporal composite-entity versioning, and a cluster of real Windows/macOS CI and multi-tenant correctness fixes. Of the sprint's 70 stories, 40 shipped DONE; the remaining 30 — including most of the entity-commissioning backlog and the codegen-drift-correction work the mission actually named — carry forward to future sprints with a tighter WIP limit.

---


# ✅ Highlights

-   Currency Pairs & Conventions: a new reference-data entity shipped end to end — domain model, codegen, Qt UI (including duplicate/ inverted-pair rejection and cross-navigation), and a full user-manual chapter with captured screenshots.
-   `ores.orgmode` (new C++ component: org-mode parsing + org-roam `id:` link resolution) and the QA Validation Runner built on it — an in-app Qt panel that turns a `test_scenario` doc into a guided, trackable test session across the fleet.
    
    ![ore_studio_scenario_runner.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/assets/images/ore_studio_scenario_runner.png)
-   Org-mode literate codegen proven on `party` (PR #963): one org file now drives both C++ and SQL generation with inline custom methods, replacing the two-JSON-file model for future entity migrations.
-   Codegen support for view groups (Qt detail-dialog tabs), piloted on currency.
-   A real multi-tenant correctness fix: tenant-scoped variability system settings other than `bootstrap_mode` weren't seeded per tenant, silently hiding gated features for every non-system tenant.
-   Windows/macOS CI reliability: the MSVC C4273 dllimport failure (via a new `ores.qt.headless` extraction), a stale-Qt-plugin-after-ABI- change crash, 20 missing `EXPORT` macros, and a silently-skipped macOS build matrix — all root-caused and fixed, not papered over.
-   Currency and its four auxiliary reference types (rounding\_type, monetary\_nature, currency\_market\_tier, plus book/book\_status) fully commissioned end to end: Qt UI, shell, CLI, and manual chapters.
-   Temporal composite entity versioning: SQL touch-function mechanism and as-of window-join read-side composition, so a parent loaded as-of version N composes its children as of that same version.
-   Composite child-entity and hierarchy Qt widgets for codegen: the hand-maintained hierarchy tree and child-entity table are now a generic, paste-block-bound codegen facet.
-   Full sweep of orphaned docs and tag taxonomy in the org-roam graph, with a new tag inventory and compass tag validation.


# 🛠️ Key Improvements


## Build & Portability

-   **Fix Windows dllimport build failure via ores.qt.headless extraction**: Get the Windows build green again, and do it by fixing the actual architectural mismatch that caused the failure rather than papering over the compiler warning that surfaced it.
-   **Investigate RegulatoryBookTypeController Windows LNK2019 error**: Determine whether a pasted Windows LNK2019 build failure reflects a real defect on `main` (and fix it if so), or is a stale/CI-caching artifact not worth a speculative code change.


## Financial Features

-   **Commission: party\_type**: Commission `party_type` across all access layers: verify the Qt UI end-to-end post-NATS migration, implement shell commands (list, add, remove — **not yet implemented**), implement CLI commands (list, add, remove — **not yet implemented**), fix any regressions found, and add the entity chapter to the user manual.
-   **Commission: business\_day\_convention\_type**: Discovered while modelling fx\_convention: `business_day_convention_type` lives in `ores.trading` (`ores.trading.business_day_convention_type`, now removed), but every `*_convention` entity that would actually reference it (`ibor_index_convention`, `fx_convention`, `swap_convention`, etc.) already lives in `ores.refdata`.
-   **Codegen: generate per-entity NATS event-mapping registrar**: Every domain entity's live NATS change-event pipeline (Postgres NOTIFY channel -> in-process event\_bus -> re-published NATS event) is currently wired by hand in each component's service/src/app/application.cpp: one #include, one register\_mapping<T\_changed\_event>() call, and one event\_bus.subscribe<T\_changed\_event>() lambda per entity, scattered across three places.
-   **Configurable feed status indicator in the FX spot grid**: `FxSpotGridWindow` currently shows per-row feed connection status (PENDING/LIVE/STALE/DISCONNECTED) via hardcoded CSS pill styling, bypassing the app's shared badge infrastructure (`BadgeCache=/=BadgeLabelUtils`) used for categorical state everywhere else in the UI.
-   **GMM improvements: tidy up synthetic data generation loose ends**: Tidy up loose ends left over from the GMM-based synthetic market data generation work (FX spot PoC and its config UI): give the preview charts readable axes, make each GMM component's PDF visible alongside the combined mixture PDF, and close the gap on stochastic process "component tools" that were designed but never implemented — Ornstein-Uhlenbeck in particular.
-   **Synthetic data librarian support: FX foundation**: When a party is created, the librarian gives it a usable synthetic data configuration out of the box, so downstream generation, publishing, and consumption work without manual setup per party.
-   **ores.shell command for market data import**: `import_market_data_request` (`projects/ores.marketdata/api/include/ores.marketdata.api/messaging/import_protocol.hpp`, served by `ores.marketdata.core::service::import_service`) is only ever triggered by the Qt `ImportTradeDialog` (`projects/ores.qt/trading/src/ImportTradeDialog.cpp`) — there's no non-interactive way to run a market data import.
-   **Introduce per-entity NATS sub-registrars**: Replace the single monolithic `registrar.cpp` in `ores.refdata.core` with a generated per-entity sub-registrar pattern.
-   **Codegen support for view groups (Qt detail-dialog tabs)**: Restore the ability to group an entity's detail-dialog fields into separate tabs — lost when currency was migrated onto the shared Qt codegen templates — as a generic, reusable codegen concept (`view_group`) rather than a currency-specific patch, piloted on currency (the entity that had it and lost it), and document the result in the manual.
-   **Fix uninitialized asset\_class/series\_subclass in market\_series generator**: Eliminate the two Valgrind uninitialized-memory defects the nightly `linux-gcc-debug-ninja` dynamic-analysis run flagged in `ores.marketdata.core.tests` so the CDash dashboard reports a clean run for this component.
-   **Fix missing ORES\_REFDATA\_CORE\_EXPORT on refdata/scheduler service classes**: Every \*\_service class meant to be called across .so boundaries (party\_service, portfolio\_service, and 17 other refdata services, plus scheduler's job\_definition\_service) is missing its ORES\_<COMPONENT>\_CORE\_EXPORT annotation.


## Service Architecture

-   **Reconcile currency Qt custom features with codegen**: Bring currency's Qt layer (`ores.qt/refdata`: `ClientCurrencyModel`, `CurrencyController`, `CurrencyMdiWindow`, `CurrencyDetailDialog`, `CurrencyHistoryDialog` + `.ui` files) in line with the current qt codegen profile, without silently losing any of the real functionality it has grown beyond the template.
-   **Codegen SQL variability for hypertable, bi-temporal and GIST patterns; model ores.marketdata entities**: Extend the codegen SQL template with three variability flags needed to model ores.marketdata entities: (1) TimescaleDB hypertable creation, (2) soft-update/soft-delete bi-temporal triggers, (3) GIST exclusion constraints.
-   **ores.orgmode: C++ org-mode parsing and org-roam link resolution**: First story of the **QA Validation Runner** epic.
-   **Fix NATS port collision with legacy env and label inconsistency**: Any environment's NATS ports should be trivially identifiable from its `ORES_BASE_PORT` alone, and a legacy/untracked checkout should never be able to silently steal another environment's ports without at least a loud, diagnosable failure.


## Other

-   **Open sprint 22**: Open sprint 22: scaffold the sprint, wire it into the version manifest and agile index, and carry postponed sprint 21 stories forward.
-   **Commission: currency**: Full commissioning across Qt, shell, CLI, and the user manual.
-   **Commission: rounding\_type**: Aux type fully commissioned — SQL, codegen, Qt UI, currency-chapter documentation.
-   **Commission: monetary\_nature**: Aux type fully commissioned — SQL, codegen, Qt UI, currency-chapter documentation.
-   **Commission: currency\_market\_tier**: Aux type fully commissioned — SQL, codegen, Qt UI, currency-chapter documentation.
-   **Commission: book**: Full commissioning across Qt, shell, CLI, and the user manual.
-   **Commission: book\_status**: Full commissioning across Qt, shell, CLI, and the user manual.
-   **Commission: purpose\_type**: Commissioned across Qt, shell, and CLI; manual chapter carried forward.
-   **Currency pair support in reference data**: Knowledge-base foundation (conventions, quoting, cross rates) plus domain/db/codegen support for currency pairs as reference data, shipped end to end.
-   **Temporal composite entity versioning** (architecture + implementation): SQL touch-function mechanism and as-of window-join read-side composition landed; the Qt composite-history dialog carries forward alongside the party/counterparty consolidation work it's gated on.
-   **Composite child-entity and hierarchy Qt widgets for codegen**: HierarchyTreeWidget + child-entity table facet, paste-block-bound, so party/counterparty can retire their hand-maintained EntityDetailDialog.
-   **Fix orphan documents and tag taxonomy**: Fixed orphaned docs in org-roam graph; created a tag inventory; added compass tag validation.
-   **QA Validation Runner: in-app test tracking for the multi-worktree fleet**: Qt panel, built on ores.orgmode, turning a test\_scenario org doc's checklist into a guided test session; results and story/task context rendered in-app, not a standalone JSON file. Piloted end to end against a real sprint story.
-   **Regen entities with latest codegen templates**: Country regenerated and reconciled against the shared codegen templates' new capability landed via currency's commissioning — generator bug fix, SQL bootstrap-filter/clock\_timestamp fixes, and a new needs\_item\_delegate template generalization.
-   **Commission: party**: Verify Qt; implement shell + CLI; manual; Wt/HTTP captures.
-   **Commission: party\_status**: Verify Qt; implement shell + CLI; manual; Wt/HTTP captures.
-   **Commission: party\_id\_scheme**: Verify Qt; implement shell + CLI; manual; Wt/HTTP captures.
-   **Commission: contact\_type**: Verify Qt; implement shell + CLI; manual; Wt/HTTP captures.
-   **Commission: counterparty**: 3/3 codegen-hardening tasks done; Qt/shell/CLI/manual scope still open.
-   **Commission: business\_unit**: Verify Qt; implement shell + CLI; manual; Wt/HTTP captures.
-   **Commission: business\_centre**: Verify Qt; implement shell + CLI; manual; Wt/HTTP captures.
-   **Commission: portfolio**: Verify Qt; implement shell + CLI; manual; Wt/HTTP captures.
-   **Commission: day\_count\_fraction\_type**: Move from ores.trading to ores.refdata (update trading\_swap\_legs' one reference), then commission fully to Qt; spun off from the currency pair story.
-   **Book data model cleanup**: 2/9 tasks done (the 2 live bugs); add classification flags, rename ledger\_ccy, investigate legal-entity/branch fields, add rates centre, analyse allowed lists, enrich manual with domain concepts remain.
-   **Rethink synthetic reference-data generation across entities**: Currency's ad-hoc Generate feature (client-side blue-highlighted unsaved rows, per-entity hand-wiring) is half-baked and not actually save-able; design a proper cross-entity mechanism for single + batch synthetic generation before reintroducing it anywhere.
-   **Market data cleanup: retire dead duplicate tables and preserve hand-written overrides**: Drop legacy pre-codegen ores\_marketdata\_series/observations/fixings\_tbl (superseded, zero C++ references); remove their stale validation\_ignore.txt entries; preserve market\_observation's hand-written repository/service overrides against codegen regeneration.
-   **Full market.txt import hits duplicate observation key**: Importing the full ORE reference vintage file `external/ore/examples/Legacy/Example_56/Input/market.txt` via `ores.marketdata.core::service::import_service` (subject `marketdata.v1.import`) hits a Postgres unique-constraint violation on `_hyper_*_market_observations_observations_current_uniq`: duplicate key `(tenant_id, party_id, series_id, observation_datetime, point_id)`, e.g.
-   **IR Rates synthetic data generation**: Short-rate stochastic process + Curve Template raw instrument generation (deposits/FRAs/swaps), tenor-collision validation, tick-batch publishing — no bootstrapping, no vol surfaces, no QuantLib.
-   **Synthetic data collections: Basic and Realistic**: 6/9 tasks done — basic/realistic bundles, FX quote inversion detection, 2016 vintage import, ORE reference vintage dataset, and the vintage guard all landed; feed namespacing and Market Simulator grouping by collection remain.
-   **Move trade import orchestration to service layer**: Move ImportTradeDialog's ~300 lines of trade-import orchestration (XML parse, per-trade save, 30-way instrument dispatch) into a server-side trade\_import\_service + import\_trades\_request, so ores.shell's future trade import command doesn't duplicate it.
-   **Decommission ores.codegen.table model type**: Migrate all remaining \_table.org models to ores.codegen.entity; remove the table type from the facet catalogue.
-   **Refactor ores.codegen C++ generation**: 6/19 tasks done — the audit, template-bug fixes, model-consistency fixes, and breaking-change decisions all landed; applying the resulting safe drift to each of the 12 remaining components is next.
-   **Decommission legacy codegen bash scripts**: Replace direct bash invocations with compass or Python codegen; remove the scripts.
-   **Codegen CI zero-diff invariant**: CI job regenerating all registered components, failing on any diff vs HEAD.
-   **Codegen model safety guardrails**: Runtime guards for missing component\_include/component\_core; block dual-template SQL generation.
-   **Codegen unified model — Phase 1: Qt derivation**: Derive Qt fields from conventions; drop explicit NATS protocol class names from models.
-   **Codegen unified model — Phase 3: unify temporal templates**: Merge temporal and non-temporal C++ template families into single model-controlled templates.
-   **Codegen developer experience improvements**: Fix spurious ERRORs in &#x2013;component mode; add &#x2013;entity filter; add entity group / tag system for domain-scoped generation.
-   **Resolve codegen model unification blockers**: Carried from sprint 21.
-   **compass generate catalogue**: Auto-regenerate catalogue/index org files by scanning tagged org files.
-   **Compass quality of life (Sprint 21)**: Improve compass quality of life with a mix of UX enhancements and bug fixes surfaced in Sprint 20 and 21 usage.
-   **Add compass client start/stop subcommands**: Add `compass client start` and `compass client stop` subcommands so the Qt client follows the same start/stop/status convention as every other Operate-pillar command (`services`, `env`, `nats`), instead of being a flat launch-only command with no way to stop an instance via compass.
-   **Compass GitHub Actions support**: Give compass a quick overview of all GitHub Actions runs, highlight which ones are failing, and for failing runs surface the error detail via gh CLI without needing to download full logs.
-   **Verify Windows and macOS CI builds**: Re-run MSVC and macOS builds; verify the rfl C1202 fix; fix or file follow-ups.
-   **Upgrade Qt to latest supported version to fix macOS 15 AGL removal**: Qt 6.8.x references -framework AGL (removed in macOS 15); the latest supported Qt drops that dependency.
-   **Canary CI: near-zero sccache hit rate makes every build a cold rebuild**: Diagnose and fix why `canary-linux.yml` (`linux-gcc-debug-ninja`) gets almost no benefit from its sccache cache, so canary stops taking ~3h/run for small PRs.
-   **Fix tenant-scoped variability system settings: permissions and per-tenant seeding**: `projects/ores.iam/core/include/ores.iam.core/messaging/tenant_handler.hpp:198-203` (`complete_tenant_provisioning`) directly constructs a `variability::service::system_settings_service` using IAM's own database context and calls `set_bootstrap_mode(false, ...)` to clear the tenant's bootstrap flag on activation.
-   **Improve site documentation discoverability**: Carried from sprint 21.
-   **Hotfix: MacOS CI relevance-guard skips matrix build silently**: Restore real signal from the "Continuous MacOS" scheduled workflow: it must not report overall Success when the actual build/test matrix job was skipped.
-   **Hotfix: refdata generator/repository test fixtures out of sync with DB constraints**: Get CI green again: 25 failing ores.refdata unit/repository tests on main, caused by two "Commission: counterparty" codegen-facet commits (`5ef46554b`, `d5762d746`) that introduced generator values violating DB check constraints and diverging from existing test expectations.
-   **Hotfix: stale Qt plugin causes crash after ores.qt.api ABI change**: An ordinary `cmake --build --target ores.qt.exe` must never leave a QPluginLoader-loaded plugin .so stale against a freshly-linked exe's ABI — CMake needs a real build-order dependency from the exe to every plugin lib, even though none of them are link-time dependencies of it.


# ⚠️ Known Issues & Postponed

-   **Commission: party** (ABANDONED): deferred.
-   **Commission: party\_status** (ABANDONED): deferred.
-   **Commission: party\_id\_scheme** (ABANDONED): deferred.
-   **Commission: contact\_type** (ABANDONED): deferred.
-   **Commission: counterparty** (ABANDONED): deferred.
-   **Commission: business\_unit** (ABANDONED): deferred.
-   **Commission: business\_centre** (ABANDONED): deferred.
-   **Commission: portfolio** (ABANDONED): deferred.
-   **Commission: day\_count\_fraction\_type** (ABANDONED): deferred.
-   **Move book, book\_status, and portfolio to ores.trading** (ABANDONED): premise rejected — book/portfolio are correctly classified as ores.refdata per Murex/Calypso static-data precedent; not recaptured. See [BROKEN LINK: 130A3261-E788-4222-9BD7-371AD92982FB]§"Where this lives in ORE Studio". A follow-on Qt-layer inconsistency (Portfolio's CRUD UI still under `ores.qt/trading`) was captured separately.
-   **Book data model cleanup** (ABANDONED): deferred.
-   **Rethink synthetic reference-data generation across entities** (ABANDONED): deferred.
-   **Market data cleanup: retire dead duplicate tables and preserve hand-written overrides** (ABANDONED): deferred.
-   **IR Rates synthetic data generation** (ABANDONED): deferred.
-   **Synthetic data collections: Basic and Realistic** (ABANDONED): 6/9 tasks done; feed namespacing and Market Simulator grouping by collection remain.
-   **Move trade import orchestration to service layer** (ABANDONED): deferred.
-   **Decommission ores.codegen.table model type** (ABANDONED): deferred.
-   **Refactor ores.codegen C++ generation** (ABANDONED): 6/19 tasks done (audit, template bugs, model consistency, breaking-change decisions); the 12 real per-component "apply safe drift" tasks (refdata pilot, trading, iam, dq, analytics, reporting, scheduler, workflow, controller, database, workspace, compute) remain.
-   **Decommission legacy codegen bash scripts** (ABANDONED): deferred.
-   **Codegen CI zero-diff invariant** (ABANDONED): deferred.
-   **Codegen model safety guardrails** (ABANDONED): deferred.
-   **Codegen unified model — Phase 1: Qt derivation** (ABANDONED): deferred.
-   **Codegen unified model — Phase 3: unify temporal templates** (ABANDONED): deferred.
-   **Codegen developer experience improvements** (ABANDONED): deferred.
-   **Resolve codegen model unification blockers** (ABANDONED): deferred.
-   **compass generate catalogue** (ABANDONED): deferred.
-   **Compass GitHub Actions support** (ABANDONED): deferred.
-   **Verify Windows and macOS CI builds** (ABANDONED): deferred.
-   **Upgrade Qt to latest supported version to fix macOS 15 AGL removal** (ABANDONED): deferred.
-   **Improve site documentation discoverability** (ABANDONED): deferred.


# 📈 Sprint Charts


## PRs and Commits per Day

Dual-axis bar chart. PRs (left axis) and commits (right axis) per day. A high commits-to-PR ratio may indicate scope creep.

![prs_commits.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_22/prs_commits.png)

No day fell below 3 PRs merged; the two peaks (07-08: 14 PRs/137 commits, 07-09: 17 PRs/93 commits) land in the final two days as work converged toward what became this closure, not a mid-sprint sprint. Every day this sprint individually exceeds what several past sprints managed in total — the chart reads less like a normal cadence than like nine consecutive high-intensity days with no cooldown.


## Daily Line Churn

Lines added (green) and deleted (red) per day. Building work produces mostly additions; refactoring produces a mix.

![line_churn.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_22/line_churn.png)

Two sharp spikes (07-06 and 07-08, both ~25-27k lines added) correspond to the currency-pair codegen/Qt sync work and the currency Qt codegen reconciliation landing in large, template-driven regenerations rather than hand-written diffs — expected for this codebase's codegen-heavy change pattern, but still a scale of churn that makes any single day's review meaningfully harder.


## PR Cycle Time

Hours from PR open to merge, one bar per PR. Long bars indicate review bottlenecks.

![pr_cycle.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_22/pr_cycle.png)

Cycle times cluster comfortably under 24h for most PRs, with two outliers early in the sprint (PRs #1386 at ~34h and #1408 at ~36h) and a smaller bump near the close (#1470 at ~26h). No systemic review bottleneck — the sprint's problem was volume of work started, not slow turnaround on any given PR.


## Cumulative Stories Done

Line chart tracking stories marked DONE during the sprint. Steady upward slope is healthy; plateauing signals a stall.

![stories_done.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_22/stories_done.png)

Steady, close-to-linear growth throughout — the sprint was never stalled. The gap between this chart's healthy slope and the RED health reviews is itself the finding: stories closed at a reasonable clip the whole time, but new stories opened faster than old ones closed (70 stories were in the sprint by close, up from 51 at the day-4 review), so the completion rate was never the actual problem.


# 📊 Time Summary

-   **Total effort**: not tracked
-   **PRs merged**: 99 (since v0.0.21, 2026-07-02 to 2026-07-10)
-   **Sprint duration**: 2026-07-02 → 2026-07-10

---

*Next sprint: work the 30 carried stories with a hard WIP limit on simultaneously-STARTED work, and pick up the codegen drift correction the mission named but this sprint barely moved.*
