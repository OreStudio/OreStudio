*August 2026*

Sprint 24 continued commissioning work across components and pushed on IR generation. Acme Corporation shipped as ORE Studio's fully populated synthetic holding-group test entity, replacing Barclays Plc, with data-driven bundle provisioning. Calendar reference data was completed end to end: a holiday-aware date picker, real QuantLib holiday/adjustment materialisation, and a fix for a silent 100-row list-truncation bug affecting 29 refdata/marketdata entities. A configurable NATS wire format (JSON/MessagePack) rolled out across every service, fixing the base64 overhead problem for every message type. Entity classification and drift measurement was designed, built, and proven on the ores.refdata pilot, replacing several fragmented hand-maintained tracking documents with one automated report. The service runtime was proven containerised and running reproducibly on a remote WSL host. The sprint ran roughly twice its intended 7-day window (two health reviews, both RED) and the "improve IR generation" goal shipped only design work rather than a delivered improvement &#x2013; see the sprint's health reviews for the full analysis.

---


# ✅ Highlights

-   Acme Corporation shipped as ORE Studio's fully populated synthetic test entity, replacing Barclays Plc.
-   Calendar reference data completed end to end: holiday-aware date picker, real QuantLib holiday materialisation, and a fix for a silent list-truncation bug affecting 29 entities.
-   Configurable NATS wire format (JSON/MessagePack) rolled out across every service.
-   Entity classification and drift-measurement tooling designed, built, and proven on the ores.refdata pilot.
-   Service runtime proven containerised and running reproducibly on a remote WSL host.
-   Sprint ran roughly twice its intended duration; two health reviews (both RED) flagged the overrun and a goal-alignment gap on IR generation &#x2013; see the health reviews for the full analysis.


# 🛠️ Key Improvements


## Build & Portability

-   **Replace GLOB\_RECURSE with explicit source lists across the build**: Eliminate GLOB-based source discovery across the codebase so that a target's source list is always exactly what's checked in, with zero risk of a build silently using a stale file set after a rebase or new-file add &#x2013; the idiomatic CMake approach, as opposed to the CONFIGURE\_DEPENDS mitigation already landed.
-   **Hotfix: CMake 4.4 uninitialized-variable warnings on Linux CI**: GitHub Actions Linux CI builds started emitting a batch of new `CMake Warning (uninitialized)` messages starting 2026-07-20, under CMake 4.4.0: #+begin\_example CMake Warning (uninitialized) at &#x2026;/CMakeDetermineSystem.cmake:231 (configure\_file): uninitialized variable 'CMAKE\_SYSTEM\_CUSTOM\_CODE' CMake Warning (uninitialized) at &#x2026;/CMakeDetermineCXXCompiler.cmake:41 (if): uninitialized variable 'CXX' CMake Warning (uninitialized) at &#x2026;/CMakeDetermineCXXCompiler.cmake:60 (set): uninitialized variable '\_CMAKE\_TOOLCHAIN\_PREFIX' CMake Warning (uninitialized) at &#x2026;/CMakeDetermineCompiler.cmake:39 (set): uninitialized variable 'CMAKE\_CXX\_COMPILER\_HINTS' CMake Warning (uninitialized) at &#x2026;/CMakeDetermineCompilerId.cmake:27 (string): uninitialized variable 'CMAKE\_CXX\_COMPILER\_ARG1' #+end\_example All five originate from the `project(OreStudio ...)` call at `CMakeLists.txt:51`, tripped inside CMake 4.4.0's own bundled `CMakeDetermineSystem.cmake=/=CMakeDetermineCXXCompiler.cmake=/ =CMakeDetermineCompiler.cmake=/=CMakeDetermineCompilerId.cmake` modules on the GitHub Actions Linux runner.
-   **Windows CI: pip self-upgrade fails invoking pip.exe directly**: Restore the Windows CI job so it doesn't fail on the pip self-upgrade step in compass.sh's requirements install.


## Financial Features

-   **Commission Acme Corporation: a realistic holding-group test entity**: Barclays Plc has been our stand-in test/demo legal entity because it has a "realistic" corporate shape, but it is not fit for purpose: it has no employees, so we cannot exercise features that depend on staff/role identity — e.g.
-   **Market data cleanup: retire dead duplicate tables and preserve hand-written overrides**: Retire the dead pre-codegen marketdata tables (`ores_marketdata_series_tbl`, `ores_marketdata_observations_tbl`, `ores_marketdata_fixings_tbl`) so the schema, RLS policies, ER diagram, and schema-validator ignore list only describe tables that are actually in use — and stop the codegen-managed `market_observation` repository/service from silently losing its hand-written `series_id` filter and pagination logic every time `compass codegen regenerate` runs.
-   **Badge colour scheme: visual polish and self-badging fixes**: Split from Improve badge colour scheme support (Sprint 23) at close: that story shipped its core deliverable — the `badge_mapping` browser UI and the migration of `badge_definition=/=badge_severity=/ =code_domain` onto standard codegen + DQ publishing — but left two concrete, scoped defects unfixed: 1.
-   **Finish integrating calendar reference data: XML export and DQ publish**: Split from Sprint 23's Model calendars as proper ORE Studio reference data at close: that story shipped the calendar domain model, the FK relationships from currency/currency\_pair\_convention, the Qt CRUD screens, and the currency\_country junction (5/7 of its own acceptance).
-   **Market data notation: design an ORE Studio canonical URN mapping to ORE/Bloomberg/Reuters**: ORE Studio currently addresses market data ad hoc and inconsistently across components: - `fx_spot_generation_config` has an explicit `ore_key` field (`FX/RATE/EUR/USD`), derived deterministically from the currency pair.
-   **Sprint 24 quick bug fixes: currency CRUD, party re-provisioning**: Bundle three small, unrelated bugs pulled in from the inbox — each blocking or degrading basic functionality: 1.
-   **Party generator test expectations and business\_center\_code drift**: Restore ores.refdata.api.tests to green on main, and remove the business\_center\_code landmine from party.org before it reaches another regen.
-   **Qt as-of combo facet and primary-key model shape migration**: Make the as-of Qt combo logic a proper codegen facet instead of hand-patched generated output, and unblock regeneration of book/book\_status/portfolio via the new primary-key model shape.


## Service Architecture

-   **As-of lookup resolution codegen facet**: Give any bitemporal reference/lookup entity a codegen-generated way to answer "what did code X mean as of timepoint t" — a point-in-time query (`valid_from <` t and valid\_to >= t=), distinct from and unrelated to composite parent-child version bumping.
-   **Classify FK-like/lookup entities and decide badge vs image vs plain-text rendering policy**: Every FK-like/lookup/code entity in the system currently gets an ad hoc rendering treatment decided per-entity as it's built: `badge_definition` renders as a colour swatch, currencies render as flags, and everything else renders as plain text — with no documented policy for which treatment a given entity should get.
-   **Unify entity key modeling: one way to declare primary/natural keys, with compound-key support**: **Superseded framing (kept for history):** the original goal below talked about "unifying two templates in parallel active use for two key shapes." That premise was wrong — see the `* Findings` correction dated 2026-07-22.
-   **Fix jetstream\_admin missing gzip decompression**: Split out of Fix image-batch NATS payload overflow, add SVG compression once that story's core goal (compression + byte-size-aware batching) shipped: the one remaining loose end, raised non-blocking in PR #1706 review, doesn't belong under a now-closed story.
-   **Entity classification and drift baseline: ores.refdata pilot**: We do not know how far ORE Studio's entities are from "regular": an up-to-date codegen model, fully current generated code, and any manual code confined to paste blocks.
-   **Fix cross-tenant badge lookup: stale generated repositories ignore tenant\_read\_scope: shared**: Found by brave\_hopper: every badge in the UI renders with the default/fallback colour, for every entity, in every tenant except the system tenant.
-   **Hotfix: Revert PR #1788 (IAM model drift) — broken DB schema**: Restore `main` to a good state.
-   **Hotfix: xsdcpp domain drift on compositeTradeComponents SubTrade**: Restore the ORE XML domain model to match `instruments.xsd`.


## New Features

-   **Make party-scoped bundle provisioning data-driven, not one hand-written phase per bundle**: Discovered while implementing Synthetic data librarian support: FX foundation: adding a new dataset (`synthetic.fx_spot_configs`) to the `ore_analytics` bundle had **no effect** on either party-provisioning path, because neither actually publishes that bundle for reports.


## Qt UI

-   **Fix image-batch NATS payload overflow, add SVG compression**: Discovered while testing the vintage IR dataset story: country flags (and any other batch-loaded image set) were partially missing in the Qt client.
-   **Automate connections.db master-password unlock via CLI/env**: `ores.qt`'s connection-bookmark store (`connections.db`, managed by `ores.connections`) is protected by a master password that gates decryption of stored server-login credentials.


## Other

-   **Open sprint 24**: Open Sprint 24: scaffold the sprint doc, set its mission, wire it into the version manifest and agile index, bump the project version, and update the `vcpkg` submodule to latest.
-   **Improve FX Spot Monitor widget UI/UX**: A Gemini UI/UX review of the FX Spot Monitor widget (full audit and mockup) identified four concrete issues: (1) colors the entire currency-pair label red/green instead of reserving color for directional price deltas only, causing "Christmas tree" visual fatigue; (2) numeric columns aren't right-aligned, so decimal places don't stack for fast vertical scanning; (3) renders every pair to 5 decimal places, violating the standard JPY convention of 3 decimals; (4) repeats the literal text "LIVE" on every row instead of a compact status indicator.
-   **DQ/Refdata service boundary cleanup**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **IAM/Refdata service boundary cleanup**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Sprint 22 leftover cleanup**: Close out four standalone leftover tasks parked at sprint 22 close — each a genuinely single-task remainder from an otherwise-DONE story, not worth re-opening or carrying its whole parent.
-   **Refdata entity NATS event registrar audit**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Codegen developer experience improvements**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Stochastic tick arrival times for synthetic feeds**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Audit refdata entities for composite (temporal-versioned) child relationships**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Codegen: generate NATS subject constants per component**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Server-side UUID generation for all entities**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Audit and classify FK-like/lookup entities for badge/image/plain-text rendering**: Split from Roll out badge/image/plain-text rendering across classified domain entities (formerly the merged classification+rollout story) at sprint 24 health review: keep the completed audit/classification work recorded as shipped, separate from the still-STARTED implementation work that consumes it.
-   **Roll out badge/image/plain-text rendering across classified domain entities**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Sprint 24 closure: release notes and demo**: Own the final steps of sprint closure: release notes summarising what shipped, illustrative screenshots captured through a real QA test scenario (not ad hoc), and the retrospective.
-   **Junction eventing: extend codegen with notify-trigger, changed-event, and event-registrar facets for the junction model type**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Commission remaining DQ entities**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Codegen infrastructure follow-ups from DQ commissioning**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Calendar entity follow-ups: date picker, list-pagination fix, QuantLib materialization**: Pick up three follow-ups beyond Model calendars as proper ORE Studio reference data (Sprint 23, in progress) that aren't required by its own acceptance: a holiday-aware date picker widget, a list-truncation/pagination bug fix, and QuantLib calendar-holiday materialization.
-   **Calendar follow-ups: remaining hand-written list pagination cleanup**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Retire per-entity history dialogs: Phase C rollout**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **IR Rates synthetic data: dataset seeding, index cleanup, dual-curve, quoting conventions**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Rate display conventions: convention-aware formatting across all Qt rate surfaces (FX + IR)**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Migrate to oresmd, delete market\_series qualifier and ore\_key**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Extend oresmd to full ORE quote-type coverage**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Make NATS wire format configurable: JSON/MessagePack, decided once at startup**: Every NATS message (server responses, service handler decode, Qt client requests, shell client requests) is currently serialized by a hard-coded, inline `rfl::json::write=/=rfl::json::read` call at each individual send/receive site &#x2013; roughly 40+ distinct call sites spread across four codebases (`ores.service`, `ores.qt`, `ores.shell`, `ores.cli`).
-   **IR curve bootstrapping + official curve republish**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Commission ores.qt.admin into codegen**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Commission ores.qt.analytics into codegen**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Commission ores.qt.compute into codegen**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Commission ores.qt.marketdata into codegen**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Commission ores.qt.synthetic into codegen**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Commission ores.qt.trading into codegen**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Commission ores.qt.workspace into codegen**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Generalize as-of and as-of-bucket queries across repositories**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Add a server-side rate-history endpoint for chart panels**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Audit ores.synthetic.service headers for missing export macro**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Entity classification and drift baseline across all components**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Audit and standardize the display\_order field across lookup/code-table entities**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Containerize the ORE Studio service runtime and verify it on a remote WSL host**: Work out and prove, on a real remote WSL host (Newton), the mechanics of running ORE Studio's service runtime somewhere other than the local dev machine: containerize the service binaries (glibc mismatch ruled out a plain scp-binaries approach), solve the networking model for a host with its own native Postgres, and get all 18 services running reproducibly with a full Acme scenario passing end-to-end.
-   **Offload service and DB runtime to a WSL host over SSH**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Split ORE Studio services into one container per service**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Systemd resource management and per-environment isolation**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Migrate shared ccache/build storage to btrfs for reflink-safe hardlink-free caching**: Reformat the shared SSD (`/mnt/development`, currently a single 220G ext4 partition at 83% full, 36G free) to btrfs, and reconfigure ccache to use btrfs reflinks (`file_clone=true`, `hard_link=false`) instead of hard links, so concurrent worktree builds can no longer corrupt each other's cached object files via a shared inode &#x2013; the failure class root-caused in the CIR rename PR's review round (heap corruption crash in `ores.refdata.service` during Barclays provisioning, traced via GDB to a stale/corrupted `party_repository.cpp.o` that a ccache-disabled rebuild proved didn't match a fresh compile of the same source).
-   **Compass quality-of-life improvements**: Collect small, standalone improvements to compass itself — ergonomics, safety nets, and workstation-integration niceties — that are each too small to warrant their own story but are still real, shippable developer-experience work.
-   **Compass quality-of-life improvements: remaining follow-ups**: Carried unfinished to the product backlog inbox at sprint 24 close.
-   **Update vendored ORE Engine to 1.8.16.0**: The vendored ORE Engine binary package, examples, and XSD schemas are current, genuinely working, and safe to refresh again in the future without manual steps or silent regressions.
-   **Hotfix: remaining CMake 4.4 uninitialized-variable warnings**: Fix the four remaining CMake 4.4.0 uninitialized-variable warnings surfaced while fixing PR #1664's vcpkg-toolchain batch, but out of scope for that PR: two genuine use-before-define bugs in our own CMake code (`SPRINT`, `flags`), one dead-variable reference (`DOGEN_VERSION`), and CPack/InstallRequiredSystemLibraries vendored- module noise.
-   **Fix Windows CMake warning: uninitialized CMAKE\_THREAD\_LIBS\_INIT**: Eliminate the CMake warning fired at every `target_link_libraries()` call referencing the legacy `${CMAKE_THREAD_LIBS_INIT}` variable, which can be left uninitialized on some platforms/generators (e.g.
-   **Hotfix: default wire codec flip to msgpack breaks ores.qt.headless.tests**: Restore a green CI.


# ⚠️ Known Issues & Postponed

-   **IR generation goal not delivered**: the sprint mission's "improve IR generation" half shipped only design work (the oresmd URN scheme). Dataset seeding, dual-curve support, and quoting conventions are all still unstarted &#x2013; see the health reviews for the full analysis.
-   **Badge/image/plain-text rollout still in progress**: all 120 candidate entities are audited and classified, but implementation is only partway through its batches.
-   **Entity classification tool proven on one component only**: the drift-measurement tool is built and verified on the ores.refdata pilot; rollout to the remaining ~19 components hasn't started.
-   **WSL host offload not yet cut over**: the service runtime is containerised and proven running on a remote WSL host, but first-class compass tooling for the deploy and the actual day-to-day dev-workflow switch are still ahead.
-   **ores.wt.service crashes on startup (SEGV)**: found during this release's own manual verification pass, after the sprint's own work closed &#x2013; not yet investigated.


# 📈 Sprint Charts


## PRs and Commits per Day

Dual-axis bar chart. PRs (left axis) and commits (right axis) per day. A high commits-to-PR ratio may indicate scope creep.

![prs_commits.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_24/prs_commits.png)


## Daily Line Churn

Lines added (green) and deleted (red) per day. Building work produces mostly additions; refactoring produces a mix.

![line_churn.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_24/line_churn.png)


## PR Cycle Time

Hours from PR open to merge, one bar per PR. Long bars indicate review bottlenecks.

![pr_cycle.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_24/pr_cycle.png)


## Cumulative Stories Done

Line chart tracking stories marked DONE during the sprint. Steady upward slope is healthy; plateauing signals a stall.

![stories_done.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_24/stories_done.png)


# 📊 Time Summary

-   **Total effort**: not tracked
-   **PRs merged**: 151 (since v0.0.23, 2026-07-20 to 2026-08-03)
-   **Sprint duration**: 2026-06-25 → 2026-08-03

---

*Next sprint: rolling out the entity classification/drift tool to every remaining component; delivering actual IR generation improvements (dataset seeding, dual-curve, quoting conventions) now that the design work has landed; and the WSL-offload cutover using the proven containerised service runtime.*
