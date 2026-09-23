*September 2026*

Release notes for [Sprint 25](https://orestudio.github.io/OreStudio/doc/agile/versions/v0/sprint_25/sprint.html).

Sprint 25 set out to resolve codegen drift on as many components as possible and to progress the IR curves work. Six components became drift-free under one method (compass-codegen-fix-drift), and the drift checks now run in CI. The IR work added the FOMC-dated OIS short end, the full ORE quote-type coverage in oresmd, and six new IR process engines. Then the sprint changed direction: the Qt and Wt clients left the repository, ores.web took their place, and the NATS protocol got a canonical specification with C++ and TypeScript generated from one model. The sprint ran 52 days against a 7-day plan, with a lull of three weeks in the middle, and 23 of its 80 finished stories were hotfixes. At close, the sprint was reset: 23 stories were abandoned so that sprint 26 starts clean.

---


# ✅ Highlights

-   Six components are drift-free: ores.dq, ores.iam, ores.trading, ores.synthetic, ores.compute, and ores.analytics. ores.reporting has its bind merged. The codegen drift checks run in CI.
-   The Qt and Wt clients left the repository. ores.web, a TypeScript client, replaces them.
-   The NATS entity protocol has a canonical specification. IAM and refdata use it, and their C++ and TypeScript messages generate from one model.
-   oresmd covers every ORE quote type, including the new inflation and correlation families and a volatility-surface sub-schema.
-   The USD SOFR curve has an FOMC-dated short end, built on a calendar event model and flat-forward pillars.
-   ores.shell drives the compute grid and the trading entities with no UI. A smoke test dispatches jobs across every online node.
-   The feed control plane is asset-class agnostic: one producer seam, one controller, one ingest loop and one auto-start.
-   The skills catalogue moved to the compass- namespace, with cybernetic levels, cited principles, ported methods and the compass-helm mode.


# 🛠️ Key Improvements


## Codegen and drift

-   **Drift baseline, per component**: ores.dq, ores.iam, ores.trading, ores.synthetic, ores.compute, ores.analytics and ores.reporting bound to profiles, with zero-diff regeneration verified.
-   **Template fixes**: history provenance fields, compound-key gating, pagination, junction templates, RLS policy emission and the history-provider registrar.
-   **Model hygiene**: redundant explicit properties removed; the has\_parent\_id feature moved to the entity root so that profile bindings take effect.
-   **Checks**: the populate reference drift check, the drift checks in CI, and the compiler warnings (-Wall, -Wextra) the code assumed.
-   **Protocol**: IAM and refdata messages generate from one model into C++ and TypeScript.


## Market data and quant

-   **oresmd**: the legacy market\_series qualifier and ore\_key are gone, and every ORE quote type has an oresmd identifier.
-   **FOMC short end**: calendar events, a unified tenor resolution (FOMC, IMM and tenor), and the bootstrap segment.
-   **Process engines**: G2++ and Black-Karasinski are wired end to end. BDT, affine, HJM, LMM and Quadratic Gaussian engines exist, but they are not wired yet.
-   **Classification**: asset classes and series subclasses are refdata catalogs, a series can belong to more than one asset class, and the classifier covers the whole ORE corpus.
-   **Feeds**: one asset-class-agnostic control plane, and per-party consumption of the shared simulated market.


## Acme Corporation and the holding company

-   The holding company has FX and CRM visibility, a Group Treasury book, and a manual section.
-   The ACME fit-for-purpose review fixed tenant scoping, a TimeZone-dependent bitemporal sentinel, and silent provisioning failures.
-   Synthetic data has scope (system, tenant, party) and binding mode (bound, sandboxed).


## Platform and tooling

-   **systemd**: per-environment isolation and resource limits across the fleet, and a smaller database connection pool for each service.
-   **Remote WSL**: compass deploys the runtime and a compute node to a remote host.
-   **Testing**: an injectable environment provider, rerunnable test databases, and a hardened eventing chain.
-   **PR flow**: GitHub CI no longer gates pull requests. Targeted local checks and a local review run before a PR is raised, and the PR body carries a mandatory Testing section.
-   **ores.shell**: split into api and application parts.
-   **ORE**: the examples and XSD are synced to v1.8.17.0, and the engine package is vendored beside v1.8.16.0.


## Hotfixes

23 hotfix stories closed this sprint. 11 of them fixed Windows or MSVC builds, and two fixed gcc-only diagnostics. Continuous Windows was red from 2026-09-04 to 2026-09-17. Most of these defects compiled on Linux clang and failed on another compiler after the merge.


# ⚠️ Known Issues & Postponed

At close, 23 stories were abandoned. Each one records why in its Result.

-   **Codegen approaches (12 stories)**: the C++ refactor, the drift stories for ores.marketdata, ores.database, ores.scheduler, ores.workflow and ores.workspace, and the codegen stories for instruments, shell commands, protocol, the JSON-to-org migration, the trading data model and the NATS protocol. Sprint 26 replaces them with one method: sync codegen and clear drift for each module.
-   **Web and journeys (3 stories)**: the IAM web entities, the IAM journeys and the trade entry prototype. The journey documents stay. Journey and UX stories start again after the codegen pass.
-   **IR (2 stories)**: IR Rates synthetic data and the new stochastic processes. The IR implementation needs a refactor before more features land. The deferred capture "Refactor the IR rates implementation before more IR features land" holds this work.
-   **Removed clients (2 stories)**: the Market Simulator client and the desktop UI verification. The Qt client they targeted is gone.
-   **Other (4 stories)**: ORE v17 (both engines in provisioning, and the seven new trade types), the NATS config passthrough, trade populations, and oresmd as the native market data identity.
-   **Skills**: the catalogue story closed with 21 tasks. Its 28 open tasks are in the inbox story "Complete the LLM skills catalogue".


# 🔎 Post Mortem


## What worked

-   High throughput: 316 PRs merged and 80 stories closed DONE.
-   The drift method worked. Each component followed the same steps and reached zero drift.
-   PRs merged fast, with a median of 1.9 hours from open to merge.
-   The close reset recorded every abandoned story with a reason, so sprint 26 starts from a clean board.


## What didn't work

-   The sprint ran 52 days against a 7-day plan. It did not close at the lull, so a second full sprint of work went into the same document.
-   The mission did not change when the work moved to ores.web and TypeScript. The mission then described less than half of the work.
-   31 stories were STARTED at the close review, 10 of them with every task done. The documents did not show the real work in progress.
-   23 hotfixes. Cross-compiler failures were found by the scheduled CI runs, after the merge.
-   Many approaches to the codegen problem ran at the same time, and none of them finished.


## Improve next sprint

-   Close the sprint at day 7. A lull ends a sprint; it does not stretch it.
-   Change the mission when the direction changes, or open a new sprint.
-   Follow one order: codegen sync and drift for each module, then user journeys, then UX.
-   Run a gcc or MSVC build before a merge that touches CMake, headers or compiler flags.
-   Close a story when its last task closes.


# 📈 Sprint Charts


## PRs and Commits per Day

Dual-axis bar chart. PRs (left axis) and commits (right axis) per day. A high commits-to-PR ratio may indicate scope creep.

![prs_commits.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_25/prs_commits.png)


## Daily Line Churn

Lines added (green) and deleted (red) per day. Building work produces mostly additions; refactoring produces a mix.

![line_churn.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_25/line_churn.png)


## PR Cycle Time

Hours from PR open to merge, one bar per PR. Long bars indicate review bottlenecks.

![pr_cycle.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_25/pr_cycle.png)


## Cumulative Stories Done

Line chart tracking stories marked DONE during the sprint. Steady upward slope is healthy; plateauing signals a stall.

![stories_done.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_25/stories_done.png)


# 📊 Time Summary

-   **Total effort**: not tracked
-   **PRs merged**: 316 (since v0.0.24, 2026-08-03 to 2026-09-23)
-   **Sprint duration**: 2026-08-03 → 2026-09-23 (52 days)

---

*Next sprint: sync codegen and clear drift for each module, starting with the modules that are not drift-free yet (ores.marketdata, ores.database, ores.scheduler, ores.workflow, ores.workspace). User journey analysis and UX follow the codegen pass.*
