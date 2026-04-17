# **ORE Studio Sprint 16 – Release Notes**
*April 2026*

Sprint 16 delivered a generalised event-driven workflow engine, full ORE XML round-trip coverage across 17 instrument families, a new market-data domain and storage service, per-asset-class typed instrument models, and a Qt plugin architecture for the client.

---

## ✅ **Highlights**

- **Generalised workflow engine** — `ores.workflow.service`, saga-based executor with FSM state UUIDs, startup recovery, correlation IDs, and a dedicated Qt Workflow Monitor plugin.
- **ORE integration (Phases 1–17)** — end-to-end ORE XML import/export: golden round-trip tests for every instrument family (rates, FX, bond, credit, equity, commodity, composite, scripted, exotic/hybrid), plus server-side `ores.ore.service` orchestrating imports over NATS + `ores.storage`.
- **ORE market-data domain model** — new `ores.marketdata.{api,core,service}` with TimescaleDB schema for series/observations/fixings, ORE key decomposition, server-side import of `market.txt` / `fixings.txt`, and a Market Data MDI UI.
- **Typed instrument domain model** — replaced the generic `ores_trading_instruments_tbl` with per-asset-class tables for rates, FX, and equity (bond/credit to follow); `instrument_family` renamed to `product_type`.
- **Qt plugin architecture** — `ores.qt.api` extracted as a shared framework; domain-specific plugins (`ores.qt.admin`, `ores.qt.compute`, `ores.qt.mktdata`, `ores.qt.refdata`, `ores.qt.trading`, `ores.qt.scheduler`) behind an `IPlugin` interface.

---

## 🛠️ **Key Improvements**

### **Workflow Orchestration**
- Saga engine orchestrates cross-service workflows (party provisioning, ORE import, report execution) with durable state, compensation steps, and startup recovery.
- Status migrated from ad-hoc strings/enums to database-driven FSM state UUIDs (`ores_workflow_fsm_*`); `fsm_state_map` utility resolves state names dynamically.
- `Nats-Correlation-Id` propagated across every handler via `log_handler_entry`; correlation IDs surfaced on the Tenant Provisioning Wizard summary page.
- Workflow Monitor Qt plugin + NATS query handlers; idempotent self-service JetStream stream provisioning replaces central bootstrap.

### **ORE Integration**
- Forward + reverse instrument mappers for credit, rates, FX exotics (barriers, digitals, accumulators, variance swaps), equity (options/forwards/swaps), bond (incl. `BondRepo`, `BondTRS`), commodity (swaps, APOs, option strips), composite, scripted, exotic/hybrid.
- Unified `map_instrument` dispatch via `std::variant`; round-trip fidelity tests for every family.
- Server-side `ores.ore.service` handles XML import over NATS; `ores.storage` provides S3-like object storage with PUT/GET/DELETE HTTP routes (replaces compute-specific buckets).
- Portfolio export to ORE XML, "Open Instrument" / "Export XML" actions in `BookMdiWindow`; auto-import of `market.txt` + `fixings.txt` alongside trade XML.
- ORE XML calendar adjustments and financial conventions (Zero, Deposit, Swap, OIS, FRA, IBOR, Overnight, FX, CDS) imported into refdata.

### **Market Data**
- `ores_marketdata_series_tbl`, `ores_marketdata_observations_tbl`, `ores_marketdata_fixings_tbl` with bitemporal history and tenant isolation.
- ORE key decomposition via `series_key_registry` for lossless parse/serialize.
- Qt Market Data MDI: list views for series + index fixings, time-series detail views, `MarketDataController`.
- `DataLibrarianWindow` and publish dialogs moved from `mktdata` to `refdata` plugin.

### **Instrument & Trade Model**
- Typed per-asset-class instrument tables and domain models for rates (Phase 0/1), FX (incl. barriers, digitals, accumulators, variance swaps), and equity (options/forwards/swaps/positions).
- `TradeDetailDialog` unification: FX, swap/rates, bond+credit, equity+commodity, composite, scripted instruments merged into a single dialog (6-phase refactor).
- `IInstrumentForm` registry + per-family form widgets; dynamic form activation via trade type cache; create-mode instrument capture.
- Data-driven trade-type metadata: typed `product_type_t` enum, `trading.v1.trade-types.list` NATS protocol, DB `ores_trading_trade_types_tbl`.

### **Asset Class Unification**
- `instrument_family` → `product_type` rename across SQL, C++, NATS, Qt (clarifies routing discriminator vs. risk taxonomy).
- Data-driven asset classes: new refdata service, repository, NATS protocol (`refdata.v1.asset-classes.list`); Qt combos loaded asynchronously; `asset_class` column added to trades.

### **Analytics**
- New `ores.analytics.{api,core,service}` component managing pricing engine types, model configurations, products, and parameters.
- Analytics CRUD UI with codegen support for combo-box widgets in detail dialogs; repository integration tests for all four entities.
- DQ project layout restructured; report definitions sourced from DQ artefact table (28 standard ORE analytics reports seeded) instead of hardcoded C++.

### **Service & Infrastructure**
- All services normalised onto NATS-based hosting via `ores.service` runners (domain, signing, Wt); unified heartbeats now the single source of truth for the service dashboard.
- New service controller: DB entities, NATS handlers, lifecycle events, replica-specific logging; service dashboard shows exit codes + last 20 log lines on failure.
- Multi-phase process shutdown (SIGTERM → SIGQUIT → SIGKILL) with Windows-safe `shutdown_signals` abstraction; TLS cert validation on NATS connections.
- **UTC-everywhere** policy: canonical UTC API in `ores.platform`, DB sessions pinned to UTC, ISO 8601 on the wire, display-only local time in UI.
- Session-level `Nats-Session-Id` header for per-session log correlation.

### **Report Execution**
- `report_execution_handler` for trade gathering + finalization; report execution workflow pipeline (Phase 3) with concurrency checks; `risk_report_config` domain model and repository.
- Party picker redesigned: business-center codes, party categories, system vs. operational filtering, flag icons via image cache.

### **Scheduler**
- Qt UI completed: Job Instances execution history + live Scheduler Monitor, backed by new NATS protocols and handlers.

### **Build, Portability & DX**
- Windows portability: replaced `strptime`/`_wfopen`/`SIGQUIT` with portable equivalents; MSVC `/Z7` embedded debug info to fix PDB contention under parallel Ninja; `trading.core` built as static lib to dodge 64 K DLL export limit; `registrar.cpp` split to avoid C1202 constexpr depth.
- macOS portability: portable file-time conversion replaces `clock_cast`.
- CI: MSYS2 path-mangling fixes in NATS cert generation; WiX 2 GB limit handled; stack size bumped to 8 MB for XML-parsing tests.
- Split-component codegen profiles (`api`/`core`/`service`) with automated file placement.
- Valgrind suppressions broadened for GLib `call_init` UB.

---

## ⚠️ **Known Issues & Postponed**

- **Asset class unification** — Phases 1 (`product_type` rename) and 2–5 (refdata seeding, validation, Qt data-driven combo) landed; remaining integration work (full Qt wiring of `asset_class` on trades) continues in Sprint 17.
- **Typed instruments** — rates, FX, and equity tables landed; **bond and credit** typed tables deferred to Sprint 17.
- **Party isolation RLS** — additional RLS policies across IAM/refdata/scheduler/trading landed (#681), but full enforcement via parent-table joins on trading instrument subtables remains open.
- **OpenTelemetry distributed tracing**, **three-level provisioning E2E tests**, **async workflow progress for large party hierarchies**, **trade-expiry and barrier-event workflows**, **positions domain model**, **party wizard UX improvements**, and **automated service registration** remain on the Sprint 16 backlog and are carried into Sprint 17.
- **Instrument creation in TradeDetailDialog** — several PRs (#649, #651) flagged regressions in create-mode flows (hidden tabs, broken save wiring); follow-up fixes tracked for Sprint 17.

---

## 📊 **Time Summary**

- **Pull requests merged**: 94 (since `v0.0.15`)
- **Estimated total effort**: ~159h 50m (~1h 42m avg per PR, estimated from commit timestamps via session-gap heuristic; see `:TIME_ESTIMATE:` properties on auto-generated stories under `* Unprocessed Stories` in `sprint_backlog_16.org`).
- **Dominant components**: `qt` (17), `workflow` (7), `ore` (7), `platform` (4), `trading` (4), `marketdata` (3), `sql` (3), `qt.api` (3).
- Refresh the backlog's `sprint_summary` clocktable in Emacs (`C-c C-x C-u`) for an authoritative tag/story breakdown.

---

*Next sprint: Complete the typed instrument rollout (bond, credit), finish asset-class unification wiring on trades, deliver OpenTelemetry distributed tracing, and extend `ores.workflow` with trade-expiry and barrier-event workflows.*

---
