# **ORE Studio Sprint 15 – Release Notes**
*March 2026*

Sprint 15 delivered the BOINC-inspired distributed compute grid, a full service architecture migration to `*.api`/`*.core` layers, a production-grade JWT refresh system, and four new CDM instrument domain models.

---

## ✅ **Highlights**

- **Distributed compute grid** — full stack implementation (domain, SQL, service, wrapper, NATS JetStream, Qt UI) inspired by BOINC, with telemetry pipeline and RAG service health dashboard.
- **Service architecture migration** — all ten service libraries (`iam`, `refdata`, `compute`, `scheduler`, `assets`, `variability`, `synthetic`, `trading`, `reporting`, `http.server`) split into `*.api` (contracts) and `*.core` (implementation) layers.
- **JWT token refresh** — configurable lifetimes via system settings, `iam.v1.auth.refresh` endpoint, reactive shell retry, proactive Qt timer, and `ores_iam_auth_events_tbl` TimescaleDB telemetry hypertable.
- **CDM instrument models** — Rates (Swap, CrossCurrencySwap, CapFloor, Swaption), FX (FxForward, FxSwap, FxOption), Bond (Bond, ForwardBond, CallableBond, ConvertibleBond, BondRepo), and Credit (CDS, CDSIndex, SyntheticCDO).
- **Unified system settings** — replaced `ores_variability_feature_flags_tbl` with `ores_variability_system_settings_tbl` supporting bool, int, string, and JSON value types across all stack layers.

---

## 🛠️ **Key Improvements**

### **Compute Grid**
- BOINC-inspired grid with `ores.compute.service` and `ores.compute.wrapper` executables; NATS JetStream lifecycle transitions replace legacy PGMQ/pg_cron.
- Telemetry: `ores_compute_grid_samples_tbl` and `ores_compute_node_samples_tbl` hypertables; `get_grid_stats` unified NATS endpoint for dashboard.
- E2E fixes: result-stuck-InProgress bug, host_id persistence, outcome code mapping, SIGSEGV on window close, file extension preservation in artifact URIs.
- Loading lifecycle refactored via `AbstractClientModel` with `dataLoaded()`/`loadError()` signals wired to `endLoading()` across all six compute MdiWindows.

### **Authentication & Security**
- Token lifetimes configurable via `iam.token.*` system settings; hot-reloaded on `system_setting_changed` events.
- `make_request_context()` now explicitly returns `token_expired` / `unauthorized` error codes via `X-Error` NATS headers — updated across all 48 domain handlers.
- Auth telemetry records login, logout, refresh, max-session events with 90-day/3-year retention policies.

### **Architecture**
- All modules migrated to `*.api` / `*.core` three-layer pattern; HTTP route handlers relocated to domain core modules.
- NATS service discovery: Qt client auto-discovers HTTP base URL after login via `ores.http` shared protocol types — eliminates manual `http_port` config.
- Full PostgreSQL environment isolation: environment-prefixed roles prevent cross-environment contamination.

### **Infrastructure & DX**
- `sccache` CI bloat fixed: cross-PR cache sharing, increased sccache limit — reduced cold build times from 2+ hours.
- `ORES_PRESET` stored in `.env`; new `start-client.sh` supports multi-instance coloured Qt clients.
- Loading indicators (4px indeterminate progress bar) added to all 45+ entity list windows.
- Badge system Phase 1 infrastructure: `badge_definition` and `badge_mapping` domain entities with DB-driven metadata for reuse across Qt and Wt.

---

## ⚠️ **Known Issues & Postponed**

- **CDM CLI commands** (instruments list/add/delete) deferred to a follow-up story for all four instrument phases.
- **Database recreate** required to pick up new `ores_trading_fx_instruments_tbl`, `ores_trading_bond_instruments_tbl`, `ores_trading_credit_instruments_tbl` tables.
- **Three-level provisioning** (party wizard split, `provision-parties` endpoint, `ores.workflow` orchestration service) — Phase 1 landed late in sprint; Phases 2–3 deferred to Sprint 16.

---

## 📊 **Time Summary**

- **Total effort**: 86h 56m
- **Code**: 99% | **Agile/Analysis/Doc**: 1%
- Top tasks: E2E compute fixes (8h 43m), API extraction (8h 13m), CDM FX (8h 00m), CDM Bond (7h 00m), NATS service discovery (7h 10m), BOINC compute grid (7h 03m)

---

*Next sprint: Complete the three-level provisioning workflow service (`ores.workflow`), advance CDM to equity/commodity phases, and begin RBAC handler guards.*

---
