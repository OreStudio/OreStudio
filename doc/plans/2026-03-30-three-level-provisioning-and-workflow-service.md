# Plan: Three-Level Provisioning and Workflow Orchestration Service

## Status

- Phase 1 (tenant/party wizard split): **In progress** — see PR feature/service-rbac-step2f
- Phase 2 (per-party status trigger, `provision-parties` endpoint): **Planned**
- Phase 3 (`ores.workflow` orchestration service): **Planned**

---

## Context

OreStudio uses a three-level account model: **System → Tenant → Party**. Each level has a
first-time setup wizard that runs on first login. Before this work the single
`TenantProvisioningWizard` conflated tenant-level setup (reference data, catalogue) with
party-level setup (organisation hierarchy, reports), which caused RLS failures when report
definitions were created under the system party but operational users belonged to a different party.

The goals of this plan are:

1. Split the provisioning experience cleanly across the three levels.
2. Replace the current global `system.party_setup_mode` flag (a stopgap) with per-party
   `status = 'Inactive'` as the wizard trigger.
3. Introduce a proper **`ores.workflow` orchestration service** for all multi-service workflows,
   starting with `provision-parties`.

---

## Three-Level Account Model

| Level | Who | Tables | Wizard trigger |
|-------|-----|--------|----------------|
| System | OreStudio operators | `ores_iam_tenants_tbl`, `ores_iam_*` system rows | `system.bootstrap_mode = true` |
| Tenant | Tenant IT admin | `ores_iam_accounts_tbl`, `ores_refdata_*` lookup tables | `system.bootstrap_mode = true` (tenant-scoped) |
| Party | Party admin / operational user | `ores_refdata_parties_tbl`, `ores_reporting_*`, `ores_scheduler_*` | `party.status = 'Inactive'` |

### Service Ownership

Each service **writes only to its own tables** (`ores_<SERVICE>_*`). Cross-service data
exchange must go via NATS request/reply, never via direct cross-schema DB writes.

| Service | Owns |
|---------|------|
| `ores.iam` | accounts, sessions, tenants, roles, permissions, account_parties |
| `ores.refdata` | parties, counterparties, currencies, countries, party lookup tables |
| `ores.variability` | system_settings |
| `ores.dq` | bundles, datasets, publication records |
| `ores.reporting` | report_definitions, report_runs |
| `ores.scheduler` | job_definitions |
| `ores.workflow` *(new)* | workflow_instances, workflow_steps (see Phase 3) |

**Known pre-existing violations** to fix in a follow-on PR:
- `bootstrap_handler.hpp` in `ores.iam` writes directly to `ores_refdata_parties_tbl`
- `auth_handler.hpp` in `ores.iam` queries `ores_refdata_*` directly for party lookup
- DQ bundle publication writes directly to `ores_refdata_*` tables

---

## Phase 1: Tenant/Party Wizard Split (this PR)

### What changed

The `TenantProvisioningWizard` was refactored from 10 pages down to 5:

**Before:** Welcome → BundleSelection → BundleInstall → DataSourceSelection → PartySetup →
CounterpartySetup → OrganisationSetup → ReportSetup → ReportInstall → Summary

**After:** Welcome → BundleSelection → BundleInstall → AccountSetupPage → Summary

The party-specific pages (DataSourceSelection through ReportInstall) were moved to a new
`PartyProvisioningWizard`.

### Stopgap: `system.party_setup_mode` flag

A tenant-scoped `system.party_setup_mode = 'true'` variability setting was added to trigger
the party wizard. This is a **temporary mechanism** that will be replaced in Phase 2 by
per-party `status = 'Inactive'`. The stopgap was introduced because:

- The architectural redesign (Phase 2/3) is too large to land in this PR.
- The wizard needs some trigger while the proper design is implemented.

The `party_setup_mode` flag is seeded alongside `bootstrap_mode` in
`ores_iam_provision_tenant_fn`. The login flow sets `party_setup_required = true` in
`login_response` when this flag is active and the account has no party.

**This approach has the following known limitations:**
- Global flag does not scale to multiple parties per tenant.
- No-party-login special case in `auth_handler.hpp` is a security workaround.
- Party accounts are created one at a time (one per tenant) rather than bulk.

---

## Phase 2: Per-Party Status Trigger and `provision-parties` Endpoint

### Overview

Replace the global `system.party_setup_mode` flag with per-party `status = 'Inactive'`.
Each party starts `Inactive` when created. The party wizard fires whenever the logged-in
user's current party is `Inactive`. The wizard sets the party to `Active` on completion.

This requires a new `provision-parties` workflow (see Phase 3 for how it fits into
`ores.workflow`). For Phase 2 it can be implemented as a single NATS endpoint while the
workflow service is built in Phase 3.

### Changes to `login_response`

Remove `party_setup_mode` field. Add:

```cpp
/**
 * @brief True when the selected party's status is 'Inactive'.
 * The client should present the PartyProvisioningWizard immediately.
 */
bool party_setup_required = false;
```

This is set in `auth_handler.hpp` after party selection by querying the party's status.

### Removing `system.party_setup_mode`

- Remove seeding from `ores_iam_provision_tenant_fn`.
- Remove `is_party_setup_mode_enabled()` / `set_party_setup_mode()` from
  `system_settings_service`.
- Remove `auth_is_party_setup_mode()` from `auth_handler.hpp`.
- Remove the no-party-login special case in `auth_handler.hpp`.

### `TenantProvisioningWizard` AccountSetupPage → PartyProvisionPage

Replace the single `AccountSetupPage` (creates one account) with `PartyProvisionPage`:

- GLEIF LEI picker for root party selection.
- Username base field (e.g. `"party_admin"`): per-party accounts derived as
  `<username_base>_<short_code>` (lowercased, spaces→underscore).
- Password field + confirm password.
- Calls `workflow.v1.provision-parties` (Phase 3) or a temporary
  `iam.v1.admin.provision-parties` endpoint.
- Shows async progress log.

On completion:
- Multiple parties created with `status = 'Inactive'`.
- One account per party, each associated with its party via `ores_iam_account_parties_tbl`.
- Wizard summary lists all provisioned usernames.
- Clears `system.bootstrap_mode` as before.

### `PartyProvisioningWizard` trigger change

Change `onLoginResult` in `LoginDialog.cpp`: emit `partySetupDetected()` when
`result.party_setup_required == true` (was `result.party_setup_mode`).

`PartyApplyAndSummaryPage::initializePage()` must:
1. Call `clientManager_->getParty(currentPartyId())` to fetch full party record.
2. Set `party.status = "Active"`.
3. Call `save_party_request`.
4. **Not** clear any variability system setting.

### `ClientManager` additions

Add helper:
```cpp
std::optional<refdata::domain::party> getParty(const boost::uuids::uuid& id);
```
Sends `get_parties_request` and filters by UUID. Used by `PartyApplyAndSummaryPage`.

### `select_party_response` change

Add `bool party_setup_required = false` to `select_party_response` in
`account_protocol.hpp`. Set in `account_handler.hpp` after party selection using the
same inactive-status check.

### File summary for Phase 2

| File | Change |
|------|--------|
| `ores.iam.api/messaging/login_protocol.hpp` | Rename `party_setup_mode` → `party_setup_required` |
| `ores.iam.api/messaging/account_protocol.hpp` | Add `party_setup_required` to `select_party_response` |
| `ores.iam.core/messaging/auth_handler.hpp` | Remove `auth_is_party_setup_mode`, no-party-login; add party status check |
| `ores.iam.core/messaging/account_handler.hpp` | Add `party_setup_required` to `select_party` response path |
| `ores.variability.core/service/system_settings_service.hpp` | Remove `party_setup_mode` methods |
| `ores.variability.core/service/system_settings_service.cpp` | Remove `party_setup_mode` implementations |
| `ores.sql/create/iam/iam_tenant_provisioner_create.sql` | Remove `party_setup_mode` seed |
| `ores.qt/ClientManager.hpp` | Rename field; add `getParty()` |
| `ores.qt/ClientManager.cpp` | Update mapping; implement `getParty()` |
| `ores.qt/LoginDialog.cpp` | Update `party_setup_mode` → `party_setup_required` |
| `ores.qt/TenantProvisioningWizard.hpp` | Replace `AccountSetupPage` with `PartyProvisionPage` |
| `ores.qt/TenantProvisioningWizard.cpp` | Implement `PartyProvisionPage`; update summary text |
| `ores.qt/PartyProvisioningWizard.hpp` | Add `currentPartyId_` state; rename `clearPartySetupFlag` |
| `ores.qt/PartyProvisioningWizard.cpp` | Replace flag-clear with `save_party_request` status=Active |
| `ores.qt/MainWindow.cpp` | Pass `currentPartyId` to party wizard |

---

## Phase 3: `ores.workflow` Orchestration Service

### Motivation

The `provision-parties` operation spans two services (IAM and Refdata):
1. Create party in Refdata → get `party_id`
2. Create account in IAM → get `account_id`
3. Associate `account_party` in IAM

Neither IAM nor Refdata should orchestrate the other. Client-side orchestration is ruled
out (must work from Qt, shell, wt). The correct solution is a dedicated **orchestration
service** (`ores.workflow`) that:

- Knows the sequence of steps.
- Holds no business logic (business rules stay in domain services).
- Persists workflow state for auditing, retries, and compensation.
- Exposes a NATS subject per workflow type.

Future workflows that will use the same infrastructure:

| Workflow | Steps |
|----------|-------|
| `provision-parties` | Refdata: create party; IAM: create account; IAM: associate |
| `trade-expiry` | Trading: expire trade; Risk: update positions; Reporting: trigger P&L recalc; Scheduler: remove jobs |
| `barrier-event` | Trading: knock-in/out; Risk: recompute Greeks; Reporting: trigger |
| `counterparty-default` | Trading: mark trades; Risk: compute CVA; Reporting: trigger credit report |

### Architecture

```
ores.workflow service
├── domain/
│   ├── workflow.hpp          — workflow instance (id, type, status, tenant_id, created_at)
│   ├── workflow_step.hpp     — step (id, workflow_id, name, status, request, response, error)
│   └── workflow_status.hpp   — enum: pending, in_progress, completed, failed, compensating, compensated
├── repository/
│   └── workflow_repository.hpp — persist/query workflow instances and steps
├── messaging/
│   ├── workflow_protocol.hpp     — NATS subjects, request/response types
│   └── workflow_handler.hpp      — dispatches incoming requests to workflow executors
└── workflows/
    ├── workflow_executor.hpp         — base interface: execute(), compensate()
    ├── provision_parties_workflow.hpp — concrete implementation
    └── (future: trade_expiry_workflow.hpp, etc.)
```

### NATS Subjects

```
workflow.v1.provision-parties          — start provisioning workflow (sync response)
workflow.v1.status                     — query workflow by id
workflow.v1.events.completed           — published on completion (JetStream)
workflow.v1.events.failed              — published on failure (JetStream)
```

Naming convention: `workflow.v1.<workflow-type>` for synchronous requests that return
a full result; async workflows return `workflow_id` immediately and publish events.

### Workflow State Machine

```
PENDING ──► IN_PROGRESS ──► COMPLETED
                       │
                       └──► FAILED ──► COMPENSATING ──► COMPENSATED
```

Each step has its own status mirroring the same state machine. Steps are persisted to
`ores_workflow_workflow_instances_tbl` and `ores_workflow_workflow_steps_tbl` before
execution, enabling restart after crash.

### `ores_workflow_*` Tables

```sql
-- Workflow instance
create table ores_workflow_workflow_instances_tbl (
    id              uuid primary key default gen_random_uuid(),
    tenant_id       uuid not null,
    type            text not null,           -- e.g. 'provision_parties'
    status          text not null,           -- pending/in_progress/completed/failed/...
    request_json    jsonb not null,          -- serialised request payload
    result_json     jsonb,                   -- serialised result on completion
    error           text,                    -- error message on failure
    created_by      text not null,
    created_at      timestamptz not null default now(),
    completed_at    timestamptz
);

-- Individual steps within a workflow
create table ores_workflow_workflow_steps_tbl (
    id              uuid primary key default gen_random_uuid(),
    workflow_id     uuid not null references ores_workflow_workflow_instances_tbl(id),
    step_index      int not null,
    name            text not null,           -- e.g. 'create_party', 'create_account'
    status          text not null,
    request_json    jsonb not null,
    response_json   jsonb,
    error           text,
    started_at      timestamptz,
    completed_at    timestamptz
);
```

No RLS needed on workflow tables — they are tenant-scoped by `tenant_id` and access is
controlled at the service level (the workflow service validates the JWT before creating
a workflow instance).

### `provision_parties` Workflow — Step Detail

**Request** (`workflow.v1.provision-parties`):

```cpp
struct provision_parties_request {
    struct party_spec {
        std::string lei;           // may be empty
        std::string full_name;
        std::string short_code;    // lower-case, no spaces
        std::string party_type;    // e.g. "Internal"
        std::string parent_lei;    // empty = root under system party
    };
    std::vector<party_spec> parties;
    std::string username_base;     // e.g. "party_admin"
    std::string initial_password;  // same for all accounts; must be reset on first login
    std::string created_by;
};
```

**Steps executed by `provision_parties_workflow`:**

```
For each party_spec:

  Step N.1 — NATS → refdata.v1.parties.save
    Request:  party with status='Inactive', party_category='Operational'
    Response: { success, party_id }
    Compensation: refdata.v1.parties.delete { ids: [party_id] }

  Step N.2 — NATS → iam.v1.accounts.save
    Request:  { principal: <username_base>_<short_code>, password, email }
    Response: { success, account_id }
    Compensation: iam.v1.accounts.delete { account_id }

  Step N.3 — NATS → iam.v1.accounts.add-party
    Request:  { account_id, party_id }
    Response: { success }
    Compensation: iam.v1.accounts.remove-party { account_id, party_id }
```

**Response** (`provision_parties_response`):

```cpp
struct provision_parties_response {
    struct party_result {
        std::string party_id;
        std::string account_id;
        std::string username;
        std::string lei;
    };
    bool success = false;
    std::string workflow_id;         // for audit/status queries
    std::string message;
    std::vector<party_result> provisioned;
};
```

### Username Derivation

`<username_base>_<short_code>` where `short_code` is lowercased and spaces replaced with
underscores.

Examples:
- `username_base = "party_admin"`, `short_code = "AcmeCorp"` → `party_admin_acmecorp`
- `username_base = "admin"`, `short_code = "UK Sub 1"` → `admin_uk_sub_1`

Uniqueness is enforced by the existing unique constraint on `ores_iam_accounts_tbl.principal`.
If a derived username already exists, the step fails with a descriptive error; the tenant
admin must choose a different base.

### Compensation Strategy

Compensation runs in reverse order on failure. Each step's compensation is a NATS
request to undo the step's effect:

| Step | Compensating action |
|------|---------------------|
| `create_party` | `refdata.v1.parties.delete` |
| `create_account` | `iam.v1.accounts.delete` |
| `add_party_to_account` | `iam.v1.accounts.remove-party` |

If a compensation step itself fails, the workflow moves to `FAILED` status and a
manual reconciliation record is written to `ores_workflow_workflow_steps_tbl` for
operator review.

### `refdata.v1.parties.delete` — New Endpoint

Currently `party_protocol.hpp` has `delete_party_request` with `std::vector<std::string> ids`.
Verify this exists and handles the `Inactive` status party correctly. If not, add it.

### `iam.v1.accounts.remove-party` — New Endpoint

Add to `account_protocol.hpp`:

```cpp
struct remove_account_party_request {
    using response_type = struct remove_account_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.remove-party";
    std::string account_id;
    std::string party_id;
};
```

Implement in `account_handler.hpp`.

### Service Registration

Add to `ores.workflow/src/messaging/registrar.cpp`:

```cpp
auto ph = std::make_shared<workflow_handler>(nats, ctx, signer);
subs.push_back(nats.queue_subscribe(
    provision_parties_request::nats_subject, qg,
    [ph](auto msg){ ph->dispatch(std::move(msg)); }));
```

The handler dispatches by request type to the appropriate `workflow_executor`.

### `ores.workflow` CMake Setup

New project: `projects/ores.workflow/`

```
projects/ores.workflow/
├── CMakeLists.txt
├── include/ores.workflow/
│   ├── domain/
│   ├── repository/
│   ├── service/
│   └── messaging/
└── src/
    ├── CMakeLists.txt
    └── messaging/
        └── registrar.cpp
```

Link against: `ores.iam.api.lib`, `ores.refdata.api.lib`, `ores.nats.lib`,
`ores.database.lib`, `ores.logging.lib`.

---

## Cross-cutting: Correlation IDs and Distributed Tracing

As the system grows to involve multi-service workflows (`provision-parties`, `trade-expiry`,
etc.) it becomes essential to correlate all log lines and NATS messages belonging to a single
logical operation. A correlation ID is a UUID generated at the entry point of a request and
threaded through every downstream call until a final response is returned to the caller.

### Design Principles

1. **One UUID per top-level request** — generated once at the entry point (Qt client or
   `ores.workflow`), never regenerated downstream.
2. **Carried in NATS message headers** — not embedded in the message body, so existing
   `*_request` and `*_response` structs do not need a new field. Services extract the header
   and attach it to their log context.
3. **Passed through every downstream NATS call** — a service that receives a message with
   a correlation header must forward that same header on every outbound NATS request it makes
   in handling that message.
4. **Logged at every service boundary** — each service logs `correlation_id=<uuid>` alongside
   `tenant_id` and `principal` at the start of every handler invocation.
5. **Included in workflow state** — `ores_workflow_workflow_instances_tbl` stores the
   correlation ID so the full trace can be reconstructed from the DB.
6. **Returned to the Qt client** — every `*_response` that may be shown in a wizard or
   error dialog should echo the correlation ID so support can cross-reference logs.

### NATS Header Convention

```
Nats-Correlation-Id: <uuid-v4>
```

Use the standard NATS `Nats-Msg-Id` header for NATS-level deduplication (JetStream). Use
`Nats-Correlation-Id` as the application-level trace key. This avoids conflicts with
JetStream's built-in deduplication.

### C++ Propagation Helpers

Add to `ores.nats` (e.g. `include/ores.nats/correlation.hpp`):

```cpp
namespace ores::nats {

/// Extract correlation ID from an inbound NATS message header.
/// Returns a new UUID if the header is absent (first hop).
std::string extract_or_generate_correlation_id(const nats_message& msg);

/// Copy the correlation ID header from one message to another.
void propagate_correlation_id(const nats_message& src, nats_message& dst);

/// Attach a correlation ID to an outbound NATS message.
void set_correlation_id(nats_message& msg, std::string_view correlation_id);

} // namespace ores::nats
```

Every handler that makes downstream NATS calls must:

```cpp
auto correlation_id = extract_or_generate_correlation_id(inbound_msg);
// ... log using correlation_id ...
auto outbound = build_request(...);
set_correlation_id(outbound, correlation_id);
nats.request(outbound, ...);
```

### Qt Client: Generating the Root ID

The Qt client generates the root correlation ID at the point of user action (button press,
wizard "Next" click). Example in `ClientManager`:

```cpp
auto cid = boost::uuids::to_string(boost::uuids::random_generator()());
auto msg = build_nats_message(request_payload);
ores::nats::set_correlation_id(msg, cid);
// store cid in LoginResult / WizardResult for display
```

The correlation ID should be stored in the wizard state (e.g.
`PartyProvisioningWizard::correlationId_`) and shown on the summary page:

```
Provisioning complete.
Correlation ID: 3f2504e0-4f89-11d3-9a0c-0305e82c3301
```

This allows operators to grep logs across all services for a single UUID.

### `ores.workflow` and Correlation IDs

The workflow service:

1. Extracts (or generates) the correlation ID from the incoming
   `workflow.v1.provision-parties` NATS header.
2. Stores it in `ores_workflow_workflow_instances_tbl.correlation_id` (new column).
3. Forwards the same header on every step NATS call (to `refdata.v1.*`, `iam.v1.*`, etc.).
4. Includes it in the `provision_parties_response`:

```cpp
struct provision_parties_response {
    // ...existing fields...
    std::string correlation_id;   // echo back for Qt display and support
};
```

### DB Schema Addition

```sql
alter table ores_workflow_workflow_instances_tbl
    add column correlation_id text;

create index on ores_workflow_workflow_instances_tbl(correlation_id);
```

The index allows `select * from ores_workflow_workflow_instances_tbl where correlation_id = $1`
for support queries.

### Logging Convention

All services should log correlation ID at `INFO` level at handler entry:

```
[iam.auth_handler] login attempt principal=alice tenant=acme correlation_id=3f2504e0-...
[refdata.party_handler] save_party lei=529900... correlation_id=3f2504e0-...
[workflow.provision_parties] step=create_party party=AcmeCorp correlation_id=3f2504e0-...
```

The `ores.logging` `make_logger` already supports structured key-value pairs. Add a
`with_correlation_id(id)` helper or pass the ID as a context parameter.

### File Summary for Correlation ID Work

| File | Change |
|------|--------|
| `ores.nats/include/ores.nats/correlation.hpp` | New: `extract_or_generate`, `propagate`, `set` helpers |
| `ores.nats/src/correlation.cpp` | Implement using `nats_msg_get_hdr` / `nats_msg_set_hdr` |
| `ores.iam.core/messaging/auth_handler.hpp` | Extract + log correlation ID; forward on downstream calls |
| `ores.iam.core/messaging/account_handler.hpp` | Same |
| `ores.refdata.core/messaging/party_handler.hpp` | Extract + log correlation ID |
| `ores.workflow/domain/workflow.hpp` | Add `correlation_id` field |
| `ores.workflow/messaging/workflow_protocol.hpp` | Add `correlation_id` to `provision_parties_response` |
| `ores.workflow/workflows/provision_parties_workflow.hpp` | Propagate header on all step NATS calls |
| `ores.sql/migrations/` | `alter table ores_workflow_workflow_instances_tbl add column correlation_id` |
| `ores.qt/ClientManager.hpp` | Generate root UUID; expose in result structs |
| `ores.qt/ClientManager.cpp` | Set header before every NATS publish |
| `ores.qt/PartyProvisioningWizard.hpp` | Store + display `correlationId_` on summary page |

---

## Phase 4: Party Wizard UX Improvements

These are follow-ons once Phases 2 and 3 are stable:

1. **Force password reset**: Add `password_reset_required = true` flag to accounts
   created by `provision-parties`. `auth_handler.hpp` already handles this flag.

2. **Multi-select LEI picker**: `LeiEntityPicker` currently supports single selection.
   Extend to multi-select for the `PartyProvisionPage` to allow selecting the full
   GLEIF hierarchy in one pass.

3. **Party wizard per-party customisation**: Allow each `PartyProvisionPage` entry to
   override the shared username/password with a per-party credential.

4. **Async wizard progress**: The `provision-parties` endpoint is currently synchronous.
   For large party hierarchies (> 20), switch to async: return `workflow_id` immediately
   and poll `workflow.v1.status` from the wizard's progress page.

---

## Phase 5: Extend `ores.workflow` for Financial Workflows

Once the service is established, the following workflows should be migrated to use it:

### `trade-expiry`

```
Step 1: trading.v1.trades.expire      { trade_id }
Step 2: risk.v1.positions.update      { trade_id }
Step 3: reporting.v1.runs.trigger-pnl { party_id, date }
Step 4: scheduler.v1.jobs.remove      { trade_id }

Compensation:
  Step 1: trading.v1.trades.reinstate  { trade_id }
  (Steps 2-4 are idempotent / can be safely retried without compensation)
```

### `barrier-event`

```
Step 1: trading.v1.trades.apply-barrier-event { trade_id, event_type }
Step 2: risk.v1.greeks.recompute              { trade_id }
Step 3: reporting.v1.runs.trigger             { party_id }
```

---

## Implementation Order

1. **Phase 2** (per-party status, provision-parties endpoint without workflow service):
   - Remove `party_setup_mode` global flag and no-party-login hack
   - Add `party_setup_required` based on `party.status`
   - Implement `provision-parties` as a temporary direct `iam.v1.admin.provision-parties`
     endpoint (IAM calls Refdata via NATS internally — acceptable as a stepping stone)
   - Update both wizard UIs

2. **Phase 3** (`ores.workflow` service):
   - Create service skeleton, DB tables, migration
   - Implement `provision_parties_workflow` executor
   - Move `iam.v1.admin.provision-parties` → `workflow.v1.provision-parties`
   - Update Qt wizard to call the new subject

3. **Phase 4** (UX polish): password reset flag, multi-select LEI picker

4. **Phase 5** (financial workflows): trade expiry, barrier event

---

## Open Questions

- **DQ bundle publication**: DQ currently writes to `ores_refdata_*` tables directly.
  This should be routed via `refdata.v1.*` NATS endpoints. Defer to a dedicated
  "DQ-Refdata boundary" PR — will require significant refactoring of the DQ publication
  pipeline.

- **`auth_handler.hpp` direct party lookup**: `auth_lookup_party` queries
  `ores_refdata_parties_tbl` directly. This should become a
  `refdata.v1.parties.get-by-id` NATS call. Defer alongside DQ work as a
  "refdata boundary cleanup" PR.

- **`bootstrap_handler.hpp` direct party write**: Same boundary violation as above.
  Defer to the same cleanup PR.
