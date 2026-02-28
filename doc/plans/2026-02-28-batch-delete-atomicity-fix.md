# Plan: Batch Delete Atomicity Fix

## Context

The Gemini Code Assist review of PR #500 (feature/batch-save-protocol) flagged a critical issue:
delete handlers loop over items calling `svc.remove_X(code)` one at a time. Each call opens its
own DB transaction, so if the 3rd item fails after the first two succeeded, the result is a partial
delete — not atomic. The fix is to use sqlgen's `"col"_c.in(vector)` to generate a single
`DELETE FROM table WHERE col IN (...)` query in one transaction.

**Save handlers** are already atomic (repository `write(vector)` passes all to
`execute_write_query` in a single `begin_transaction | insert | commit`). Only delete handlers
need fixing.

sqlgen supports this natively: `delete_from<Entity> | where("code"_c.in(codes))` uses the
`InVec` struct from `conditions.hpp` which generates `WHERE code IN ($1, $2, ...)`.

## Scope

**23 entities** across 4 modules. **Skip**: UUID-based deletes (user confirmed OK), composite-key
deletes (backlog story — see below).

---

## Change Pattern (3 layers per entity)

### Repository `.hpp` — add batch overload
```cpp
void remove(const std::vector<std::string>& codes);
```

### Repository `.cpp` — implement using sqlgen `in()`
```cpp
void change_reason_repository::remove(const std::vector<std::string>& codes) {
    const auto query = sqlgen::delete_from<change_reason_entity> |
        where("code"_c.in(codes));
    execute_delete_query(ctx_, query, lg(), "batch removing change_reasons");
}
```

### Service `.hpp` — add batch method
```cpp
void remove_reasons(const std::vector<std::string>& codes);
```

### Service `.cpp` — delegate to batch repo method
```cpp
void change_management_service::remove_reasons(const std::vector<std::string>& codes) {
    reason_repo_.remove(codes);
}
```

### Handler `.cpp` — replace loop with single call
```cpp
// Before:
for (const auto& code : request.codes) { svc.remove_reason(code); }
// After:
svc.remove_reasons(request.codes);
```

---

## Entity-by-Entity Breakdown

### ores.dq — 7 entities

All use `request.codes`, column `"code"_c`, `ctx_` member variable.

| Handler (dq_message_handler.cpp) | Service method → new | Service file | Repo file |
|---|---|---|---|
| `handle_delete_change_reason_request` (L443) | `remove_reason` → `remove_reasons` | `change_management_service` | `change_reason_repository` |
| `handle_delete_change_reason_category_request` (L567) | `remove_category` → `remove_categories` | `change_management_service` | `change_reason_category_repository` |
| `handle_delete_coding_scheme_request` (L1625) | `remove_coding_scheme` → `remove_coding_schemes` | `coding_scheme_service` | `coding_scheme_repository` |
| `handle_delete_coding_scheme_authority_type_request` (L1785) | `remove_authority_type` → `remove_authority_types` | `coding_scheme_service` | `coding_scheme_authority_type_repository` |
| `handle_delete_nature_dimension_request` (L1942) | `remove_nature_dimension` → `remove_nature_dimensions` | `dimension_service` | `nature_dimension_repository` |
| `handle_delete_origin_dimension_request` (L2099) | `remove_origin_dimension` → `remove_origin_dimensions` | `dimension_service` | `origin_dimension_repository` |
| `handle_delete_treatment_dimension_request` (L2256) | `remove_treatment_dimension` → `remove_treatment_dimensions` | `dimension_service` | `treatment_dimension_repository` |

Service headers: `projects/ores.dq/include/ores.dq/service/`
Repo headers: `projects/ores.dq/include/ores.dq/repository/`

---

### ores.refdata — 12 entities

Most use `request.codes`/`"code"_c`. Exceptions noted below.

| Handler (refdata_message_handler.cpp) | Request field | Column | Service method → new | Service | Repo |
|---|---|---|---|---|---|
| `handle_delete_currency_request` (~L433) | `request.iso_codes` | `"iso_code"_c` | `delete_currency` → `delete_currencies` | `currency_service` | `currency_repository` |
| `handle_delete_business_centre_request` (~L625) | `request.codes` | `"code"_c` | `delete_business_centre` → `delete_business_centres` | `business_centre_service` | `business_centre_repository` |
| `handle_delete_country_request` (~L830) | `request.alpha2_codes` | `"alpha2_code"_c` | `delete_country` → `delete_countries` | `country_service` | `country_repository` |
| `handle_delete_party_type_request` (~L1004) | `request.codes` | `"code"_c` | `remove_type` → `remove_types` | `party_type_service` | `party_type_repository` |
| `handle_delete_party_status_request` (~L1173) | `request.codes` | `"code"_c` | `remove_status` → `remove_statuses` | `party_status_service` | `party_status_repository` |
| `handle_delete_party_id_scheme_request` (~L1342) | `request.codes` | `"code"_c` | `remove_scheme` → `remove_schemes` | `party_id_scheme_service` | `party_id_scheme_repository` |
| `handle_delete_contact_type_request` (~L1511) | `request.codes` | `"code"_c` | `remove_type` → `remove_types` | `contact_type_service` | `contact_type_repository` |
| `handle_delete_book_status_request` (~L3457) | `request.codes` | `"code"_c` | `remove_status` → `remove_statuses` | `book_status_service` | `book_status_repository` |
| `handle_delete_business_unit_type_request` | UUID-based | — | **SKIP** | — | — |
| `handle_delete_purpose_type_request` (~L3626) | `request.codes` | `"code"_c` | `remove_type` → `remove_types` | `purpose_type_service` | `purpose_type_repository` |
| `handle_delete_rounding_type_request` (~L3793) | `request.codes` | `"code"_c` | `remove_type` → `remove_types` | `rounding_type_service` | `rounding_type_repository` |
| `handle_delete_monetary_nature_request` (~L3960) | `request.codes` | `"code"_c` | `remove_type` → `remove_types` | `monetary_nature_service` | `monetary_nature_repository` |
| `handle_delete_currency_market_tier_request` (~L4128) | `request.codes` | `"code"_c` | `remove_type` → `remove_types` | `currency_market_tier_service` | `currency_market_tier_repository` |

Service headers: `projects/ores.refdata/include/ores.refdata/service/`
Repo headers: `projects/ores.refdata/include/ores.refdata/repository/`

**Note**: currency/business_centre use `delete_` prefix (not `remove_`) — batch method names
must follow the existing convention for each service.

---

### ores.trading — 4 entities

All use `request.codes`/`"code"_c`.

| Handler (trade_message_handler.cpp) | Service method → new | Service | Repo |
|---|---|---|---|
| `handle_delete_trade_type_request` (~L218) | `remove_type` → `remove_types` | `trade_type_service` | `trade_type_repository` |
| `handle_delete_lifecycle_event_request` (~L363) | `remove_event` → `remove_events` | `lifecycle_event_service` | `lifecycle_event_repository` |
| `handle_delete_party_role_type_request` (~L508) | `remove_role_type` → `remove_role_types` | `party_role_type_service` | `party_role_type_repository` |
| `handle_delete_trade_id_type_request` (~L653) | `remove_id_type` → `remove_id_types` | `trade_id_type_service` | `trade_id_type_repository` |

Service headers: `projects/ores.trading/include/ores.trading/service/`
Repo headers: `projects/ores.trading/include/ores.trading/repository/`

---

### ores.iam — 2 entities

**Special case**: IAM repos take `context ctx` as an explicit parameter (not `ctx_` member).
Also the WHERE column names differ and include a temporal guard `"valid_to"_c == max.value()`.

| Handler (accounts_message_handler.cpp) | Request field | Column | Service method → new | Service | Repo |
|---|---|---|---|---|---|
| `handle_delete_tenant_type_request` (~L2876) | `request.types` | `"type"_c` | `remove_type` → `remove_types` | `tenant_type_service` | `tenant_type_repository` |
| `handle_delete_tenant_status_request` (~L3049) | `request.statuses` | `"status"_c` | `remove_status` → `remove_statuses` | `tenant_status_service` | `tenant_status_repository` |

IAM batch remove pattern:
```cpp
void tenant_type_repository::remove(context ctx, const std::vector<std::string>& types) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<tenant_type_entity> |
        where("type"_c.in(types) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "batch removing tenant_types");
}
```

---

## save_result.hpp Cleanup

All symbols (`save_result` struct, `make_save_results_ok`, `make_save_results_error`,
`max_save_batch_size`) are **included but never referenced** in any `.cpp` file.

**Actions:**
1. Delete `projects/ores.comms/include/ores.comms/messaging/save_result.hpp`
2. Remove the `#include "ores.comms/messaging/save_result.hpp"` line from 4 files:
   - `projects/ores.trading/src/messaging/trade_message_handler.cpp`
   - `projects/ores.iam/src/messaging/accounts_message_handler.cpp`
   - `projects/ores.http.server/src/routes/refdata_routes.cpp`
   - `projects/ores.variability/src/messaging/variability_message_handler.cpp`

---

## Backlog Story

**Title**: [dq] Atomic batch delete for composite-key entities

**Description**: `handle_delete_subject_area_request` and `handle_delete_dataset_bundle_member_request`
in `dq_message_handler.cpp` loop over composite keys (2 columns), calling remove one-at-a-time.
sqlgen's `in()` only works on a single column. Possible approaches:
- PostgreSQL ROW-value `IN`: `WHERE (name, domain_name) IN (...)` via raw SQL
- Loop within a single explicitly opened transaction

Affected:
- `subject_area_repository` — key: `(name, domain_name)`
- `dataset_bundle_member_repository` — key: `(bundle_code, dataset_code)`

---

## Execution Order

1. Fix `save_result.hpp` (delete file + remove includes) — isolated, no dependencies
2. For each module (dq → refdata → trading → iam), per entity:
   a. Repository `.hpp` + `.cpp` — add batch `remove()` overload
   b. Service `.hpp` + `.cpp` — add batch method
   c. Handler `.cpp` — replace loop
3. Build: `cmake --build --preset linux-clang-debug`
4. Run tests: `cmake --build --preset linux-clang-debug --target rat`
5. Commit: `[comms/dq/refdata/trading/iam] Make batch deletes atomic via DELETE WHERE IN`
6. Add backlog story for composite-key cases

## Key Files

- `execute_delete_query` template: `projects/ores.database/include/ores.database/repository/bitemporal_operations.hpp`
- sqlgen `in()` for vectors: `Col::in()` in sqlgen headers — use `"col"_c.in(std::vector<std::string>)`
- Handler files modified: `dq_message_handler.cpp`, `refdata_message_handler.cpp`,
  `trade_message_handler.cpp`, `accounts_message_handler.cpp`
