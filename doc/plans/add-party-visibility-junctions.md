# Add Party-Visibility Junction Tables

**Branch**: `claude/add-party-visibility-junctions-jZCWW`
**Date**: 2026-02-22

## Background

Reference data (currencies, countries) is shared at the tenant level. When
a user working on behalf of a party selects a currency or country (e.g.
populating a book's currency field) they currently see the entire tenant-wide
list. This story adds two junction tables — `party_currencies` and
`party_countries` — that control which currencies and countries each party
can see and use.

The underlying reference data definitions remain shared at the tenant level;
only **visibility** is per-party.

## Design

### Junction table pattern

The implementation follows the existing `party_counterparty` junction exactly.
The key difference is that currencies and countries use **text** primary keys
(`iso_code` and `alpha2_code`) rather than UUIDs for the right-side column.

```
party_counterparty  : party_id (uuid) → counterparty_id (uuid)
party_currencies    : party_id (uuid) → currency_iso_code (text)
party_countries     : party_id (uuid) → country_alpha2_code (text)
```

All junction tables are bitemporal (valid_from / valid_to), tenant-scoped,
and support optimistic locking via a version counter. Insert triggers handle
versioning; a delete rule soft-deletes by setting valid_to to the current
timestamp.

### Filtering integration

Two new methods are added to the existing services:

- `currency_service::list_currencies_for_party(party_id, offset, limit)`
- `country_service::list_countries_for_party(party_id, offset, limit)`

These are implemented at the service level to keep repositories decoupled:
1. Read visible codes from the junction repository for the party.
2. Fetch all currencies/countries from the main repository.
3. Filter to the visible set.
4. Apply pagination on the filtered result.

## Files Changed

### New files (38)

| Layer | Files |
|-------|-------|
| Codegen models | `ores.codegen/models/refdata/party_currency_junction.json` |
| | `ores.codegen/models/refdata/party_country_junction.json` |
| SQL create | `ores.sql/create/refdata/refdata_party_currency_create.sql` |
| | `ores.sql/create/refdata/refdata_party_country_create.sql` |
| SQL drop | `ores.sql/drop/refdata/refdata_party_currencies_drop.sql` |
| | `ores.sql/drop/refdata/refdata_party_countries_drop.sql` |
| C++ domain headers | `ores.refdata/include/.../domain/party_currency.hpp` |
| | `ores.refdata/include/.../domain/party_currency_json_io.hpp` |
| | `ores.refdata/include/.../domain/party_currency_table.hpp` |
| | `ores.refdata/include/.../domain/party_currency_table_io.hpp` |
| | `ores.refdata/include/.../domain/party_country.hpp` |
| | `ores.refdata/include/.../domain/party_country_json_io.hpp` |
| | `ores.refdata/include/.../domain/party_country_table.hpp` |
| | `ores.refdata/include/.../domain/party_country_table_io.hpp` |
| C++ domain sources | `ores.refdata/src/domain/party_currency_json_io.cpp` |
| | `ores.refdata/src/domain/party_currency_table.cpp` |
| | `ores.refdata/src/domain/party_currency_table_io.cpp` |
| | `ores.refdata/src/domain/party_country_json_io.cpp` |
| | `ores.refdata/src/domain/party_country_table.cpp` |
| | `ores.refdata/src/domain/party_country_table_io.cpp` |
| C++ generators | `ores.refdata/include/.../generators/party_currency_generator.hpp` |
| | `ores.refdata/src/generators/party_currency_generator.cpp` |
| | `ores.refdata/include/.../generators/party_country_generator.hpp` |
| | `ores.refdata/src/generators/party_country_generator.cpp` |
| C++ repository headers | `ores.refdata/include/.../repository/party_currency_entity.hpp` |
| | `ores.refdata/include/.../repository/party_currency_mapper.hpp` |
| | `ores.refdata/include/.../repository/party_currency_repository.hpp` |
| | `ores.refdata/include/.../repository/party_country_entity.hpp` |
| | `ores.refdata/include/.../repository/party_country_mapper.hpp` |
| | `ores.refdata/include/.../repository/party_country_repository.hpp` |
| C++ repository sources | `ores.refdata/src/repository/party_currency_entity.cpp` |
| | `ores.refdata/src/repository/party_currency_mapper.cpp` |
| | `ores.refdata/src/repository/party_currency_repository.cpp` |
| | `ores.refdata/src/repository/party_country_entity.cpp` |
| | `ores.refdata/src/repository/party_country_mapper.cpp` |
| | `ores.refdata/src/repository/party_country_repository.cpp` |
| C++ tests | `ores.refdata/tests/repository_party_currency_repository_tests.cpp` |
| | `ores.refdata/tests/repository_party_country_repository_tests.cpp` |

### Modified files (6)

| File | Change |
|------|--------|
| `ores.sql/create/refdata/refdata_create.sql` | Added `\ir` entries for the two new create scripts |
| `ores.sql/drop/refdata/refdata_drop.sql` | Added `\ir` entries for the two new drop scripts |
| `ores.refdata/include/.../service/currency_service.hpp` | Added `list_currencies_for_party`, `count_currencies_for_party`; added `junction_repo_` member |
| `ores.refdata/src/service/currency_service.cpp` | Implemented the new filtering methods |
| `ores.refdata/include/.../service/country_service.hpp` | Added `list_countries_for_party`, `count_countries_for_party`; added `junction_repo_` member |
| `ores.refdata/src/service/country_service.cpp` | Implemented the new filtering methods |

## SQL Table Schemas

### ores_refdata_party_currencies_tbl

```sql
party_id           uuid      not null  -- FK: ores_refdata_parties_tbl.id
tenant_id          uuid      not null
currency_iso_code  text      not null  -- FK: ores_refdata_currencies_tbl.iso_code
version            integer   not null
modified_by        text      not null
performed_by       text      not null
change_reason_code text      not null
change_commentary  text      not null
valid_from         timestamptz not null
valid_to           timestamptz not null
PRIMARY KEY (tenant_id, party_id, currency_iso_code, valid_from)
```

### ores_refdata_party_countries_tbl

```sql
party_id            uuid      not null  -- FK: ores_refdata_parties_tbl.id
tenant_id           uuid      not null
country_alpha2_code text      not null  -- FK: ores_refdata_countries_tbl.alpha2_code
version             integer   not null
modified_by         text      not null
performed_by        text      not null
change_reason_code  text      not null
change_commentary   text      not null
valid_from          timestamptz not null
valid_to            timestamptz not null
PRIMARY KEY (tenant_id, party_id, country_alpha2_code, valid_from)
```

## Repository API

### party_currency_repository

```cpp
explicit party_currency_repository(context ctx);
std::string sql();
void write(const domain::party_currency& pc);
void write(const std::vector<domain::party_currency>& pcs);
std::vector<domain::party_currency> read_latest();
std::vector<domain::party_currency> read_latest_by_party(const boost::uuids::uuid& party_id);
std::vector<domain::party_currency> read_latest_by_currency(const std::string& iso_code);
void remove(const boost::uuids::uuid& party_id, const std::string& iso_code);
void remove_by_party(const boost::uuids::uuid& party_id);
```

### party_country_repository

```cpp
explicit party_country_repository(context ctx);
std::string sql();
void write(const domain::party_country& pc);
void write(const std::vector<domain::party_country>& pcs);
std::vector<domain::party_country> read_latest();
std::vector<domain::party_country> read_latest_by_party(const boost::uuids::uuid& party_id);
std::vector<domain::party_country> read_latest_by_country(const std::string& alpha2_code);
void remove(const boost::uuids::uuid& party_id, const std::string& alpha2_code);
void remove_by_party(const boost::uuids::uuid& party_id);
```

## Service API additions

### currency_service

```cpp
std::vector<domain::currency> list_currencies_for_party(
    const boost::uuids::uuid& party_id,
    std::uint32_t offset, std::uint32_t limit);

std::uint32_t count_currencies_for_party(const boost::uuids::uuid& party_id);
```

### country_service

```cpp
std::vector<domain::country> list_countries_for_party(
    const boost::uuids::uuid& party_id,
    std::uint32_t offset, std::uint32_t limit);

std::uint32_t count_countries_for_party(const boost::uuids::uuid& party_id);
```

## Testing

Run with Catch2 tag filters:

```sh
ores.refdata.tests "[repository][party_currency]"
ores.refdata.tests "[repository][party_country]"
```

Test coverage per type:
- `write_single_*` — single junction record write
- `write_multiple_*` — bulk write
- `read_latest_*_by_party` — look up visible entries for a party
- `read_latest_*_by_currency/country` — reverse lookup
- `remove_*` — soft-delete via bitemporal rule
- `read_nonexistent_*` — empty result for unknown party

## Build

```sh
cmake --preset linux-clang-debug          # configure
cmake --build --preset linux-clang-debug  # build
```
