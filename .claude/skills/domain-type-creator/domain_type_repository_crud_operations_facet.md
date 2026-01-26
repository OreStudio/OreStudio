This facet provides instructions on how to create repository with CRUD operations for a domain type.

Create the repository class:

1.  Header: `projects/COMPONENT/include/COMPONENT/repository/TYPE_NAME_repository.hpp`
2.  Implementation: `projects/COMPONENT/src/repository/TYPE_NAME_repository.cpp`

The repository should provide:

-   Constructor accepting a `context` parameter
-   `sql()` method returning the table creation SQL
-   Write methods (with **upsert semantics** - creates or updates):
    -   `void write(const domain::TYPE_NAME& obj)`
    -   `void write(const std::vector<domain::TYPE_NAME>& objs)`
-   Read methods:
    -   `std::vector<domain::TYPE_NAME> read_latest()`
    -   `std::vector<domain::TYPE_NAME> read_latest(const KEY_TYPE& key)`
    -   `std::vector<domain::TYPE_NAME> read_latest_since(std::chrono::system_clock::time_point modified_since)`
    -   `std::vector<domain::TYPE_NAME> read_all()`
    -   `std::vector<domain::TYPE_NAME> read_all(const KEY_TYPE& key)`
    -   `std::vector<domain::TYPE_NAME> read_at_timepoint(const std::string& timestamp)`
    -   `std::vector<domain::TYPE_NAME> read_at_timepoint(const std::string& timestamp, const KEY_TYPE& key)`

Implementation details:

-   Use sqlgen for database operations
-   Use the mapper to convert between domain and entity types
-   Include proper error handling with `ensure_success()`
-   Include logging using `BOOST_LOG_SEV`
-   Follow the pattern in `projects/ores.accounts/src/repository/account_repository.cpp`


# Required includes for the repository implementation

```cpp
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "COMPONENT/domain/TYPE_NAME_json_io.hpp" // IWYU pragma: keep.
#include "COMPONENT/repository/TYPE_NAME_entity.hpp"
#include "COMPONENT/repository/TYPE_NAME_mapper.hpp"
```

Note: `bitemporal_operations.hpp` provides `execute_read_query` and `execute_write_query` helpers. The `helpers.hpp` provides `generate_create_table_sql` and other utilities.


# Implementing read\_latest\_since for incremental loading

The `read_latest_since` method supports incremental cache loading by returning only items modified after a given timestamp. This method uses sqlgen's native query building with timestamp comparison.


## Repository implementation

```cpp
std::vector<domain::TYPE_NAME>
TYPE_NAME_repository::read_latest_since(context ctx,
    std::chrono::system_clock::time_point modified_since) {

    // Format timestamp for sqlgen query (thread-safe)
    const auto timestamp_str =
        platform::time::datetime::format_time_point_utc(modified_since);

    BOOST_LOG_SEV(lg(), debug) << "Reading latest items modified since: "
                               << timestamp_str;

    // Use sqlgen query with timestamp comparison
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto since_ts = make_timestamp(timestamp_str, lg());

    const auto query = sqlgen::read<std::vector<TYPE_NAME_entity>> |
        where("valid_to"_c == max.value() && "valid_from"_c >= since_ts.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<TYPE_NAME_entity, domain::TYPE_NAME>(ctx, query,
        [](const auto& entities) { return TYPE_NAME_mapper::map(entities); },
        lg(), "Reading latest items since timestamp");
}
```


## Required header include

Add the platform datetime utility for thread-safe timestamp formatting:

```cpp
#include "ores.platform/time/datetime.hpp"
```

See `projects/ores.assets/src/repository/image_repository.cpp` for a reference implementation.
