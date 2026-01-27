This facet provides instructions on how to create tests for a domain type.

Create comprehensive tests for the new domain type under `projects/COMPONENT/tests/`:

1.  Domain tests: test JSON and table serialization
2.  Generator tests: verify generator produces valid instances
3.  Repository tests: test all CRUD operations

Follow the test patterns in [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.iam/testsprojects/ores.iam/tests).


# Domain test requirements


## Assert all set fields

When testing domain object creation, add CHECK assertions for **all** fields that are set on the test object, not just a subset. This ensures all fields are being assigned correctly.

```cpp
TEST_CASE("create_entity_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    entity sut;
    sut.version = 1;
    sut.name = "Test Entity";
    sut.description = "Test description";
    sut.recorded_by = "admin";
    sut.change_commentary = "Initial creation";
    BOOST_LOG_SEV(lg, info) << "Entity: " << sut;

    // Assert ALL fields that were set
    CHECK(sut.version == 1);
    CHECK(sut.name == "Test Entity");
    CHECK(sut.description == "Test description");
    CHECK(sut.recorded_by == "admin");              // Don't forget audit fields
    CHECK(sut.change_commentary == "Initial creation");
}
```


## Table conversion tests

When testing table conversion, verify that the output includes:

1.  Expected table headers (column names)
2.  Expected field values from the test data

Use C++23's `std::string::contains()` method for substring checks instead of `find() !` std::string::npos=:

```cpp
TEST_CASE("entity_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    entity e;
    e.name = "Test Name";
    e.description = "Test description";
    // ... other fields ...

    std::vector<entity> entities = {e};
    auto table = convert_to_table(entities);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    // Check for headers
    CHECK(table.contains("Name"));
    CHECK(table.contains("Description"));
    // Check for field values
    CHECK(table.contains("Test Name"));
    CHECK(table.contains("Test description"));
}
```


## Use fixed time points for determinism

For fields with `std::chrono::system_clock::time_point` type, use fixed time values instead of `system_clock::now()`. This makes tests deterministic and repeatable.

Use the `ores::utility::faker::datetime::make_timepoint` helper. This function uses C++20's timezone-agnostic `sys_days` for consistent results across different environments:

```cpp
#include "ores.utility/faker/datetime.hpp"

namespace {

const std::string_view test_suite("ores.COMPONENT.tests");
const std::string tags("[domain]");

using ores::utility::faker::datetime;

}
```

Then use it in tests:

```cpp
// Good: deterministic, repeatable, timezone-agnostic
sut.as_of_date = datetime::make_timepoint(2023, 1, 1);
sut.ingestion_timestamp = datetime::make_timepoint(2023, 1, 1, 12, 30);

// Bad: non-deterministic, value changes on each run
sut.as_of_date = std::chrono::system_clock::now();
```
