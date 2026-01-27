# C++ Code Generation Analysis

## Overview

This document analyzes the C++ files created manually for `dataset_bundle` and `dataset_bundle_member` to identify patterns that can be codified into templates, extending the existing SQL generation models.

## Files Generated Per Domain Entity

| # | File Type | Location | Purpose |
|---|-----------|----------|---------|
| 1 | Domain class | `include/.../domain/{entity}.hpp` | Core data structure |
| 2 | JSON I/O header | `include/.../domain/{entity}_json_io.hpp` | ostream operator declaration |
| 3 | JSON I/O impl | `src/domain/{entity}_json_io.cpp` | rfl::json::write implementation |
| 4 | Table header | `include/.../domain/{entity}_table.hpp` | convert_to_table declaration |
| 5 | Table impl | `src/domain/{entity}_table.cpp` | fort table implementation |
| 6 | Table I/O header | `include/.../domain/{entity}_table_io.hpp` | vector ostream operator |
| 7 | Table I/O impl | `src/domain/{entity}_table_io.cpp` | Uses convert_to_table |
| 8 | Generator header | `include/.../generators/{entity}_generator.hpp` | Synthetic data generators |
| 9 | Generator impl | `src/generators/{entity}_generator.cpp` | faker-based implementation |
| 10 | Entity header | `include/.../repository/{entity}_entity.hpp` | sqlgen struct |
| 11 | Entity impl | `src/repository/{entity}_entity.cpp` | ostream for entity |
| 12 | Mapper header | `include/.../repository/{entity}_mapper.hpp` | Domain <-> Entity mapping |
| 13 | Mapper impl | `src/repository/{entity}_mapper.cpp` | Mapping implementation |
| 14 | Repository header | `include/.../repository/{entity}_repository.hpp` | CRUD operations |
| 15 | Repository impl | `src/repository/{entity}_repository.cpp` | sqlgen-based CRUD |

**Total: 15 files per domain entity**

---

## Type Mappings

### SQL to C++ Domain Type Mapping

| SQL Type | C++ Domain Type | Includes |
|----------|-----------------|----------|
| `uuid` | `boost::uuids::uuid` | `<boost/uuid/uuid.hpp>` |
| `text` | `std::string` | `<string>` |
| `integer` | `int` | - |
| `timestamp` | `std::chrono::system_clock::time_point` | `<chrono>` |

### SQL to C++ Entity Type Mapping (sqlgen)

| SQL Type | C++ Entity Type | Notes |
|----------|-----------------|-------|
| `uuid` (PK) | `sqlgen::PrimaryKey<std::string>` | UUID stored as string |
| `uuid` | `std::string` | Non-PK UUIDs |
| `text` (PK) | `sqlgen::PrimaryKey<std::string>` | For junction tables |
| `text` | `std::string` | |
| `integer` | `int` | |
| `timestamp` | `sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">` | With format string |

---

## Standard Columns

All bi-temporal entities share these audit columns:

### Domain Class Columns
```cpp
int version = 0;                                    // Optimistic locking
std::string recorded_by;                            // Who made the change
std::string change_reason_code;                     // Why (references change_reasons)
std::string change_commentary;                      // Free-text explanation
std::chrono::system_clock::time_point recorded_at;  // When
```

### Entity Class Columns (DB mapping)
```cpp
int version = 0;
std::string modified_by;                            // Maps to recorded_by
std::string change_reason_code;
std::string change_commentary;
sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_from;  // Maps to recorded_at
sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_to;
```

---

## Key Differences: Domain Entity vs Junction Table

| Aspect | Domain Entity | Junction Table |
|--------|---------------|----------------|
| Primary Key | UUID (`id`) | Composite text keys |
| Natural Keys | Separate (code, name) | Part of composite PK |
| Entity PK Type | `PrimaryKey<string>` for id | `PrimaryKey<string>` for left key |
| Domain includes | `<boost/uuid/uuid.hpp>` | None (no UUID) |
| Mapper | UUID conversion needed | Simple string copy |

---

## Extended Model Schema Proposal

### For Domain Entity (`*_domain_entity.json`)

```json
{
  "domain_entity": {
    "component": "dq",
    "entity_singular": "dataset_bundle",
    "entity_plural": "dataset_bundles",
    "entity_title": "Dataset Bundle",
    "brief": "A named collection of datasets designed to work together.",
    "description": "Multi-line description...",

    "primary_key": {
      "column": "id",
      "type": "uuid",
      "description": "UUID uniquely identifying this bundle."
    },

    "natural_keys": [
      {
        "column": "code",
        "type": "text",
        "description": "Unique code for stable referencing.",
        "examples": ["slovaris", "base", "crypto"]
      },
      {
        "column": "name",
        "type": "text",
        "description": "Human-readable name for the bundle."
      }
    ],

    "columns": [
      {
        "name": "description",
        "type": "text",
        "nullable": false,
        "description": "Detailed description of the bundle's contents and purpose."
      }
    ],

    "table_display": ["code", "name", "description", "recorded_by", "version"],

    "generator": {
      "code": "faker::word::noun() + \"_bundle\"",
      "name": "faker::word::adjective() + \" \" + faker::word::noun() + \" Bundle\"",
      "description": "faker::lorem::sentence()"
    }
  }
}
```

### For Junction Table (`*_junction.json`)

```json
{
  "junction": {
    "component": "dq",
    "name": "dataset_bundle_members",
    "name_singular": "dataset_bundle_member",
    "name_title": "Dataset Bundle Member",
    "brief": "Links a dataset to a bundle.",
    "description": "Multi-line description...",

    "left": {
      "column": "bundle_code",
      "type": "text",
      "description": "Code of the bundle this membership belongs to.",
      "references": "dataset_bundle.code"
    },

    "right": {
      "column": "dataset_code",
      "type": "text",
      "description": "Code of the dataset that is a member of the bundle.",
      "references": "dataset.code"
    },

    "columns": [
      {
        "name": "display_order",
        "type": "integer",
        "nullable": false,
        "default": 0,
        "description": "Order in which this dataset should be displayed or processed."
      }
    ],

    "table_display": ["bundle_code", "dataset_code", "display_order", "recorded_by", "version"],

    "generator": {
      "bundle_code": "faker::word::noun() + \"_bundle\"",
      "dataset_code": "faker::word::noun() + \".\" + faker::word::noun()",
      "display_order": "faker::number::integer(1, 100)"
    }
  }
}
```

---

## Template Categories

### 1. Simple Templates (mostly boilerplate)

These require minimal model data - just names and component:

- `{entity}_json_io.hpp` - Just needs entity name, component
- `{entity}_json_io.cpp` - Just needs entity name, component
- `{entity}_table_io.hpp` - Just needs entity name, component
- `{entity}_table_io.cpp` - Just needs entity name, component
- `{entity}_entity.cpp` - Just needs entity name, component

### 2. Medium Complexity Templates

Need field information for iterations:

- `{entity}.hpp` - Domain class with all fields + doxygen
- `{entity}_table.hpp` - Declaration with entity name
- `{entity}_table.cpp` - Table columns from model
- `{entity}_entity.hpp` - Entity struct with sqlgen types
- `{entity}_mapper.hpp` - Declaration with entity name

### 3. Complex Templates

Need detailed field mappings and logic:

- `{entity}_generator.hpp` - Function declarations
- `{entity}_generator.cpp` - faker expressions per field
- `{entity}_mapper.cpp` - Field-by-field mapping with type conversions
- `{entity}_repository.hpp` - CRUD operations, query methods
- `{entity}_repository.cpp` - sqlgen queries, custom lookups

---

## Template Naming Convention

Templates are named to align with the facets in `doc/skills/domain-type-creator/`:

| Facet | Template Name | Output File |
|-------|---------------|-------------|
| **Domain Type Class Definition** | | |
| - Domain entity variant | `cpp_domain_entity_class.hpp.mustache` | `{entity}.hpp` |
| - Junction variant | `cpp_junction_class.hpp.mustache` | `{entity}.hpp` |
| **Domain Type JSON I/O** | | |
| - Header | `cpp_json_io.hpp.mustache` | `{entity}_json_io.hpp` |
| - Implementation | `cpp_json_io.cpp.mustache` | `{entity}_json_io.cpp` |
| **Domain Type Table I/O** | | |
| - Table function header | `cpp_table.hpp.mustache` | `{entity}_table.hpp` |
| - Table function impl | `cpp_table.cpp.mustache` | `{entity}_table.cpp` |
| - Table ostream header | `cpp_table_io.hpp.mustache` | `{entity}_table_io.hpp` |
| - Table ostream impl | `cpp_table_io.cpp.mustache` | `{entity}_table_io.cpp` |
| **Domain Type Generator** | | |
| - Header | `cpp_generator.hpp.mustache` | `{entity}_generator.hpp` |
| - Implementation | `cpp_generator.cpp.mustache` | `{entity}_generator.cpp` |
| **Repository Entity & Mapper** | | |
| - Entity header (domain) | `cpp_repository_entity.hpp.mustache` | `{entity}_entity.hpp` |
| - Entity header (junction) | `cpp_repository_junction_entity.hpp.mustache` | `{entity}_entity.hpp` |
| - Entity impl | `cpp_repository_entity.cpp.mustache` | `{entity}_entity.cpp` |
| - Mapper header | `cpp_repository_mapper.hpp.mustache` | `{entity}_mapper.hpp` |
| - Mapper impl (domain) | `cpp_repository_entity_mapper.cpp.mustache` | `{entity}_mapper.cpp` |
| - Mapper impl (junction) | `cpp_repository_junction_mapper.cpp.mustache` | `{entity}_mapper.cpp` |
| **Repository CRUD Operations** | | |
| - Header (domain) | `cpp_repository.hpp.mustache` | `{entity}_repository.hpp` |
| - Header (junction) | `cpp_junction_repository.hpp.mustache` | `{entity}_repository.hpp` |
| - Implementation (domain) | `cpp_repository.cpp.mustache` | `{entity}_repository.cpp` |
| - Implementation (junction) | `cpp_junction_repository.cpp.mustache` | `{entity}_repository.cpp` |

**Naming Pattern:**
- `cpp_` prefix for C++ templates (vs `sql_` for SQL)
- Facet name in middle (e.g., `json_io`, `table`, `generator`, `repository`)
- Variant suffix for model-specific templates (`_entity`, `_junction`)
- File extension at end (`.hpp.mustache`, `.cpp.mustache`)

---

## Implementation Phases

### Phase 1: Extend Models
- Add `brief` for short @brief description
- Add `entity_title`/`name_title` for comment headers
- Add `description` field for each column (for doxygen)
- Add `table_display` array for table column selection
- Add `generator` config for faker expressions per field

### Phase 2: Shared Templates (6 templates)
Templates that work for both domain entities and junctions:
- `cpp_json_io.hpp.mustache`
- `cpp_json_io.cpp.mustache`
- `cpp_table.hpp.mustache`
- `cpp_table_io.hpp.mustache`
- `cpp_table_io.cpp.mustache`
- `cpp_repository_entity.cpp.mustache`

### Phase 3: Domain Class Templates (2 templates)
- `cpp_domain_entity_class.hpp.mustache`
- `cpp_junction_class.hpp.mustache`

### Phase 4: Table Implementation (1 template)
- `cpp_table.cpp.mustache` (uses table_display config)

### Phase 5: Repository Entity Headers (2 templates)
- `cpp_repository_entity.hpp.mustache` (domain entity)
- `cpp_repository_junction_entity.hpp.mustache` (junction)

### Phase 6: Generator Templates (2 templates)
- `cpp_generator.hpp.mustache`
- `cpp_generator.cpp.mustache`

### Phase 7: Mapper Templates (3 templates)
- `cpp_repository_mapper.hpp.mustache` (shared header)
- `cpp_repository_entity_mapper.cpp.mustache` (domain entity impl)
- `cpp_repository_junction_mapper.cpp.mustache` (junction impl)

### Phase 8: Repository Templates (4 templates)
- `cpp_repository.hpp.mustache` (domain entity)
- `cpp_junction_repository.hpp.mustache` (junction)
- `cpp_repository.cpp.mustache` (domain entity)
- `cpp_junction_repository.cpp.mustache` (junction)

---

## Validation Strategy

For each template, generate output and diff against the manually-created files:
- `dataset_bundle` files (domain entity pattern)
- `dataset_bundle_member` files (junction pattern)

Acceptable differences:
- Copyright year (template uses current year)
- Minor whitespace differences
