# ORE Studio Developer Guide

## Conventions

Use UNIX line terminations for all new files.

## Build Commands

The project is implemented in C++ 23 and uses CMake as the build system.

See `doc/skills/cmake-runner/skill.org` for details on how to run CMake. Key
commands:

- `cmake --preset linux-clang-debug-ninja` - Configure project in debug and install dependencies.
- `cmake --build --preset linux-clang-debug-ninja` - Build project in debug.
- `cmake --build --preset linux-clang-release-ninja --target rat` - Run all tests

## Code Style

- Follow standard C++ formatting conventions
- Group imports: standard library first, then third-party
- Use snake_case for classes and methods.
- Add comments for public API and complex logic
- Place related functionality in logically named files

## Architecture

Read `projects/modeling/system_model.org` for details on the system
architecture.

## Documentation Validation

Run before pushing changes that add or modify component documentation:

```sh
./projects/ores.codegen/validate_docs.sh
```

Two checks run in sequence:

1. Component structure: every `projects/ores.*/` has `modeling/component_overview.org`,
   required v2 frontmatter (`:ID:`, `#+type: component`, `#+description:`),
   required sections, and at least one `.puml` diagram. Exceptions are in
   `projects/ores.codegen/docs_exceptions.txt`.
2. Version tagging: every `.org` file in the repo carries `#+version: 1` or
   `#+version: 2` (see "Documentation versions" below).

## Documentation versions

Every `.org` file in the repo carries an org-mode marker:

- `#+version: 1` — legacy v1 document, still in its original form.
- `#+version: 2` — current cybernetic v2 document (also has `#+type:`).

**Rule: touch-it-to-port-it.** When you edit a v1 doc, full-rewrite it to v2
(satisfying the v2 frontmatter contract in `doc/meta/document_types.org`).
Small fixes still trigger the rewrite — that's the intentional pressure to
clear the v1 backlog.

Tooling:

```sh
# Summary: tagged counts and % migrated
python3 projects/ores.codegen/scripts/doc_version_audit.py

# Remaining v1 backlog (one path per line)
python3 projects/ores.codegen/scripts/doc_version_audit.py --list-v1

# CI gate — exits non-zero if any .org file is missing #+version:
python3 projects/ores.codegen/scripts/doc_version_audit.py --check

# Stamp any newly-untagged files (idempotent)
python3 projects/ores.codegen/scripts/doc_version_audit.py --stamp
```

## Database Isolation

Each domain service user (`ores_<env>_<service>_service`) holds DML only on
its own `ores_<service>_*` tables. This is the strict service table isolation
invariant. Full rules and patterns are in
`projects/ores.sql/modeling/ores.sql.org` under "Service Table Isolation".

Key points for daily work:

- **Do not add cross-component SELECT/DML grants** to the service registry
  (`projects/ores.codegen/models/services/ores_services_service_registry.json`)
  without a documented justification.
- **Trigger functions that SELECT from another service's tables** must be
  `SECURITY DEFINER SET search_path = public, pg_temp`. Never add a
  cross-component SELECT grant to satisfy a trigger check.

## Testing

- Write table-driven tests with clear input/output expectations
- Include detailed error messages (expected vs. actual)
- Read `doc/skills/unit-test-writer/skill.org` for details on how to write unit
  tests.

## Dependencies

- Minimum C++ version: 23
- External dependencies managed through vcpkg

## Environment Setup (Ubuntu / Debian / WSL)

On a fresh install — including WSL — run once to get all required packages:

```sh
./build/scripts/install_debian_packages.sh --full-install
```

Installs from distro repos: GCC, Clang, CMake, Ninja, PostgreSQL, Qt6, Valgrind,
and X11/GL headers. Then set up the database with
`./projects/ores.sql/recreate_database.sh` before the first build.

## Pull Requests

- do not add "Test plan" section to PRs.
- git commit titles should use format: [COMPONENT] Description.
  Example: [qt] Change xyz
