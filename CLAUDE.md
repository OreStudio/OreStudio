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

## Database Isolation

Each domain service user (`ores_<env>_<service>_service`) holds DML only on
its own `ores_<service>_*` tables. This is the strict service table isolation
invariant. Key rules:

- **Do not add cross-component SELECT/DML grants** to the service registry
  (`projects/ores.codegen/models/services/ores_services_service_registry.json`)
  without a documented justification. Any such addition is a tracked violation.
- **FK constraints do not require SELECT grants** on the referenced table at
  runtime. PostgreSQL enforces FKs internally; the `REFERENCES` privilege is
  only needed when creating the constraint (done by the DDL user).
- **Cross-component trigger validation functions** (`ores_iam_validate_tenant_fn`,
  `ores_dq_validate_change_reason_fn`) are `SECURITY DEFINER` — they run as
  the DDL owner and require no cross-component grants on calling service users.
- **New trigger validation functions** that SELECT from another service's tables
  must be `SECURITY DEFINER` with `SET search_path = public, pg_temp`.

The remaining cross-component grants (IAM↔variability, IAM/workflow↔refdata,
ORE↔workflow, trading↔refdata) are tracked in
`doc/plans/2026-05-12-strict-service-table-isolation.org` with phase targets
for removal.

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
