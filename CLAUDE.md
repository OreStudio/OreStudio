# ORE Studio Developer Guide

## Build Commands

The project is implemented in C++ 23 and uses CMake as the build system.

See `doc/skills/cmake-runner/skill.org` for details on how to run CMake. Key
commands:

- `cmake --preset linux-clang-debug` - Configure project in debug and install dependencies.
- `cmake --build --preset linux-clang-debug` - Build project in debug.
- `cmake --build --preset linux-clang-release --target rat` - Run all tests

## Code Style

- Follow standard C++ formatting conventions
- Group imports: standard library first, then third-party
- Use snake_case for classes and methods.
- Add comments for public API and complex logic
- Place related functionality in logically named files

## Architecture

Read `projects/modeling/system_model.org` for details on how the system
architecture.

## Testing

- Write table-driven tests with clear input/output expectations
- Include detailed error messages (expected vs. actual)
- Read `doc/skills/unit-test-writer/skill.org` for details on how to write unit
  tests.

## Dependencies

- Minimum C++ version: 23
- External dependencies managed through vcpkg

## Pull Requests

- do not add "Test plan" section to PRs.
- git commit titles should use format: [COMPONENT] Description.
  Example: [qt] Change xyz
