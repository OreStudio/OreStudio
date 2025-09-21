# ORE Studio Developer Guide

## Build Commands

The project is implemented in C++ 20 and uses CMake as the build system.

- `cmake --preset linux-clang-debug` - Configure project in debug and install dependencies.
- `cmake --build --preset linux-clang-debug` - Build project in debug.
- `cmake --build --preset linux-clang-release --target rat` - Run all tests

## Code Style

- Follow standard C++ formatting conventions
- Group imports: standard library first, then third-party
- Use snake_case for classes and methods.
- Add comments for public API and complex logic
- Place related functionality in logically named files

## Testing

- Write table-driven tests with clear input/output expectations
- Include detailed error messages (expected vs. actual)

## Dependencies

- Minimum C++ version: 20
- External dependencies managed through vcpkg

## Pull Requests

- do not add "Test plan" section to PRs.
