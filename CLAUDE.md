# ORE Studio Developer Guide

## Build Commands

The project is implemented in C++ 23 and uses CMake as the build system.

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

- `ores.comms`: contains the client and server code that speak a binary bespoke
  protocol, as well as infrastructure related to the protocol (frames, etc).
- `ores.risk`: contains the domain types that implement the ORE (Open Source
  Risk Engine) types, associated database support and import and export to ORE
  XML representation.
- `ores.variability`: contains feature flag domain types, repositories, and
  messaging infrastructure for feature flag management protocol.
- `ores.accounts`: contains user account management, authentication, and
  authorization functionality.
- `ores.service`: binary to run the service. Simple host that just instantiates
  the server defined in comms.
- `ores.client`: deprecated. Code will be in comms instead.
- `ores.cli`: command line interface that instantiates functionality in risk and
  other core libraries.
- `ores.qt`: Qt based GUI for the application.
- `ores.utility`: assorted utilities which do not contain domain specific code.

## Testing

- Write table-driven tests with clear input/output expectations
- Include detailed error messages (expected vs. actual)

## Dependencies

- Minimum C++ version: 23
- External dependencies managed through vcpkg

## Pull Requests

- do not add "Test plan" section to PRs.
- git commit titles should use format [COMPONENT] Description. Example [qt] change xyz