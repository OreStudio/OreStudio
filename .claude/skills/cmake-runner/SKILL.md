---
name: cmake-runner
description: Guide for running CMake commands in ORE Studio for building, testing, and deploying.
license: Complete terms in LICENSE.txt
---


# When to use this skill

When you need to configure, build, test, or deploy ORE Studio using CMake.


# How to use this skill

1.  Identify what operation you need to perform (configure, build, test, deploy, generate diagrams).
2.  All commands must be executed from the top-level project directory.
3.  Follow the appropriate command from the Detailed instructions section below.
4.  Default to `linux-clang-debug` preset when on Linux unless otherwise specified.
5.  If you need to inspect the output of the build, see section Output directory layout.


# Detailed instructions


## Listing and choosing presets

To see all available CMake presets:

```sh
cmake --list-presets
```

The default preset for Linux is `linux-clang-debug`. For release builds, use `linux-clang-release`.


## Configuring the project

To configure the project with a specific preset:

```sh
cmake --preset linux-clang-debug
```

For release configuration:

```sh
cmake --preset linux-clang-release
```


## Building the project

To build the entire project:

```sh
cmake --build --preset linux-clang-debug
```

For release builds:

```sh
cmake --build --preset linux-clang-release
```


## Running tests


### Running all tests

To run all tests (target name: `rat`):

```sh
cmake --build --preset linux-clang-release --target rat
```


### Running specific component tests

To run tests for a specific component, use the pattern `test_COMPONENT_NAME`. For example, to run tests for `ores.accounts.tests`:

```sh
cmake --build --target test_ores.accounts.tests --preset linux-clang-debug
```

Replace `ores.accounts.tests` with the desired component name.


### Configuring test logging

Test logging is disabled by default for performance. To enable logging when debugging test failures, use CMake options during configuration.

-   Using CMake options (recommended)

    Configure the project with logging enabled:
    
    ```sh
    # Enable logging at debug level (logs to files only)
    cmake --preset linux-clang-debug -DORES_TEST_LOG_LEVEL=debug
    
    # Enable logging at trace level with console output
    cmake --preset linux-clang-debug -DORES_TEST_LOG_LEVEL=trace -DORES_TEST_LOG_CONSOLE=ON
    ```
    
    Then run tests as normal:
    
    ```sh
    cmake --build --preset linux-clang-debug --target rat
    ```
    
    Available CMake options:
    
    -   `ORES_TEST_LOG_LEVEL`: Log level (default: `OFF`, options: `trace`, `debug`, `info`, `warn`, `error`)
    -   `ORES_TEST_LOG_CONSOLE`: Output logs to console (default: `OFF`)
    
    To disable logging again, reconfigure with:
    
    ```sh
    cmake --preset linux-clang-debug -DORES_TEST_LOG_LEVEL=OFF
    ```

-   Using environment variables

    Alternatively, set environment variables directly when running tests:
    
    ```sh
    # Enable logging to files
    ORES_TEST_LOG_ENABLED=true cmake --build --target test_ores.accounts.tests --preset linux-clang-debug
    
    # Enable logging with console output
    ORES_TEST_LOG_ENABLED=true ORES_TEST_LOG_CONSOLE=true cmake --build --target test_ores.accounts.tests --preset linux-clang-debug
    
    # Change log level (default: trace, options: trace, debug, info, warn, error)
    ORES_TEST_LOG_ENABLED=true ORES_TEST_LOG_LEVEL=debug cmake --build --target test_ores.accounts.tests --preset linux-clang-debug
    ```
    
    Available environment variables:
    
    -   `ORES_TEST_LOG_ENABLED`: Enable/disable logging (default: `false`)
    -   `ORES_TEST_LOG_LEVEL`: Log severity level (default: `trace`)
    -   `ORES_TEST_LOG_CONSOLE`: Output logs to console (default: `false`)


### Analysing test results

The `parse_test_results.py` script provides a summary of test results, including failed tests and relevant log entries. It requires test logging to be enabled.

```sh
# First, enable logging and run tests
cmake --preset linux-clang-debug -DORES_TEST_LOG_LEVEL=debug
cmake --build --preset linux-clang-debug --target rat

# Then analyse the results
./scripts/parse_test_results.py build/output/linux-clang-debug/publish/bin
```

The script:

-   Parses all `test-results*.xml` files in the specified directory
-   Shows overall statistics (total tests, passed, failed, duration)
-   For failed tests, displays the test name, location, and exception info
-   Correlates failures with log files and shows ERROR/WARN entries
-   Provides an overall summary across all test suites


## Deploying skills

To deploy Claude Code skills:

```sh
cmake --build --target deploy_skills --preset linux-clang-debug
```


## Generating PlantUML Diagrams


### Generating all diagrams

To generate all PlantUML diagrams for the project:

```sh
cmake --build --target mad --preset linux-clang-debug
```


### Generating individual component diagrams

To generate a diagram for a specific component, use the pattern `generate_COMPONENT_diagram` where `COMPONENT` is the component name (with dots).

For example, to generate the ores.refdata diagram:

```sh
cmake --build --target generate_ores.refdata_diagram --preset linux-clang-debug
```

To generate the ores.comms diagram:

```sh
cmake --build --target generate_ores.comms_diagram --preset linux-clang-debug
```

Other available diagram targets include:

-   `generate_ores.accounts_diagram`
-   `generate_ores.utility_diagram`
-   `generate_ores_diagram` (system-level architecture diagram)


## Deploying the site

To deploy the project website:

```sh
cmake --build --target deploy_site --preset linux-clang-debug
```


# Output directory layout

The build output directory lives under `build/output`. There is a top-level directory for the presets (e.g. `linux-clang-debug`, etc) and a top-level directory for the site called `site`.

Within the preset directory:

-   binaries are located under `publish/bin`.
-   logs for applications are under `publish/log`.
-   logs for tests are organised as follows:
    -   they are under `publish/log/COMPONENT_TESTS` where `COMPONENT_TESTS` is the name of the component under test, e.g. `ores.accounts.tests`.
    -   Inside, there is one folder per test suite, e.g. `domain_account_tests`, `domain_feature_flags_tests`, and so on. These must match the test suite implementation files (e.g. `projects/ores.accounts/tests/domain_account_tests.cpp`).
    -   Inside the test suite folder, there is one log file per test, e.g. `account_serialization_to_json.log`. This must match the name of the unit test, e.g. `TEST_CASE("account_serialization_to_json", tags) {` in `projects/ores.accounts/tests/domain_account_tests.cpp`.
