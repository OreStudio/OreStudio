---
name: test-failure-investigator
description: Investigate unit test failures by parsing results and analysing logs.
license: Complete terms in LICENSE.txt
---


# When to use this skill

When one or more unit tests are failing and you need to investigate the root cause. This skill provides a systematic workflow for diagnosing test failures using test result XML files and log output.


# How to use this skill

1.  Ask the user which test suite(s) they want to investigate.
2.  Follow the Detailed instructions section to enable logging, run tests, and analyse results.


# Detailed instructions


## Gathering information

Before investigating, ask the user the following:

-   Which test suite(s) should be investigated? Examples:
    -   `ores.accounts.tests` - tests for the accounts component
    -   `ores.refdata.tests` - tests for the reference data component
    -   `ores.cli.tests` - tests for the CLI component
    -   `rat` - run all tests
-   Are they investigating a specific test failure, or all failures in a suite?
-   If investigating a specific test, what is the test name?


## Enabling test logging

Test logging is disabled by default. Before investigating failures, ensure logging is enabled by following the instructions in [CMake Runner Skill](../cmake-runner/SKILL.md) under "Configuring test logging".

Quick reference for enabling logging:

```sh
# Enable logging at debug level
cmake --preset linux-clang-debug -DORES_TEST_LOG_LEVEL=debug

# Or trace level for more detail
cmake --preset linux-clang-debug -DORES_TEST_LOG_LEVEL=trace -DORES_TEST_LOG_CONSOLE=ON
```


## Running the failing tests

Run only the specific test suite(s) under investigation to minimise noise:

```sh
# Run a specific test suite
cmake --build --target test_COMPONENT.tests --preset linux-clang-debug

# Example: run accounts tests
cmake --build --target test_ores.accounts.tests --preset linux-clang-debug

# Or run all tests
cmake --build --preset linux-clang-debug --target rat
```

Replace `COMPONENT` with the actual component name (e.g. `ores.accounts`, `ores.refdata`).


## Parsing test results

After running tests, use the `parse_test_results.py` script to get a summary of failures:

```sh
./scripts/parse_test_results.py build/output/linux-clang-debug/publish/bin
```


### Understanding the output

The script produces output in three sections:

-   1. File summary

    ```
    Found 3 test-results files:
      - build/output/linux-clang-debug/publish/bin/test-results-ores.accounts.tests.xml
      - build/output/linux-clang-debug/publish/bin/test-results-ores.cli.tests.xml
      - build/output/linux-clang-debug/publish/bin/test-results-ores.refdata.tests.xml
    
    Looking for logs in: build/output/linux-clang-debug/publish/log
    ```

-   2. Per-suite results

    For each test suite, you will see:
    
    ```
    ================================================================================
    Processing: test-results-ores.accounts.tests.xml
    ================================================================================
    Test Suite: ores.accounts.tests
    Catch2 Version: 3.8.0
    RNG Seed: 123456789
    Modification Time: 2025-01-26 10:30:45
    Total Tests: 42
    Passed: 40
    Failed: 2
    Skipped: 0
    Total Duration: 1.234s
    
      Test Suite Log: build/output/linux-clang-debug/publish/log/ores.accounts.tests/ores.accounts.tests.log
      Errors/Warnings in test suite log:
        Line 156: [2025-01-26 10:30:44.123] [ERROR] Database connection failed
        Line 234: [2025-01-26 10:30:45.456] [WARN] Retry attempt 3 of 5
    
    Found 2 failed test(s):
    
    FAILURE #1 (account_creation_with_invalid_currency):
      Name: account_creation_with_invalid_currency
      Tags: [domain][accounts]
      File: /path/to/projects/ores.accounts/tests/domain_account_tests.cpp
      Line: 156
      Duration: 0.023s
      Exception: [/path/to/file.cpp:42] Expected: valid currency code, Got: "XXX"
    
      Test Case Log: build/output/linux-clang-debug/publish/log/ores.accounts.tests/domain_account_tests/account_creation_with_invalid_currency.log
      Errors/Warnings in test case log:
        Line 12: [ERROR] Currency lookup failed for code: XXX
        Line 15: [WARN] Falling back to default currency handling
    ```
    
    Key information to extract:
    
    -   **Exception**: The assertion or error that caused the failure
    -   **File/Line**: Where the test is defined (navigate here to see the test code)
    -   **Test Case Log**: Individual log file for this specific test
    -   **Errors/Warnings**: Log entries that may indicate the root cause

-   3. Overall summary

    ```
    ================================================================================
    OVERALL SUMMARY
    ================================================================================
    Total Files Processed: 3
    Valid XML Files: 3
    Invalid/Skipped Files: 0
    Total Test Suites: 3
    Total Tests: 150
    Total Passed: 148
    Total Failed: 2
    Total Skipped: 0
    Total Duration: 5.678s
    Pass Rate: 98.67%
    Fail Rate: 1.33%
    Failed Test Cases: 2
    ================================================================================
    ```


### Common scenarios

-   No logs found

    If the script reports "No test logs found!", logging is disabled. Enable it as described in "Enabling test logging" above, then re-run the tests.

-   XML parse errors

    If XML files cannot be parsed, the test executable may have crashed. The script will dump the entire test suite log to help diagnose the crash.


## Reading individual test logs

For deeper investigation, read the specific test case log file directly. Log files are organised as follows (see [CMake Runner Skill](../cmake-runner/SKILL.md) section "Output directory layout" for details):

```
build/output/linux-clang-debug/publish/log/
  COMPONENT.tests/                    <- Test suite directory
    COMPONENT.tests.log               <- Suite-level log
    TEST_CATEGORY/                    <- Test category from filename
      TEST_NAME.log                   <- Individual test log
```

Example path for a specific test:

```
build/output/linux-clang-debug/publish/log/ores.accounts.tests/domain_account_tests/account_serialization_to_json.log
```


## Investigation workflow

Follow this systematic approach:

1.  **Parse results**: Run `parse_test_results.py` to identify failing tests
2.  **Review exceptions**: Check the exception messages for immediate clues
3.  **Read test code**: Navigate to the test <line> to understand what the test expects
4.  **Check test log**: Read the individual test case log for ERROR/WARN entries
5.  **Check suite log**: If the test log is not helpful, check the suite-level log for broader context
6.  **Increase log level**: If more detail is needed, reconfigure with `ORES_TEST_LOG_LEVEL=trace` and re-run
7.  **Add targeted logging**: If still unclear, add temporary debug logging to the code under test


## Cleanup

After investigation, disable logging to improve test performance:

```sh
cmake --preset linux-clang-debug -DORES_TEST_LOG_LEVEL=OFF
```
