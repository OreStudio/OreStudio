#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""
Script to parse test result XML files and extract information about failed tests,
along with relevant logs from corresponding log directories.
"""

import os
import sys
import glob
import xml.etree.ElementTree as ET
import re
from datetime import datetime


def find_xml_files(base_dir):
    """Find all test-results*.xml files in the specified directory."""
    pattern = os.path.join(base_dir, "test-results*.xml")
    return glob.glob(pattern)


def parse_test_run_stats(root, xml_file_path):
    """Extract overall test run statistics from the XML root."""
    stats = {}

    # Get attributes from the root element
    if root.tag == 'Catch2TestRun':
        stats['name'] = root.get('name', 'Unknown')
        stats['rng_seed'] = root.get('rng-seed')
        stats['catch2_version'] = root.get('catch2-version')
        stats['start_time'] = root.get('start-time')  # if available in XML

    # Get modification time of the file
    try:
        mod_time = os.path.getmtime(xml_file_path)
        stats['modification_time'] = datetime.fromtimestamp(mod_time).strftime('%Y-%m-%d %H:%M:%S')
    except OSError:
        stats['modification_time'] = 'Unknown'

    # Count total tests, passed, failed
    all_test_cases = root.findall(".//TestCase")
    stats['total_tests'] = len(all_test_cases)

    passed_tests = 0
    failed_tests = 0
    skipped_tests = 0

    for test_case in all_test_cases:
        overall_result = test_case.find("./OverallResult")
        if overall_result is not None:
            if overall_result.get('success') == 'true':
                passed_tests += 1
            elif overall_result.get('success') == 'false':
                failed_tests += 1
            # Check for skips
            skips = overall_result.get('skips', '0')
            if int(skips) > 0:
                skipped_tests += int(skips)

    stats['passed'] = passed_tests
    stats['failed'] = failed_tests
    stats['skipped'] = skipped_tests

    # Calculate duration if available
    total_duration = 0
    for test_case in all_test_cases:
        overall_result = test_case.find("./OverallResult")
        if overall_result is not None:
            duration = overall_result.get('durationInSeconds')
            if duration:
                total_duration += float(duration)

    stats['total_duration'] = total_duration

    return stats


def parse_failed_tests(xml_file):
    """Parse XML file and return a list of failed test cases."""
    tree = ET.parse(xml_file)
    root = tree.getroot()

    failed_tests = []

    # Look for TestCase elements with OverallResult success="false"
    for test_case in root.findall(".//TestCase"):
        overall_result = test_case.find("./OverallResult")
        if overall_result is not None and overall_result.get('success') == 'false':
            failed_test = {
                'name': test_case.get('name'),
                'tags': test_case.get('tags'),
                'filename': test_case.get('filename'),
                'line': test_case.get('line'),
                'duration': overall_result.get('durationInSeconds'),
                'exception': test_case.find('./Exception'),
                'skips': overall_result.get('skips', '0')
            }
            failed_tests.append(failed_test)

    return failed_tests


def extract_exception_info(exception_elem):
    """Extract information from exception element."""
    if exception_elem is not None:
        # Get the text content and attributes
        exc_text = exception_elem.text
        exc_filename = exception_elem.get('filename')
        exc_line = exception_elem.get('line')

        info = ""
        if exc_filename and exc_line:
            info += f"[{exc_filename}:{exc_line}] "
        if exc_text:
            info += exc_text.strip()

        return info
    return ""


def find_log_directory(base_output_dir):
    """Find the log directory corresponding to the output directory."""
    log_dir = os.path.join(base_output_dir, "log")
    return log_dir


def find_test_suite_log_by_project_name(log_dir, project_name):
    """Find the test suite log file based on the project name."""
    # Construct the expected log file path
    test_suite_log = os.path.join(log_dir, f"{project_name}.tests", f"{project_name}.tests.log")
    if os.path.exists(test_suite_log):
        return test_suite_log

    # If the expected path doesn't exist, try to find any matching log file
    for root, dirs, files in os.walk(log_dir):
        for file in files:
            if file.endswith('.log') and project_name in file:
                return os.path.join(root, file)

    return None


def find_test_suite_log(log_dir, test_filename):
    """Find the test suite log file based on the test file path."""
    # Extract project name from the test filename
    # Example: "/path/to/projects/ores.cli/tests/app_application_import_tests.cpp"
    # We want to get "ores.cli" from this path
    match = re.search(r'/projects/([^/]+)/tests/', test_filename)
    if match:
        project_name = match.group(1)
        # Use the new function
        return find_test_suite_log_by_project_name(log_dir, project_name)

    # If we can't determine the project name, try to find any matching log file
    for root, dirs, files in os.walk(log_dir):
        for file in files:
            if file.endswith('.log') and 'tests' in file:
                return os.path.join(root, file)

    return None


def find_test_case_log(log_dir, project_name, test_name):
    """Find the specific test case log file."""
    # Look for the test case log file in the log directory
    # Path would be like: log/{project_name}.tests/{category}/{test_name}.log
    for root, dirs, files in os.walk(log_dir):
        for file in files:
            if file == f"{test_name}.log":
                return os.path.join(root, file)

    # Alternative: look for logs in subdirectories named after categories
    # Try to find a log file with the test name in the project's log directory
    project_log_dir = os.path.join(log_dir, f"{project_name}.tests")
    if os.path.exists(project_log_dir):
        for category_dir in os.listdir(project_log_dir):
            category_path = os.path.join(project_log_dir, category_dir)
            if os.path.isdir(category_path):
                test_log_path = os.path.join(category_path, f"{test_name}.log")
                if os.path.exists(test_log_path):
                    return test_log_path

    return None


def extract_project_name_from_path(test_filename):
    """Extract project name from the test file path."""
    match = re.search(r'/projects/([^/]+)/tests/', test_filename)
    if match:
        return match.group(1)
    return "unknown"


def grep_log_file(log_file_path, patterns=['ERROR', 'WARN']):
    """Grep a log file for specified patterns and return matching lines."""
    if not os.path.exists(log_file_path):
        return [f"Log file does not exist: {log_file_path}"]

    matches = []
    try:
        with open(log_file_path, 'r', encoding='utf-8', errors='ignore') as f:
            for line_num, line in enumerate(f, 1):
                for pattern in patterns:
                    if pattern in line:
                        matches.append(f"Line {line_num}: {line.strip()}")
                        break  # Don't duplicate the line if multiple patterns match
    except Exception as e:
        matches.append(f"Error reading log file {log_file_path}: {str(e)}")

    return matches


def validate_and_parse_xml_file(xml_file):
    """Try to parse an XML file and return (tree, root, success, error_message)."""
    try:
        tree = ET.parse(xml_file)
        root = tree.getroot()
        return tree, root, True, None
    except ET.ParseError as e:
        return None, None, False, f"XML Parse Error: {str(e)}"
    except Exception as e:
        return None, None, False, f"Unexpected Error: {str(e)}"


def main():
    if len(sys.argv) != 2:
        print("Usage: python parse_test_results.py <path_to_directory>")
        print("Example: python parse_test_results.py build/output/linux-clang-debug/publish/bin")
        sys.exit(1)

    bin_dir = sys.argv[1]

    if not os.path.exists(bin_dir):
        print(f"Directory does not exist: {bin_dir}")
        sys.exit(1)

    # Find XML files
    xml_files = find_xml_files(bin_dir)

    if not xml_files:
        print(f"No test-results*.xml files found in {bin_dir}")
        sys.exit(0)

    print(f"Found {len(xml_files)} test-results files:")
    for xml_file in xml_files:
        print(f"  - {xml_file}")

    # Calculate the base output directory for logs
    # The logs seem to be in build/output/log regardless of the specific build type
    # So we'll look for a "log" directory at the same level as the bin directory's parent
    potential_log_dirs = [
        os.path.join(os.path.dirname(os.path.dirname(bin_dir)), "log"),  # build/output/log
        os.path.join(os.path.dirname(bin_dir), "log"),  # build/output/linux-clang-debug/log
        os.path.join(bin_dir, "log")  # build/output/linux-clang-debug/publish/bin/log
    ]

    log_dir = None
    for potential_dir in potential_log_dirs:
        if os.path.exists(potential_dir):
            log_dir = potential_dir
            break

    if log_dir is None:
        # If no log directory found, try to find it by walking up the tree
        current_dir = os.path.dirname(os.path.dirname(bin_dir))
        while current_dir != "/" and current_dir != "":
            potential_log = os.path.join(current_dir, "log")
            if os.path.exists(potential_log):
                log_dir = potential_log
                break
            current_dir = os.path.dirname(current_dir)

    if log_dir is None:
        log_dir = os.path.join(os.path.dirname(os.path.dirname(bin_dir)), "log")  # fallback

    print(f"\nLooking for logs in: {log_dir}")

    total_failures = 0
    all_stats = []
    valid_xml_count = 0

    for xml_file in xml_files:
        print(f"\n{'='*80}")
        print(f"Processing: {os.path.basename(xml_file)}")
        print(f"{'='*80}")

        # Try to parse the XML file
        tree, root, success, error_msg = validate_and_parse_xml_file(xml_file)

        # Extract project name from the XML filename to find the corresponding log
        # Get the project name from the filename pattern: test-results-{project_name}.tests.xml
        filename = os.path.basename(xml_file)
        if filename.startswith("test-results-") and ".tests.xml" in filename:
            project_name = filename.replace("test-results-", "").replace(".tests.xml", "")
        else:
            # Fallback: try to extract from path
            project_name = extract_project_name_from_path(xml_file)

        # Always check the test suite log regardless of XML parsing success
        test_suite_log = find_test_suite_log_by_project_name(log_dir, project_name)
        if test_suite_log:
            print(f"\n  Test Suite Log: {test_suite_log}")
            if not success:
                # If XML parsing failed, dump the entire log file content
                print("  XML parsing failed - dumping entire log file content:")
                print("  " + "="*77)
                try:
                    with open(test_suite_log, 'r', encoding='utf-8', errors='ignore') as f:
                        log_content = f.read()
                        # Print the entire log content with proper indentation
                        for line in log_content.split('\n'):
                            print(f"    {line}")
                except Exception as e:
                    print(f"    Error reading log file: {str(e)}")
                print("  " + "="*77)
            else:
                # XML parsing succeeded, proceed normally and check for WARN/ERROR
                suite_log_matches = grep_log_file(test_suite_log)
                if suite_log_matches:
                    print("  Errors/Warnings in test suite log:")
                    for match in suite_log_matches[:20]:  # Show first 20 matches for failed XML files
                        print(f"    {match}")
                    if len(suite_log_matches) > 20:
                        print(f"    ... and {len(suite_log_matches) - 20} more")
                else:
                    print("  No errors/warnings found in test suite log")

        if not success:
            print(f"Skipping XML processing due to error: {error_msg}")
            continue

        valid_xml_count += 1

        # Get test run statistics
        stats = parse_test_run_stats(root, xml_file)
        all_stats.append(stats)

        print(f"Test Suite: {stats['name']}")
        print(f"Catch2 Version: {stats.get('catch2_version', 'N/A')}")
        print(f"RNG Seed: {stats.get('rng_seed', 'N/A')}")
        print(f"Modification Time: {stats['modification_time']}")
        print(f"Total Tests: {stats['total_tests']}")
        print(f"Passed: {stats['passed']}")
        print(f"Failed: {stats['failed']}")
        print(f"Skipped: {stats['skipped']}")
        print(f"Total Duration: {stats['total_duration']:.3f}s")

        failed_tests = parse_failed_tests(xml_file)

        if not failed_tests:
            print("\nNo failed tests found in this file.")
        else:
            print(f"\nFound {len(failed_tests)} failed test(s):\n")

            for i, test in enumerate(failed_tests, 1):
                total_failures += 1
                print(f"FAILURE #{total_failures} ({test['name']}):")
                print(f"  Name: {test['name']}")
                print(f"  Tags: {test['tags']}")
                print(f"  File: {test['filename']}")
                print(f"  Line: {test['line']}")
                print(f"  Duration: {test['duration']}s" if test['duration'] else "  Duration: N/A")

                exception_info = extract_exception_info(test['exception'])
                if exception_info:
                    print(f"  Exception: {exception_info[:500]}...")  # Truncate long exceptions

                # Find and parse logs for failed test specifically
                project_name_specific = extract_project_name_from_path(test['filename'])
                if project_name_specific != "unknown":
                    # Find test case log
                    test_case_log = find_test_case_log(log_dir, project_name_specific, test['name'])
                    if test_case_log:
                        print(f"\n  Test Case Log: {test_case_log}")
                        case_log_matches = grep_log_file(test_case_log)
                        if case_log_matches:
                            print("  Errors/Warnings in test case log:")
                            for match in case_log_matches[:10]:  # Show first 10 matches
                                print(f"    {match}")
                            if len(case_log_matches) > 10:
                                print(f"    ... and {len(case_log_matches) - 10} more")
                        else:
                            print("  No errors/warnings found in test case log")

                print()  # Empty line for readability

    # Print overall summary
    print(f"\n{'='*80}")
    print("OVERALL SUMMARY")
    print(f"{'='*80}")

    total_tests = sum(stat['total_tests'] for stat in all_stats)
    total_passed = sum(stat['passed'] for stat in all_stats)
    total_failed = sum(stat['failed'] for stat in all_stats)
    total_skipped = sum(stat['skipped'] for stat in all_stats)
    total_duration = sum(stat['total_duration'] for stat in all_stats)

    print(f"Total Files Processed: {len(xml_files)}")
    print(f"Valid XML Files: {valid_xml_count}")
    print(f"Invalid/Skipped Files: {len(xml_files) - valid_xml_count}")
    print(f"Total Test Suites: {len(all_stats)}")
    print(f"Total Tests: {total_tests}")
    print(f"Total Passed: {total_passed}")
    print(f"Total Failed: {total_failed}")
    print(f"Total Skipped: {total_skipped}")
    print(f"Total Duration: {total_duration:.3f}s")

    if total_tests > 0:
        pass_rate = (total_passed / total_tests) * 100
        fail_rate = (total_failed / total_tests) * 100
        print(f"Pass Rate: {pass_rate:.2f}%")
        print(f"Fail Rate: {fail_rate:.2f}%")

    print(f"Failed Test Cases: {total_failures}")
    print(f"{'='*80}")


if __name__ == "__main__":
    main()
