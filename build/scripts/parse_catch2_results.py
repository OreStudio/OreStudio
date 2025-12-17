#!/usr/bin/env python3
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
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
"""Parse Catch2 XML test results and display summary with failure details."""

import argparse
import glob
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class TestFailure:
    """Represents a single test failure with assertion details."""
    name: str
    filename: str
    line: int
    tags: str
    duration: float
    expressions: list = field(default_factory=list)


@dataclass
class Expression:
    """Represents a failed assertion expression."""
    type: str
    filename: str
    line: int
    original: str
    expanded: str


@dataclass
class TestSummary:
    """Summary statistics for test results."""
    total: int = 0
    passed: int = 0
    failed: int = 0
    skipped: int = 0
    duration: float = 0.0
    failures: list = field(default_factory=list)


def parse_test_file(filepath: Path) -> TestSummary:
    """Parse a single Catch2 XML result file."""
    summary = TestSummary()

    try:
        tree = ET.parse(filepath)
        root = tree.getroot()
    except ET.ParseError as e:
        print(f"Error parsing {filepath}: {e}", file=sys.stderr)
        return summary

    for test_case in root.findall('TestCase'):
        summary.total += 1
        name = test_case.get('name', 'Unknown')
        filename = test_case.get('filename', 'Unknown')
        line = int(test_case.get('line', 0))
        tags = test_case.get('tags', '')

        overall_result = test_case.find('OverallResult')
        if overall_result is not None:
            success = overall_result.get('success', 'true') == 'true'
            skips = int(overall_result.get('skips', 0))
            duration = float(overall_result.get('durationInSeconds', 0))
            summary.duration += duration

            if skips > 0:
                summary.skipped += 1
            elif success:
                summary.passed += 1
            else:
                summary.failed += 1
                failure = TestFailure(
                    name=name,
                    filename=filename,
                    line=line,
                    tags=tags,
                    duration=duration
                )

                for expr in test_case.findall('Expression'):
                    if expr.get('success', 'true') == 'false':
                        original_elem = expr.find('Original')
                        expanded_elem = expr.find('Expanded')
                        failure.expressions.append(Expression(
                            type=expr.get('type', 'CHECK'),
                            filename=expr.get('filename', filename),
                            line=int(expr.get('line', 0)),
                            original=original_elem.text.strip() if original_elem is not None and original_elem.text else '',
                            expanded=expanded_elem.text.strip() if expanded_elem is not None and expanded_elem.text else ''
                        ))

                summary.failures.append(failure)

    return summary


def merge_summaries(summaries: list) -> TestSummary:
    """Merge multiple test summaries into one."""
    merged = TestSummary()
    for s in summaries:
        merged.total += s.total
        merged.passed += s.passed
        merged.failed += s.failed
        merged.skipped += s.skipped
        merged.duration += s.duration
        merged.failures.extend(s.failures)
    return merged


def print_summary(summary: TestSummary, verbose: bool = False) -> None:
    """Print test summary to stdout."""
    print("=" * 70)
    print("TEST RESULTS SUMMARY")
    print("=" * 70)
    print(f"Total:   {summary.total}")
    print(f"Passed:  {summary.passed}")
    print(f"Failed:  {summary.failed}")
    print(f"Skipped: {summary.skipped}")
    print(f"Duration: {summary.duration:.2f}s")
    print("=" * 70)

    if summary.failures:
        print("\nFAILED TESTS:")
        print("-" * 70)
        for failure in summary.failures:
            print(f"\n  Test: {failure.name}")
            print(f"  File: {failure.filename}:{failure.line}")
            if failure.tags:
                print(f"  Tags: {failure.tags}")
            print(f"  Duration: {failure.duration:.4f}s")

            if failure.expressions:
                print("  Assertions:")
                for expr in failure.expressions:
                    print(f"    {expr.type} at {expr.filename}:{expr.line}")
                    print(f"      Expression: {expr.original}")
                    print(f"      Expanded:   {expr.expanded}")
        print("\n" + "-" * 70)
    elif summary.total > 0:
        print("\nAll tests passed!")


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Parse Catch2 XML test results and display summary'
    )
    parser.add_argument(
        'pattern',
        help='Glob pattern for XML result files (e.g., "test-results-*.xml")'
    )
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Show verbose output'
    )
    args = parser.parse_args()

    files = glob.glob(args.pattern)
    if not files:
        print(f"No files found matching pattern: {args.pattern}", file=sys.stderr)
        return 1

    summaries = []
    for filepath in sorted(files):
        if args.verbose:
            print(f"Parsing: {filepath}")
        summaries.append(parse_test_file(Path(filepath)))

    merged = merge_summaries(summaries)

    if merged.total == 0:
        print("No test results found in XML files.", file=sys.stderr)
        return 1

    print_summary(merged, args.verbose)

    return 1 if merged.failed > 0 else 0


if __name__ == '__main__':
    sys.exit(main())
