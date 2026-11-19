#!/usr/bin/env python3
"""Give CDash one test entry per Catch2 test case.

ctest runs each suite as one test, so CDash shows one entry for a suite of
hundreds of cases: a failed suite names no case, and a slow suite does not
say which case is slow. Every suite already writes its Catch2 results to the
file its command names with =-r xml::out=<path>=. This script reads those
files and rewrites ctest's Test.xml, after ctest_test and before
ctest_submit, so that each suite entry becomes one entry per case, named
=<suite>.<case>=, with the case's status, time and failure text.

The suites still run as one process each, so nothing about how they run
changes: no extra process starts, and no extra test tenant is provisioned.

A suite entry stays as it is when its results cannot account for it: the
results file is missing, unreadable or older than the run (the binary
crashed, timed out or never started), or the suite failed while every case
passed (a registration error or a crash after the last case). A failure is
never hidden behind passing cases.

Usage::

    expand_catch2_cases.py <Testing/<tag>/Test.xml>
"""

from __future__ import annotations

import re
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from pathlib import Path

RESULTS_PATH = re.compile(r'xml::out=([^"]+)')


@dataclass(frozen=True)
class Case:
    name: str
    status: str
    seconds: float
    text: str


def measurement(test: ET.Element, name: str) -> str | None:
    for m in test.iterfind("Results/NamedMeasurement"):
        if m.get("name") == name:
            return m.findtext("Value")
    return None


def results_path(test: ET.Element) -> Path | None:
    command = test.findtext("FullCommandLine") or ""
    found = RESULTS_PATH.search(command)
    return Path(found.group(1)) if found else None


def failure_lines(element: ET.Element, sections: tuple[str, ...] = ()) -> list[str]:
    """The failures, exceptions and messages under a test case, in order."""
    lines: list[str] = []
    where = " / ".join(sections)
    prefix = f"[{where}] " if where else ""
    for child in element:
        location = f"{child.get('filename', '')}:{child.get('line', '')}"
        if child.tag == "Section":
            lines += failure_lines(child, sections + (child.get("name", ""),))
        elif child.tag == "Expression" and child.get("success") == "false":
            original = (child.findtext("Original") or "").strip()
            expanded = (child.findtext("Expanded") or "").strip()
            lines.append(f"{prefix}{location}: {child.get('type', '')}"
                         f"( {original} ) with expansion: {expanded}")
        elif child.tag in ("Exception", "FatalErrorCondition", "Failure",
                           "Info", "Warning"):
            lines.append(f"{prefix}{location}: {child.tag}: "
                         f"{(child.text or '').strip()}")
    return lines


def read_cases(path: Path) -> list[Case]:
    root = ET.parse(path).getroot()
    cases: list[Case] = []
    for test_case in root.iter("TestCase"):
        overall = test_case.find("OverallResult")
        if overall is None:
            raise ValueError(f"{test_case.get('name')} has no OverallResult")
        if overall.get("success") != "true":
            status = "failed"
        elif int(overall.get("skips", "0")) > 0:
            status = "notrun"
        else:
            status = "passed"
        text = failure_lines(test_case)
        for stream in ("StdOut", "StdErr"):
            captured = (overall.findtext(stream) or "").strip()
            if captured:
                text.append(f"{stream}:\n{captured}")
        cases.append(Case(name=test_case.get("name", ""),
                          status=status,
                          seconds=float(overall.get("durationInSeconds", "0")),
                          text="\n".join(text)))
    return cases


def usable_cases(test: ET.Element) -> list[Case] | None:
    """The suite's cases, or None when its entry must stay as it is."""
    path = results_path(test)
    if path is None or not path.is_file():
        return None
    started = float(test.findtext("StartTestTime") or "0")
    if path.stat().st_mtime < started:
        return None
    try:
        cases = read_cases(path)
    except (ET.ParseError, ValueError):
        return None
    if not cases:
        return None
    suite_failed = test.get("Status") != "passed"
    if suite_failed and not any(c.status == "failed" for c in cases):
        return None
    return cases


def named(kind: str, name: str, value: str) -> ET.Element:
    m = ET.Element("NamedMeasurement", {"type": kind, "name": name})
    ET.SubElement(m, "Value").text = value
    return m


def case_entry(suite: ET.Element, case: Case) -> ET.Element:
    suite_name = suite.findtext("Name") or ""
    path = suite.findtext("Path") or "."
    binary = (suite.findtext("FullCommandLine") or "").split(" ")[0]
    name = f"{suite_name}.{case.name}"
    command = f'{binary} "{case.name}"'

    entry = ET.Element("Test", {"Status": case.status})
    ET.SubElement(entry, "Name").text = name
    ET.SubElement(entry, "Path").text = path
    ET.SubElement(entry, "FullName").text = f"{path}/{name}"
    ET.SubElement(entry, "FullCommandLine").text = command
    ET.SubElement(entry, "StartTestTime").text = suite.findtext("StartTestTime")
    results = ET.SubElement(entry, "Results")
    if case.status == "failed":
        results.append(named("text/string", "Exit Code", "Failed"))
    results.append(named("numeric/double", "Execution Time", f"{case.seconds:.6f}"))
    results.append(named("text/string", "Completion Status",
                         "Completed" if case.status != "notrun" else "Skipped"))
    results.append(named("text/string", "Command Line", command))
    ET.SubElement(ET.SubElement(results, "Measurement"), "Value").text = case.text
    labels = suite.find("Labels")
    if labels is not None:
        entry.append(labels)
    return entry


def expand(tree: ET.ElementTree) -> tuple[int, int]:
    """Rewrite the tree in place; return the suites expanded and the cases added."""
    testing = tree.getroot().find("Testing")
    if testing is None:
        return 0, 0
    test_list = testing.find("TestList")
    suites = cases_added = 0
    for test in list(testing):
        if test.tag != "Test":
            continue
        cases = usable_cases(test)
        if cases is None:
            continue
        index = list(testing).index(test)
        testing.remove(test)
        entries = [case_entry(test, c) for c in cases]
        for offset, entry in enumerate(entries):
            testing.insert(index + offset, entry)
        if test_list is not None:
            full_name = test.findtext("FullName")
            for listed in list(test_list):
                if listed.text == full_name:
                    test_list.remove(listed)
            for entry in entries:
                ET.SubElement(test_list, "Test").text = entry.findtext("FullName")
        suites += 1
        cases_added += len(entries)
    return suites, cases_added


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    path = Path(argv[1])
    tree = ET.parse(path)
    suites, cases = expand(tree)
    tree.write(path, encoding="UTF-8", xml_declaration=True)
    print(f"expand_catch2_cases: {suites} suites expanded into {cases} cases")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
