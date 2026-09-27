#!/usr/bin/env python3
"""
Check that no test case sits inside a conditional compilation block.

A ``TEST_CASE`` inside a conditional block that is false does not exist. The
translation unit compiles, the link succeeds, ctest runs the suite and reports
green, and the only evidence is a case count that nothing compares against the
sources. The measured instance was the ``ores.service`` readiness suite: its
three cases sat inside ``#if defined(BOOST_ASIO_HAS_LOCAL_SOCKETS)``, and the
guard was placed above every boost::asio include, so the macro was never
defined when the guard was read and the file compiled to nothing. ctest
reported 39 cases where the sources held 42. A clean-standard record then cited
that suite as the coverage for ``systemd_notify.cpp``, so the record claimed
verification it did not have.

Every way a case can fail to reach the compiler runs through the preprocessor,
so one rule covers the whole class: a test case declaration may not sit inside a
conditional block. The alternatives are worse. Preprocessing each source with
its own flags from ``compile_commands.json`` and comparing the cases that
survive against the cases declared is exact, and it cannot gate a pull request:
the codegen job has no C++ build, and the job that has one runs on a schedule
and on tags. A rule that forbids the construct needs no flags, no build and no
formatter, and it makes the class impossible instead of reporting it.

The condition belongs inside the body, where the case still registers and
reports as skipped:

    TEST_CASE("reads the socket", tags) {
    #if defined(HAS_SOCKETS)
        CHECK(...);
    #else
        SKIP("sockets are unavailable on this platform");
    #endif
    }

A test source that declares a case this way has a stable case count, and a
reader sees the skip instead of an absence.

This check reads the tree and writes nothing.

Usage:
  check_test_case_reachability.py
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS_DIR = REPO_ROOT / "projects"

# Where test sources live. Every declaration in the tree is in a file with this
# name, so the gate does not need to guess which .cpp files are tests.
TEST_SOURCE_GLOB = "*/**/*_tests.cpp"

# Path components that hold vendored or generated trees, never our test sources.
SKIP_PARTS = frozenset({"node_modules", "venv", ".venv", "build", "external"})

# The Catch2 macros that declare a case. Longest first, because ``TEST_CASE``
# is a prefix of several of them and an alternation matches its first arm.
TEST_MACROS = (
    "TEMPLATE_TEST_CASE_METHOD_SIG",
    "TEMPLATE_PRODUCT_TEST_CASE",
    "TEMPLATE_TEST_CASE_METHOD",
    "TEMPLATE_LIST_TEST_CASE",
    "TEST_CASE_PERSISTENT_FIXTURE",
    "TEMPLATE_TEST_CASE_SIG",
    "TEST_CASE_METHOD_SIG",
    "TEST_CASE_METHOD",
    "TEMPLATE_TEST_CASE",
    "SCENARIO_METHOD",
    "TEST_CASE",
    "SCENARIO",
)

_TEST_MACRO_RE = re.compile(
    r"\b(?:" + "|".join(TEST_MACROS) + r")(?![A-Za-z0-9_])\s*\("
)

# A conditional directive that opens a region, and the one that closes it.
_OPENS = ("if", "ifdef", "ifndef")
_DIRECTIVE_RE = re.compile(r"^\s*#\s*([A-Za-z_]+)")


def _test_sources() -> list[Path]:
    """Every test source under ``projects/``, sorted by path."""
    sources = []
    for path in sorted(PROJECTS_DIR.glob(TEST_SOURCE_GLOB)):
        if any(part in SKIP_PARTS for part in path.parts):
            continue
        sources.append(path)
    return sources


def _blank_comments_and_literals(text: str) -> str:
    """``text`` with comments and string/char literals blanked out.

    Length and line breaks are preserved, so a line number in the result is a
    line number in the original. This is what keeps the scan honest in both
    directions: a ``#if`` inside a comment or a raw string is not a directive,
    and a ``TEST_CASE`` named inside one is not a case.
    """
    out = list(text)
    i = 0
    n = len(text)

    def blank(start: int, stop: int) -> None:
        for k in range(start, stop):
            if text[k] != "\n":
                out[k] = " "

    while i < n:
        c = text[i]
        if c == "/" and i + 1 < n and text[i + 1] == "/":
            stop = text.find("\n", i)
            stop = n if stop == -1 else stop
            # A line comment ends at the backslash-newline splice, not at the
            # newline, so a directive on the next line is still inside it.
            while stop < n and stop > 0 and text[stop - 1] == "\\":
                stop = text.find("\n", stop + 1)
                stop = n if stop == -1 else stop
            blank(i, stop)
            i = stop
        elif c == "/" and i + 1 < n and text[i + 1] == "*":
            stop = text.find("*/", i + 2)
            stop = n if stop == -1 else stop + 2
            blank(i, stop)
            i = stop
        elif c == '"' and i > 0 and text[i - 1] == "R":
            # A raw string: R"delim(...)delim". The delimiter is whatever sits
            # between the quote and the first parenthesis.
            open_paren = text.find("(", i)
            if open_paren == -1:
                blank(i, n)
                i = n
                continue
            delim = text[i + 1:open_paren]
            stop = text.find(")" + delim + '"', open_paren)
            stop = n if stop == -1 else stop + len(delim) + 2
            blank(i, stop)
            i = stop
        elif c == '"':
            j = i + 1
            while j < n and text[j] != '"':
                j += 2 if text[j] == "\\" else 1
            blank(i, min(j + 1, n))
            i = j + 1
        elif c == "'" and _closes_char_literal(text, i):
            j = text.find("'", i + 1)
            blank(i, j + 1)
            i = j + 1
        else:
            i += 1
    return "".join(out)


def _closes_char_literal(text: str, start: int) -> bool:
    """Whether the quote at ``start`` opens a char literal rather than a digit
    separator, as in ``1'000'000``."""
    if start > 0 and text[start - 1].isdigit():
        return False
    end = text.find("'", start + 1)
    if end == -1 or end - start > 8:
        return False
    return "\n" not in text[start:end]


def scan_source(text: str) -> tuple[int, list[tuple[int, str, int, str]]]:
    """Count the cases ``text`` declares and locate the conditional ones.

    Returns the declared case count, then one tuple per conditional case: the
    case's line number and line text, then the line number and text of the
    directive that opened the region. The reported text is the original source
    line, not the blanked one the scan matches against.
    """
    stripped_lines = _blank_comments_and_literals(text).splitlines()
    original_lines = text.splitlines()
    open_stack: list[tuple[int, str]] = []
    findings: list[tuple[int, str, int, str]] = []
    declared = 0

    for lineno, line in enumerate(stripped_lines, 1):
        directive = _DIRECTIVE_RE.match(line)
        if directive:
            keyword = directive.group(1)
            if keyword in _OPENS:
                open_stack.append((lineno, original_lines[lineno - 1].strip()))
            elif keyword == "endif" and open_stack:
                open_stack.pop()
            # ``elif`` and ``else`` continue the region already open.
            continue
        matches = _TEST_MACRO_RE.findall(line)
        if not matches:
            continue
        declared += len(matches)
        if open_stack:
            opened_at, opened_by = open_stack[-1]
            findings.append((lineno, original_lines[lineno - 1].strip(),
                             opened_at, opened_by))
    return declared, findings


def _rel(path: Path) -> str:
    """``path`` relative to REPO_ROOT, or its string form when outside it."""
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def main() -> int:
    sources = _test_sources()
    if not sources:
        print(f"no test sources matched {PROJECTS_DIR}/{TEST_SOURCE_GLOB}",
              file=sys.stderr)
        return 1

    findings: list[tuple[Path, int, str, int, str]] = []
    declared = 0
    for path in sources:
        text = path.read_text(encoding="utf-8", errors="replace")
        source_declared, source_findings = scan_source(text)
        declared += source_declared
        for lineno, line, opened_at, opened_by in source_findings:
            findings.append((path, lineno, line, opened_at, opened_by))

    if not declared:
        print(f"no test case declared in {len(sources)} test source(s); the "
              f"macros this check looks for are {', '.join(TEST_MACROS)}",
              file=sys.stderr)
        return 1

    if findings:
        print("Test case(s) inside a conditional compilation block:",
              file=sys.stderr)
        for path, lineno, line, opened_at, opened_by in findings:
            print(f"  {_rel(path)}:{lineno}: {line[:88]}", file=sys.stderr)
            print(f"      inside the block opened at line {opened_at}: "
                  f"{opened_by[:88]}", file=sys.stderr)
        print(f"\n{len(findings)} of {declared} declared case(s) can be "
              "compiled away, and a suite that loses them still reports "
              "green. Declare the case unconditionally and move the condition "
              "into the body, with SKIP(...) in the branch that cannot run.",
              file=sys.stderr)
        return 1

    print(f"Test case reachability intact: {declared} declared case(s) in "
          f"{len(sources)} test source(s), none inside a conditional "
          "compilation block.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
