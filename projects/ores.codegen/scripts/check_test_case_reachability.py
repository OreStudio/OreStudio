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

== What the check reads

A case can be declared in a test source or in a fragment one includes, so the
check reads both: every file named ``*_tests.*``, and every C++ source or
fragment under a ``tests/`` directory. It resolves the backslash-newline splices
of translation phase 2 before it looks for anything, so a directive broken
across lines is a directive, and a case declared through a macro the file itself
defines to a test macro is a case. It also fails on a file whose conditionals do
not balance, because a guard left open leaks into whatever includes that file,
and the case it deletes is in the includer, where nothing looks conditional.

A compiler splices before it tokenises and then keeps the splice inside a raw
string's content, so a literal's extent comes from the spliced text while its
content still holds the newline. This check removes every splice, so a splice at
a literal's opening, or inside its content, can move where it ends, and then a
directive that is string content to the compiler reads as code here, or the
other way round. The check settles that by construction rather than by guessing:
it reads the raw strings of the spliced text and of the source, and where the
two disagree it reports the file and leaves its cases out of the census. A
splice inside a raw string that moves nothing agrees in both readings and is
left alone.

== What the check does not cover

The rule is static and per file. It does not expand macros beyond one level, so
a test macro reached through a chain of definitions is invisible to it. It does
not read the flags a translation unit is compiled with, so a conditional that is
merely wrong for one platform, rather than unreachable everywhere, is beyond it.
And it cannot see a case deleted by anything other than the preprocessor;
whether a declared case is in a target's source list belongs to
``regenerate_cmake_component_files.py --check``.

This check reads the tree and writes nothing.

Usage:
  check_test_case_reachability.py
"""
from __future__ import annotations

import os
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS_DIR = REPO_ROOT / "projects"

# A test source, and a fragment a test source may include.
TEST_SOURCE_SUFFIXES = frozenset(
    {".cpp", ".cc", ".cxx", ".c++", ".hpp", ".h", ".hxx", ".ipp", ".inc"}
)

# The directory whose contents are test code, whatever the file is called.
TEST_DIR = "tests"

# Directories that hold vendored or generated trees, never our sources. Pruned
# during the walk, so they are never descended into.
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

# A conditional directive that opens a region, and the one that closes it.
_OPENS = ("if", "ifdef", "ifndef")

_DIRECTIVE_RE = re.compile(r"^\s*#\s*([A-Za-z_]+)")
_DEFINE_RE = re.compile(r"^\s*#\s*define\s+([A-Za-z_][A-Za-z0-9_]*)")


def _macro_re(extra_names: frozenset[str] = frozenset()) -> re.Pattern:
    """A matcher for every test macro plus ``extra_names``, longest first.

    The trailing ``(`` is required: a declaration is an invocation.
    """
    names = sorted(set(TEST_MACROS) | set(extra_names), key=len, reverse=True)
    return re.compile(r"\b(?:" + "|".join(names) + r")(?![A-Za-z0-9_])\s*\(")


def _macro_name_re() -> re.Pattern:
    """A matcher for a test macro named anywhere, invocation or not.

    A ``#define`` binds its name to a test macro whether or not it repeats the
    argument list, so the replacement text is searched for the name alone:
    ``#define MY_CASE TEST_CASE`` is as much an alias as
    ``#define MY_CASE(n) TEST_CASE(n, tags)``.
    """
    names = sorted(TEST_MACROS, key=len, reverse=True)
    return re.compile(r"\b(?:" + "|".join(names) + r")(?![A-Za-z0-9_])")


_TEST_MACRO_RE = _macro_re()
_TEST_MACRO_NAME_RE = _macro_name_re()


def _relative_parts(path: Path) -> tuple[str, ...]:
    """``path`` relative to PROJECTS_DIR, or its own parts when outside it."""
    try:
        return path.relative_to(PROJECTS_DIR).parts
    except ValueError:
        return path.parts


def _test_sources() -> list[Path]:
    """Every file under ``projects/`` that may declare a case, sorted.

    Vendored trees are pruned rather than filtered, so the walk never descends
    into them. The skip test is on the path relative to PROJECTS_DIR, so a
    checkout that happens to live under a directory named ``build`` still sees
    the tree.
    """
    sources = []
    for dirpath, dirnames, filenames in os.walk(PROJECTS_DIR):
        dirnames[:] = sorted(d for d in dirnames if d not in SKIP_PARTS)
        here = Path(dirpath)
        in_tests = TEST_DIR in _relative_parts(here)
        for name in sorted(filenames):
            path = here / name
            if path.suffix.lower() not in TEST_SOURCE_SUFFIXES:
                continue
            if in_tests or path.stem.endswith("_tests"):
                sources.append(path)
    return sources


def _unsplice(text: str) -> tuple[str, list[int], list[int]]:
    """Apply the backslash-newline splices of translation phase 2.

    Returns the spliced text, the original 1-based line each spliced character
    came from, and the original offset each spliced character came from.
    Splicing runs before comments are recognised, which is what makes a
    directive split across lines a directive, a ``#end\\``/``if`` an
    ``#endif``, and a ``//`` comment swallow the line it is continued onto.
    """
    out: list[str] = []
    line_of: list[int] = []
    offset_of: list[int] = []
    line = 1
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if c == "\\" and text.startswith("\r\n", i + 1):
            i += 3
            line += 1
            continue
        if c == "\\" and text.startswith("\n", i + 1):
            i += 2
            line += 1
            continue
        out.append(c)
        line_of.append(line)
        offset_of.append(i)
        if c == "\n":
            line += 1
        i += 1
    return "".join(out), line_of, offset_of


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
        if c == "/" and text.startswith("//", i):
            stop = text.find("\n", i)
            stop = n if stop == -1 else stop
            blank(i, stop)
            i = stop
        elif c == "/" and text.startswith("/*", i):
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


def _raw_string_spans(text: str) -> list[tuple[int, int, int]]:
    """``(start, stop, line)`` for every raw string literal in ``text``.

    This reads one text at a time and splices nothing. It is run over the
    spliced text and over the source, and ``_raw_strings_diverge`` compares the
    two, because neither reading on its own is the compiler's: the compiler
    takes a literal's extent from the spliced text and its content from the
    source.
    """
    spans: list[tuple[int, int, int]] = []
    i = 0
    n = len(text)
    while i < n:
        if text.startswith("//", i):
            end = text.find("\n", i)
            i = n if end == -1 else end
        elif text.startswith("/*", i):
            end = text.find("*/", i + 2)
            i = n if end == -1 else end + 2
        elif text[i] == '"' and i > 0 and text[i - 1] == "R":
            open_paren = text.find("(", i)
            if open_paren == -1:
                i = n
                continue
            delim = text[i + 1:open_paren]
            end = text.find(")" + delim + '"', open_paren)
            end = n if end == -1 else end + len(delim) + 2
            spans.append((i - 1, end, text.count("\n", 0, i - 1) + 1))
            i = end
        elif text[i] == '"':
            j = i + 1
            while j < n and text[j] != '"':
                j += 2 if text[j] == "\\" else 1
            i = j + 1
        elif text[i] == "'" and _closes_char_literal(text, i):
            i = text.find("'", i + 1) + 1
        else:
            i += 1
    return spans


def _raw_strings_diverge(text: str) -> int | None:
    """The line of a raw string that splicing moves, or ``None``.

    A compiler splices before it tokenises, and then keeps the splice inside a
    raw string's content, so a literal's extent is read from the spliced text
    while its content still holds the newline. This check removes every splice,
    so a splice at a raw string's opening, or inside its content, can move where
    the literal ends. Reading the raw strings of both texts and comparing them
    settles it by construction, in both directions at once: any disagreement
    means a directive that is string content to the compiler may read as code
    here, or the other way round. A splice inside a raw string that changes
    nothing agrees in both readings and is left alone.
    """
    spliced, _, offset_of = _unsplice(text)
    original_spans = {(start, stop) for start, stop, _ in
                      _raw_string_spans(text)}
    # A span is half-open, so its end is one past the source offset of its last
    # character. Taking the offset of the character after the span would
    # overshoot by the length of any splice deleted between the two.
    spliced_spans = {
        (offset_of[start], offset_of[stop - 1] + 1)
        for start, stop, _ in _raw_string_spans(spliced)
    }
    if original_spans == spliced_spans:
        return None
    differing = sorted(original_spans ^ spliced_spans)
    return text.count("\n", 0, differing[0][0]) + 1 if differing else None


def _logical_lines(text: str) -> tuple[list[tuple[int, str]], list[str]]:
    """``(original line number, blanked logical line)``, plus the raw lines.

    A logical line is what the compiler sees after splicing: one or more
    physical lines with their splices removed.
    """
    spliced, line_of, _ = _unsplice(text)
    blanked = _blank_comments_and_literals(spliced)
    logical: list[tuple[int, str]] = []
    start = 0
    for offset, char in enumerate(blanked):
        if char == "\n":
            logical.append((line_of[start], blanked[start:offset]))
            start = offset + 1
    if start < len(blanked):
        logical.append((line_of[start], blanked[start:]))
    return logical, text.splitlines()


def _aliases(logical: list[tuple[int, str]]) -> frozenset[str]:
    """Names this file's own ``#define`` binds to a test macro.

    One level only: ``#define MY_CASE TEST_CASE`` and
    ``#define MY_CASE(n) TEST_CASE(n, tags)`` are both followed, a chain
    through two definitions is not.
    """
    names = set()
    for _, line in logical:
        define = _DEFINE_RE.match(line)
        if define and _TEST_MACRO_NAME_RE.search(line[define.end():]):
            names.add(define.group(1))
    return frozenset(names)


def scan_source(text: str) -> tuple[int, list[tuple[int, str, int, str]],
                                    list[tuple[int, str, str, str]]]:
    """Count the cases ``text`` declares, and find the ones that can vanish.

    Returns the declared case count; one tuple per case inside a conditional,
    as ``(line, line text, line of the opening directive, directive text)``; and
    one tuple per problem the check cannot certify the file for, as
    ``(line, line text, kind, message)``. Reported text is the original source
    line, not the blanked one.
    """
    logical, original = _logical_lines(text)
    macro_re = _macro_re(_aliases(logical))
    open_stack: list[tuple[int, str]] = []
    findings: list[tuple[int, str, int, str]] = []
    problems: list[tuple[int, str, str, str]] = []
    declared = 0

    def at(lineno: int) -> str:
        if 1 <= lineno <= len(original):
            return original[lineno - 1].strip()
        return ""

    for lineno, line in logical:
        directive = _DIRECTIVE_RE.match(line)
        if directive:
            keyword = directive.group(1)
            if keyword in _OPENS:
                open_stack.append((lineno, at(lineno)))
            elif keyword == "endif":
                if open_stack:
                    open_stack.pop()
                else:
                    problems.append((lineno, at(lineno), "unbalanced",
                                     "closes a conditional that nothing "
                                     "opened"))
            # ``elif`` and ``else`` continue the region already open.
            continue
        matches = macro_re.findall(line)
        if not matches:
            continue
        declared += len(matches)
        if open_stack:
            opened_at, opened_by = open_stack[-1]
            # One entry per case, so the count in the summary matches the
            # number of findings when a line declares more than one.
            for _ in matches:
                findings.append((lineno, at(lineno), opened_at, opened_by))

    for lineno, text_at in open_stack:
        problems.append((lineno, text_at, "unbalanced",
                         "opens a conditional that never closes"))

    spliced_raw = _raw_strings_diverge(text)
    if spliced_raw is not None:
        problems.append((spliced_raw, at(spliced_raw), "raw-string",
                         "a raw string literal here begins or ends somewhere "
                         "else once the backslash-newline splices are removed, "
                         "which is how a compiler reads it and not how this "
                         "check does, so its conditionals cannot be read from "
                         "this file alone"))

    return declared, findings, problems


def _rel(path: Path) -> str:
    """``path`` relative to REPO_ROOT, or its string form when outside it."""
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def main() -> int:
    sources = _test_sources()
    if not sources:
        print(f"no test sources found under {PROJECTS_DIR}", file=sys.stderr)
        return 1

    findings: list[tuple[Path, int, str, int, str]] = []
    problems: list[tuple[Path, int, str, str, str]] = []
    declared_by_path: dict[Path, int] = {}
    for path in sources:
        text = path.read_text(encoding="utf-8", errors="replace")
        source_declared, source_findings, source_problems = scan_source(text)
        declared_by_path[path] = source_declared
        for lineno, line, opened_at, opened_by in source_findings:
            findings.append((path, lineno, line, opened_at, opened_by))
        for lineno, line, kind, message in source_problems:
            problems.append((path, lineno, line, kind, message))

    if not sum(declared_by_path.values()):
        print(f"no test case declared in {len(sources)} test source(s); the "
              f"macros this check looks for are {', '.join(TEST_MACROS)}",
              file=sys.stderr)
        return 1

    # Where a raw string moves under splicing, the conditional reading of that
    # file is not to be trusted either way, so the file is reported once, for
    # the reason the reading failed, and its cases are left out of the census.
    unreadable = {path for path, _line, _text, kind, _msg in problems
                  if kind == "raw-string"}
    findings = [f for f in findings if f[0] not in unreadable]
    declared = sum(n for path, n in declared_by_path.items()
                   if path not in unreadable)

    failures = 0
    if findings:
        print("Test case(s) inside a conditional compilation block:",
              file=sys.stderr)
        # One line can declare more than one case; it is listed once, while the
        # summary counts every case it declares.
        listed: set[tuple[Path, int, int]] = set()
        for path, lineno, line, opened_at, opened_by in findings:
            key = (path, lineno, opened_at)
            if key in listed:
                continue
            listed.add(key)
            print(f"  {_rel(path)}:{lineno}: {line[:88]}", file=sys.stderr)
            print(f"      inside the block opened at line {opened_at}: "
                  f"{opened_by[:88]}", file=sys.stderr)
        failures += 1
    for kind, heading in (("unbalanced", "File(s) whose conditionals do not "
                                        "balance:"),
                          ("raw-string", "File(s) whose raw strings this check "
                                         "cannot read:")):
        group = [p for p in problems if p[3] == kind]
        if not group:
            continue
        print(heading, file=sys.stderr)
        for path, lineno, line, _kind, message in group:
            print(f"  {_rel(path)}:{lineno}: {line[:88]}", file=sys.stderr)
            print(f"      {message}", file=sys.stderr)
        failures += 1

    if failures:
        if findings:
            print(f"\n{len(findings)} of {declared} declared case(s) can be "
                  "compiled away, and a suite that loses them still reports "
                  "green. Declare the case unconditionally and move the "
                  "condition into the body, with SKIP(...) in the branch that "
                  "cannot run.", file=sys.stderr)
        if any(p[3] == "unbalanced" for p in problems):
            print("\nA guard that a file opens and does not close applies to "
                  "whatever includes that file, so a case in the includer can "
                  "be deleted where nothing looks conditional. Close the guard "
                  "in the file that opens it.", file=sys.stderr)
        if unreadable:
            print("\nSplicing moves a raw string literal in that file, and a "
                  "compiler reads the literal from the spliced text while this "
                  "check reads it from the source, so its conditionals cannot "
                  "be settled from the file alone and its cases are left out "
                  "of the census above. Remove the trailing backslash, or move "
                  "the literal out of the test source.", file=sys.stderr)
        return 1

    print(f"Test case reachability intact: {declared} declared case(s) in "
          f"{len(sources)} test source(s), none inside a conditional "
          "compilation block, every conditional is balanced, and no raw string "
          "moves under splicing.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
