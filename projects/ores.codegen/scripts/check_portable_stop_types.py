#!/usr/bin/env python3
"""
Check that no code outside ``ores.platform`` names a C++20 stop type.

Apple's libc++ implements none of the C++20 stop machinery: it has no
``std::jthread``, no ``std::stop_token``, no ``std::stop_source`` and no
``std::stop_callback``. A file that names one compiles on Linux and breaks the
macOS build, which runs at night, so the break reaches main before anything
reports it. It has happened three times: a comms thread on 2026-03-12, the
workflow engine's deadline watch on 2026-09-30, and the feed restore retry on
2026-10-19.

``ores.platform/concurrency/stoppable_thread.hpp`` names ``jthread``,
``stop_token``, ``stop_source`` and ``wait_for`` for every platform. They are
the standard types where the library has them and a portable subset where it
does not. This check fails on the standard types anywhere else, so the next
author finds out in the pull request and not at the nightly build. When Apple's
libc++ has the types, the header and this check go, and the code uses ``std::``. Comments and string literals are not code, so a comment
that names a standard type, as the header of the portable thread does, is not
a finding.

The check reads C++ sources and headers under ``projects/`` and writes nothing.

Usage:
  check_portable_stop_types.py
  check_portable_stop_types.py --self-test    # prove the check fails on a planted defect
"""
from __future__ import annotations

import argparse
import os
import re
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS_DIR = REPO_ROOT / "projects"

# The library that owns the portable types, and so may name the standard ones.
PLATFORM_DIR = "ores.platform"

SOURCE_SUFFIXES = frozenset({".cpp", ".cc", ".cxx", ".hpp", ".h", ".hxx", ".ipp", ".inc"})

# Vendored or generated trees, pruned during the walk.
SKIP_PARTS = frozenset({"node_modules", "venv", ".venv", "build", "external"})

STOP_TYPE_RE = re.compile(r"\bstd::(jthread|stop_token|stop_source|stop_callback)\b")
STOP_INCLUDE_RE = re.compile(r"^\s*#\s*include\s*<stop_token>")

# A comment or a string literal, matched left to right so a quote inside a
# comment, or a slash pair inside a string, does not start the other.
_NOISE_RE = re.compile(
    r'//[^\n]*|/\*.*?\*/|R"(?P<d>[^(\s]*)\(.*?\)(?P=d)"|"(?:\\.|[^"\\\n])*"',
    re.DOTALL,
)


def _blank_noise(text: str) -> str:
    """The text with comments and string literals blanked, newlines kept."""
    return _NOISE_RE.sub(lambda m: re.sub(r"[^\n]", " ", m.group(0)), text)


def _sources() -> list[Path]:
    found = []
    for dirpath, dirnames, filenames in os.walk(PROJECTS_DIR):
        dirnames[:] = sorted(d for d in dirnames if d not in SKIP_PARTS)
        for name in sorted(filenames):
            path = Path(dirpath) / name
            if path.suffix.lower() in SOURCE_SUFFIXES:
                found.append(path)
    return found


def _in_platform(path: Path) -> bool:
    try:
        parts = path.relative_to(PROJECTS_DIR).parts
    except ValueError:
        return False
    return bool(parts) and parts[0] == PLATFORM_DIR


def check() -> list[str]:
    """One message for each use of a standard stop type outside the platform."""
    findings = []
    for path in _sources():
        if _in_platform(path):
            continue
        text = path.read_text(encoding="utf-8", errors="ignore")
        for number, line in enumerate(_blank_noise(text).splitlines(), start=1):
            match = STOP_TYPE_RE.search(line)
            if match is None and STOP_INCLUDE_RE.match(line):
                name = "<stop_token>"
            elif match is not None:
                name = f"std::{match.group(1)}"
            else:
                continue
            try:
                shown = path.relative_to(REPO_ROOT)
            except ValueError:
                shown = path
            findings.append(
                f"{shown}:{number}: {name} is not portable. Use the same name in "
                "ores::platform::concurrency from stoppable_thread.hpp."
            )
    return findings


def self_test() -> int:
    """Plant a defect in a throw-away tree and require the check to find it."""
    global PROJECTS_DIR, REPO_ROOT
    saved = PROJECTS_DIR, REPO_ROOT
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        planted = root / "projects" / "ores.widget" / "src" / "planted.cpp"
        planted.parent.mkdir(parents=True)
        planted.write_text("std::jthread worker;\n", encoding="utf-8")
        ignored = root / "projects" / "ores.platform" / "include" / "ok.hpp"
        ignored.parent.mkdir(parents=True)
        ignored.write_text("std::jthread worker;\n", encoding="utf-8")
        PROJECTS_DIR, REPO_ROOT = root / "projects", root
        try:
            findings = check()
        finally:
            PROJECTS_DIR, REPO_ROOT = saved
    if len(findings) != 1 or "planted.cpp:1" not in findings[0]:
        print(f"self-test failed: expected one finding in planted.cpp, got {findings}")
        return 1
    print("self-test passed: the planted std::jthread was found.")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--self-test", action="store_true", help="plant a defect and require a finding")
    parser.add_argument("--all", action="store_true", help="accepted for the shared gate loop; the check is tree-wide")
    args = parser.parse_args()
    if args.self_test:
        return self_test()
    findings = check()
    for finding in findings:
        print(finding)
    if findings:
        print(f"\n{len(findings)} use(s) of a C++20 stop type outside ores.platform.")
        return 1
    print("no C++20 stop type is named outside ores.platform.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
