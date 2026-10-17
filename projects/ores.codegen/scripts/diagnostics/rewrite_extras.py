#!/usr/bin/env python3
"""Rewrite each component's extra_dependencies and extra_test_dependencies block
so it carries every target_link_libraries call the hand CMake made, verbatim.

The first extraction took only the first call, so a second call -- a
PRIVATE-only addendum, or the one-line ores.rfl.pch link -- was dropped, and the
include directory that came with it vanished from every consumer.

Dry run by default; --write applies.
"""

import os
import re
import subprocess
import sys

BASE = "ed7e0a3a87"
ROOT = os.getcwd()
BLOCK = re.compile(r"target_link_libraries\s*\(", re.I)
FIND = re.compile(r"^[ \t]*find_package\s*\(", re.M)
BEGIN = re.compile(
    r"^#\+begin_src cmake :name (extra_dependencies|extra_test_dependencies)\n"
    r"(.*?)^#\+end_src\n", re.M | re.S)

# A find_package call over more than one line would not survive a line copy.
MULTILINE = re.compile(r"find_package\s*\([^)]*$", re.M)


def calls(text):
    """Every target_link_libraries call, verbatim including the parens."""
    out = []
    for m in BLOCK.finditer(text):
        depth, i = 1, m.end()
        while i < len(text) and depth:
            if text[i] == "(":
                depth += 1
            elif text[i] == ")":
                depth -= 1
            i += 1
        out.append(text[m.start():i])
    return out


def find_packages(text):
    lines = []
    for m in FIND.finditer(text):
        end = text.find(")", m.start())
        lines.append(text[m.start():end + 1].strip())
    return lines


def normalise(call):
    """The call as the hand file wrote it. Every link call starts at column 0
    there, so the layout is already correct for insertion."""
    return "\n".join(ln.rstrip() for ln in call.splitlines())


def target_of(call):
    """The target a link call names, e.g. '${lib_target_name}'."""
    return call[call.index("(") + 1:].split()[0]


def block_for(name):
    """The only target an extras block speaks for. A hand src/CMakeLists also
    links the service app's exe target further down, and a call for that target
    placed in the library section names a target that does not exist yet."""
    return "${tests_target_name}" if name == "extra_test_dependencies" \
        else "${lib_target_name}"


def hand_text(path):
    r = subprocess.run(["git", "show", f"{BASE}:{path}"],
                       capture_output=True, text=True)
    return r.stdout if r.returncode == 0 else None


def main():
    write = "--write" in sys.argv
    overviews = subprocess.run(
        ["grep", "-rl", "-e", "name extra_dependencies",
         "-e", "name extra_test_dependencies", "projects"],
        capture_output=True, text=True).stdout.split()
    overviews = [o for o in overviews if o.endswith("component_overview.org")]
    changed = made = skipped = 0
    for overview in overviews:
        comp = os.path.dirname(os.path.dirname(overview))
        text = open(overview).read()

        def replace(m):
            nonlocal changed, made, skipped
            name, body = m.group(1), m.group(2)
            kind = "tests" if name == "extra_test_dependencies" else "src"
            hand = hand_text(f"{comp}/{kind}/CMakeLists.txt")
            if hand is None:
                print(f"SKIP  {overview}: no hand {kind}/CMakeLists.txt")
                skipped += 1
                return m.group(0)
            if MULTILINE.search(hand):
                print(f"FLAG  {overview}: multi-line find_package")
            blocks = [normalise(c) for c in calls(hand)]
            want = block_for(name)
            foreign = [t for t in (target_of(c) for c in blocks) if t != want]
            blocks = [c for c in blocks if target_of(c) == want]
            if foreign:
                print(f"NOTED {overview}: {name} leaves "
                      f"{', '.join(sorted(set(foreign)))} to its own archetype")
            if not blocks:
                print(f"SKIP  {overview}: hand {kind} links nothing for {want}")
                skipped += 1
                return m.group(0)
            parts = find_packages(hand) + blocks
            new = "\n\n".join(parts) + "\n"
            if new != body:
                changed += 1
                made += len(blocks)
            return (f"#+begin_src cmake :name {name}\n{new}#+end_src\n")

        new_text = BEGIN.sub(replace, text)
        if new_text != text and write:
            open(overview, "w").write(new_text)

    print(f"\n{changed} blocks rewritten, {made} link calls carried, "
          f"{skipped} skipped, write={write}")


main()
