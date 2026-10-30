#!/usr/bin/env python3
"""A line of section tags holds one tag, so the renderer can strip it.

The renderer strips a line that holds nothing but a *single* section tag, and
only when the tag stands alone. A line that chains several tags,
= {{^a}}{{^b}}{{^c}} =, is not stripped: its newline survives whether the
condition holds or not, so an empty chain leaks a blank line into every file the
template produces.

That is the mechanical half of the leak class. The other half is a blank line
left outside a section in the template, which no static rule can judge: it
renders as a blank line whenever the section is empty, and whether that is
wanted depends on the template's intent. The chained-tag rule is what a scanner
can prove, so it is what this check states; a rendered-output test for the
second half belongs beside the template that needs it, as the protocol header
has one.

The rule is checked against a baseline while the one template that predates it
is cleaned up: the baseline holds the count per file, and the check fails when a
file grows past it, and when a file falls below it, because a baseline nobody
shrinks is a hiding place.

Usage: check_template_sections.py [--templates <dir>] [--baseline <file>]
"""

import argparse
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
TEMPLATES = os.path.join(ROOT, "projects", "ores.codegen", "library", "templates")
BASELINE = os.path.join(HERE, "template_section_baseline.json")

TAG = re.compile(r"\{\{([#^/])\s*([^}]+?)\s*\}\}")


def alone(text, start, end):
    """Whether nothing but whitespace shares the tag's line."""
    line_start = text.rfind("\n", 0, start) + 1
    line_end = text.find("\n", end)
    line_end = len(text) if line_end < 0 else line_end
    return not text[line_start:start].strip() and not text[end:line_end].strip()


def chained_lines(text):
    """The lines that hold more than one section tag and nothing else.

    Each is a hint that survived the strip the renderer gives a lone tag, and the
    line is a hazard when the section it opens or closes spans more than one line.
    """
    found = set()
    stack = []
    pairs = []
    for match in TAG.finditer(text):
        kind, name = match.group(1), match.group(2).strip()
        if kind in "#^":
            stack.append((name, match.start(), match.end()))
            continue
        for index in range(len(stack) - 1, -1, -1):
            if stack[index][0] != name:
                continue
            _, open_start, open_end = stack.pop(index)
            pairs.append((open_start, open_end, match.start(), match.end()))
            break
    for open_start, open_end, close_start, close_end in pairs:
        if text.count("\n", 0, open_start) == text.count("\n", 0, close_start):
            continue
        for start, end in ((open_start, open_end), (close_start, close_end)):
            line_start = text.rfind("\n", 0, start) + 1
            line_end = text.find("\n", end)
            line_end = len(text) if line_end < 0 else line_end
            line = text[line_start:line_end]
            if len(TAG.findall(line)) < 2 or TAG.sub("", line).strip():
                continue
            found.add(text.count("\n", 0, start) + 1)
    return sorted(found)


def census(directory):
    found = {}
    for path in sorted(os.listdir(directory)):
        if not path.endswith(".mustache"):
            continue
        with open(os.path.join(directory, path), encoding="utf-8") as handle:
            text = handle.read()
        lines = chained_lines(text)
        if lines:
            found[path] = {"count": len(lines), "lines": lines[:5]}
    return found


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--templates", default=TEMPLATES)
    parser.add_argument("--baseline", default=BASELINE)
    parser.add_argument("--write-baseline", action="store_true",
                        help="replace the baseline with the tree's own counts")
    args = parser.parse_args()

    found = census(args.templates)
    if args.write_baseline:
        with open(args.baseline, "w", encoding="utf-8") as handle:
            json.dump({"templates": {name: body["count"] for name, body in found.items()}},
                      handle, indent=4, sort_keys=True)
            handle.write("\n")
        print(f"Wrote {args.baseline}: {len(found)} template(s) hold a section tag"
              " that does not stand alone on its line.")
        return 0

    with open(args.baseline, encoding="utf-8") as handle:
        baseline = json.load(handle).get("templates", {})

    problems = []
    for name, body in sorted(found.items()):
        allowed = baseline.get(name, 0)
        if body["count"] > allowed:
            problems.append(
                f"{name}: {body['count']} line(s) chain several section tags,"
                f" {allowed} allowed; line {body['lines'][0]} of the template."
                " Give each tag its own line, so the renderer can strip it")
    for name, allowed in sorted(baseline.items()):
        found_count = found.get(name, {}).get("count", 0)
        if found_count < allowed:
            problems.append(
                f"{name}: {found_count} chained line(s), below the baseline's"
                f" {allowed}; shrink the baseline rather than leave room for the"
                " next one")
    for problem in problems:
        print(problem)
    print(f"{len(found)} of the templates chain section tags on a line,"
          f" {sum(b['count'] for b in found.values())} line(s) in all; the baseline"
          f" covers {sum(baseline.values())}, {len(problems)} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
