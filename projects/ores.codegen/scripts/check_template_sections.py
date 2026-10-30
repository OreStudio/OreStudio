#!/usr/bin/env python3
"""A mustache section tag stands alone on its line, or its section is one line.

The renderer strips a line that holds nothing but a section tag, so a section
that renders nothing emits no text and no newline. A tag that shares its line
with other tags or with text is not stripped: the newline around it is emitted
whatever the section does, and a template whose sections nest on one line leaks
one blank line per empty section.

That leak is not cosmetic. A template edit that changes how many such newlines
survive alters every file the template produces, so a two-byte change rewrites
an estate -- twice now, from a protocol header and from the field-group gate,
where 26 trading headers came back changed for no stated reason.

The rule is checked against a baseline while the templates that predate it are
cleaned up: the baseline holds the number of offending section pairs per file,
and the check fails when a file grows past it, and when a file falls below it,
because a baseline nobody shrinks is a hiding place.

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


def hazards(text):
    """The section pairs whose tags do not stand alone on their lines."""
    found = []
    stack = []
    for match in TAG.finditer(text):
        kind, name = match.group(1), match.group(2).strip()
        if kind in "#^":
            stack.append((name, match.start(), match.end()))
            continue
        for index in range(len(stack) - 1, -1, -1):
            if stack[index][0] != name:
                continue
            _, open_start, open_end = stack.pop(index)
            one_line = text.count("\n", 0, open_start) == text.count("\n", 0, match.start())
            if one_line:
                break
            if not (alone(text, open_start, open_end) and alone(text, match.start(), match.end())):
                found.append(text.count("\n", 0, open_start) + 1)
            break
    return found


def census(directory):
    found = {}
    for path in sorted(os.listdir(directory)):
        if not path.endswith(".mustache"):
            continue
        with open(os.path.join(directory, path), encoding="utf-8") as handle:
            text = handle.read()
        lines = hazards(text)
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
                f"{name}: {body['count']} section pair(s) share a line with other"
                f" text, {allowed} allowed; a line {body['lines'][0]} of the"
                " template. Put each tag on its own line, or keep the whole"
                " section on one line")
    for name, allowed in sorted(baseline.items()):
        found_count = found.get(name, {}).get("count", 0)
        if found_count < allowed:
            problems.append(
                f"{name}: {found_count} section pair(s) share a line, below the"
                f" baseline's {allowed}; shrink the baseline rather than leave"
                " room for the next one")
    for problem in problems:
        print(problem)
    print(f"{len(found)} of the templates hold {sum(b['count'] for b in found.values())}"
          f" offending pair(s); baseline covers {sum(baseline.values())},"
          f" {len(problems)} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
