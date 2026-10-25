#!/usr/bin/env python3
"""Turn a harness run into the sign off sheet of one screen: every line of the
screen review checklist, with the harness's mark where it checks the line and a
blank for the person to fill where it does not.

The checklist names the rule identifiers each line rests on. A line is the
harness's when the run has a check for every identifier on it; the mark is then
✗ if any check failed, – if any was skipped, and ✓ otherwise. A line with an
identifier the harness does not check is the person's, and the Note column
names the identifiers still to walk. A line with no identifier is the person's
too.

Usage: screen_sign_off.py <screen> [results.json] [--checklist <path>]
<screen> is the describe title the harness gives the screen, "record currencies"
or "classification calendar-type"; the kind alone also matches. Prints the org
sheet the sign off recipe records beside the task.
"""
import argparse
import json
import pathlib
import re
import sys

from harness_baseline import fold_columns, walk

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent.parent.parent
DEFAULT_RESULTS = HERE.parent / ".runtime" / "harness" / "results.json"
DEFAULT_CHECKLIST = ROOT / "doc" / "knowledge" / "ui" / "screen_review_checklist.org"
LINE = re.compile(r"^- \[ \] (.*) \(([^()]*)\)\s*$")
SECTION = re.compile(r"^\*\* (.*)$")
RULE = re.compile(r"\b(?:RS|ES)-[A-Z]+-\d{2}\b")
WORST = {"✗": 0, "–": 1, "✓": 2}


def checklist_lines(path):
    """Yield (section, text, rules, source) per line of the checklist, where
    rules is the list of identifiers the line names, possibly empty."""
    section = ""
    for raw in path.read_text(encoding="utf-8").splitlines():
        heading = SECTION.match(raw)
        if heading:
            section = heading.group(1)
            continue
        line = LINE.match(raw)
        if not line:
            continue
        text, source = line.groups()
        rules = RULE.findall(source)
        yield section, text, rules, source


def screen_marks(report, screen):
    """The marks of one screen, keyed by column, from the folded report."""
    out = {}
    for suite in report.get("suites", []):
        walk(suite, [suite.get("title", "")], out)
    screens = fold_columns({k: v for k, v in out.items() if k})
    matches = [s for s in screens if s == screen or s.endswith(f" {screen}")]
    if len(matches) != 1:
        found = ", ".join(sorted(screens)) or "none"
        raise SystemExit(f"screen {screen!r} matches {len(matches)} of the run's screens: {found}")
    return matches[0], screens[matches[0]]


def judge(rules, marks):
    """The harness cell, the person cell and the note for one line."""
    cells = {}
    for rule in rules:
        columns = sorted(c for c in marks if c == rule or c.startswith(f"{rule} "))
        if columns:
            cells[rule] = {c: marks[c] for c in columns}
    unanswered = [r for r in rules if r not in cells]
    if not cells:
        return "", "walk", ""
    all_marks = [m for by_column in cells.values() for m in by_column.values()]
    overall = min(all_marks, key=lambda m: WORST[m])
    if len(rules) == 1 and len(all_marks) == 1:
        harness = overall
    else:
        parts = []
        for rule, by_column in cells.items():
            for column, mark in by_column.items():
                part = column.removeprefix(rule).strip()
                parts.append(f"{rule}{' ' + part if part else ''} {mark}")
        harness = ", ".join(parts)
    if unanswered:
        return harness, "walk", "the harness does not check " + ", ".join(unanswered)
    return harness, "", ""


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("screen", help="the harness's describe title of the screen, or its kind alone")
    parser.add_argument("results", nargs="?", type=pathlib.Path, default=DEFAULT_RESULTS, help="the JSON report of the run")
    parser.add_argument("--checklist", type=pathlib.Path, default=DEFAULT_CHECKLIST, help="the screen review checklist")
    args = parser.parse_args()
    checklist = args.checklist
    report = json.loads(args.results.read_text(encoding="utf-8"))
    screen, marks = screen_marks(report, args.screen)
    started = report.get("stats", {}).get("startTime", "an unknown time")

    print(f"Sign off sheet for ={screen}=, from the harness run of {started}.")
    print("The Harness column is the run's; a person fills Person with Meets, Gap, N/A or")
    print("Not checked on every line marked walk, and the Note with what was seen.")
    print()
    print("| Line | Rules | Harness | Person | Note |")
    print("|------+-------+---------+--------+------|")
    section = None
    marked = failed = to_walk = whole = 0
    for current, text, rules, source in checklist_lines(checklist):
        if current != section:
            section = current
            print(f"| *{section}* | | | | |")
        harness, person, note = judge(rules, marks)
        marked += bool(harness)
        failed += "✗" in harness
        to_walk += person == "walk"
        whole += person == "walk" and not harness
        print(f"| {text} | {', '.join(rules) or source} | {harness} | {person} | {note} |")
    print()
    print(
        f"The harness marks {marked} lines, {failed} of them with a failure; "
        f"{to_walk} lines need the person, {whole} of them with no harness mark at all."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
