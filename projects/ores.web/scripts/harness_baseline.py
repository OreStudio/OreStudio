#!/usr/bin/env python3
"""Turn a harness run into the baseline table: one row per screen, one column
per rule, each cell what the run found.

The harness writes a JSON report to .runtime/harness/results.json. A test's
title carries the identifier of the rule it checks in square brackets, and its
describe names the screen, so the report folds into a table of rule against
screen without any further knowledge of the tests.

Usage: harness_baseline.py [results.json] [--org]
Prints a summary by default; --org prints the org table the baseline page
carries, with ✓ for a pass, ✗ for a failure and – for a check the harness
skipped on that screen.
"""
import json
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
DEFAULT = HERE.parent / ".runtime" / "harness" / "results.json"
RULE = re.compile(r"\[((?:RS|ES)-[A-Z]+-\d{2})\]")
SCREEN = re.compile(r"^(record|classification) [a-z-]+$")


# Every other status, unexpected, flaky and timedOut, is a failure.
MARK = {"skipped": "–", "expected": "✓"}


def walk(suite, titles, out):
    """Fold the report into {screen: {column: mark}}. A rule checked on more than
    one part of a screen, such as the load time on the list and on the record
    page, gets one column per part, named by the spec that checks it."""
    for spec in suite.get("specs", []):
        rule = RULE.search(spec["title"])
        stage = next((t.removesuffix(".spec.ts") for t in titles if t.endswith(".spec.ts")), "")
        column = f"{rule.group(1)} {stage}" if rule else spec["title"]
        screen = next((t for t in titles if SCREEN.match(t)), "")
        for test in spec.get("tests", []):
            out.setdefault(screen, {})[column] = MARK.get(test.get("status", ""), "✗")
    for child in suite.get("suites", []):
        walk(child, titles + [child.get("title", "")], out)


def fold_columns(screens):
    """A rule that one spec alone checks keeps its bare name."""
    by_rule = {}
    for cells in screens.values():
        for column in cells:
            by_rule.setdefault(column.split(" ")[0], set()).add(column)
    rename = {c: (c.split(" ")[0] if len(cs) == 1 else c) for cs in by_rule.values() for c in cs}
    return {s: {rename[c]: v for c, v in cells.items()} for s, cells in screens.items()}


def main() -> int:
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    path = pathlib.Path(args[0]) if args else DEFAULT
    report = json.loads(path.read_text(encoding="utf-8"))
    out: dict[str, dict[str, str]] = {}
    for suite in report.get("suites", []):
        walk(suite, [suite.get("title", "")], out)
    screens = fold_columns({k: v for k, v in out.items() if k})
    rules = sorted({r for cells in screens.values() for r in cells})
    if "--org" in sys.argv:
        print("| Screen | " + " | ".join(f"={r}=" for r in rules) + " |")
        print("|" + "+".join(["---"] * (len(rules) + 1)) + "|")
        for screen in sorted(screens):
            print(f"| {screen} | " + " | ".join(screens[screen].get(r, " ") for r in rules) + " |")
        return 0
    failed = sum(1 for cells in screens.values() for v in cells.values() if v == "✗")
    passed = sum(1 for cells in screens.values() for v in cells.values() if v == "✓")
    skipped = sum(1 for cells in screens.values() for v in cells.values() if v == "–")
    print(f"{len(screens)} screens, {len(rules)} rules, {passed} passed, {failed} failed, {skipped} skipped")
    by_rule: dict[str, int] = {}
    for cells in screens.values():
        for rule, v in cells.items():
            if v == "✗":
                by_rule[rule] = by_rule.get(rule, 0) + 1
    for rule, n in sorted(by_rule.items(), key=lambda kv: -kv[1]):
        print(f"  {rule:16s} fails on {n} screen(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
