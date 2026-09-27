#!/usr/bin/env python3
"""Print every vacuous test case in a component as Markdown.

Item V07 of doc/knowledge/architecture/component_clean_standard.org asks
that no test pass while its subject does nothing, and names the forms that
give a test away: one that asserts only a length, a non-throw, a
truthiness, or one production run against another. It also names the proof:
stub the subject and re-run, and the test must fail.

This is the census that precedes the proof. It reads every TEST_CASE body
in the component's test trees and classifies each assertion as weak or
strong. An assertion is weak when it cannot distinguish a subject that
worked from one that returned nothing:

  * CHECK_NOTHROW / REQUIRE_NOTHROW
  * a comparison on a container's size or length, whatever the other side
  * emptiness or has_value, with or without negation
  * a bare identifier or its negation, UNLESS the test itself assigned that
    identifier -- a `bool found` set inside a loop from a field comparison
    is exactly the assertion the item asks for

Everything else is strong, because it compares a value. A test case with no
strong assertion is reported.

The census is not the proof. A case it reports may still be defensible, and
a case it passes may still be vacuous in a way only stubbing shows. Use it
to find the work, then stub to prove the fix.

Usage:
    python3 projects/ores.codegen/scripts/survey_vacuous_tests.py --component dq
    python3 projects/ores.codegen/scripts/survey_vacuous_tests.py --component dq --quiet
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]

TEST_CASE_RE = re.compile(r"TEST_CASE\(\s*\"([^\"]+)\"")

# A comparison on a container's size asserts a count, not a value.
SIZE_ONLY = re.compile(r"\.(size|length)\(\)\s*(==|>=|>|<=|<)")
EMPTY_ONLY = re.compile(r"^!?[\w.\[\]\(\)>-]*\.(empty|has_value)\(\)$")
TRUTHY_ONLY = re.compile(r"^!?[A-Za-z_][\w.\[\]>-]*$")


def component_dirs(slug: str) -> list[Path]:
    """The component's parts: ores.<slug>/<part> plus the shell adapter.

    A catalogue component is one directory; a composite has several. The
    shell adapter lives under a different project name and is part of the
    component's user-facing surface, so its tests count too.
    """
    dirs = []
    base = REPO_ROOT / "projects" / f"ores.{slug}"
    if base.is_dir():
        dirs.extend(sorted(p for p in base.iterdir() if (p / "src").is_dir()))
    shell = REPO_ROOT / "projects" / "ores.shell" / slug
    if shell.is_dir():
        dirs.append(shell)
    return dirs


def test_sources(dirs: list[Path]) -> list[Path]:
    out: list[Path] = []
    for d in dirs:
        tests = d / "tests"
        if tests.is_dir():
            out.extend(sorted(tests.rglob("*.cpp")))
    return out


def body_of(lines: list[str], start: int) -> str:
    """The braced body of the TEST_CASE that starts at ``start``."""
    depth = 0
    began = False
    out: list[str] = []
    for line in lines[start:]:
        depth += line.count("{") - line.count("}")
        if "{" in line:
            began = True
        out.append(line)
        if began and depth <= 0:
            break
    return "".join(out)


def classify(body: str) -> tuple[str, list[str]]:
    """(verdict, the assertions that carried it)."""
    checks = re.findall(r"(CHECK[A-Z_]*|REQUIRE[A-Z_]*)\((.*?)\);", body, re.S)
    if not checks:
        return "NO_ASSERTION", []
    assigned = set(re.findall(r"\b([A-Za-z_]\w*)\s*=", body))
    weak: list[str] = []
    strong: list[str] = []
    for kind, arg in checks:
        a = " ".join(arg.split())
        if kind in ("CHECK_NOTHROW", "REQUIRE_NOTHROW"):
            weak.append(f"{kind}({a})")
        elif (SIZE_ONLY.search(a) or EMPTY_ONLY.match(a)
              or kind in ("CHECK_FALSE", "REQUIRE_FALSE")):
            weak.append(f"{kind}({a})")
        elif TRUTHY_ONLY.match(a):
            (strong if a.lstrip("!") in assigned else weak).append(f"{kind}({a})")
        else:
            strong.append(f"{kind}({a})")
    return ("VACUOUS" if not strong else "OK"), (weak if not strong else strong)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--component", required=True,
                    help="component slug, e.g. dq, iam, refdata")
    ap.add_argument("--quiet", action="store_true",
                    help="print the summary only")
    args = ap.parse_args()

    dirs = component_dirs(args.component)
    if not dirs:
        print(f"no component directories found for {args.component!r}", file=sys.stderr)
        return 2

    sources = test_sources(dirs)
    total = 0
    findings: list[tuple[str, str, list[str]]] = []
    for path in sources:
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines(keepends=True)
        for i, line in enumerate(lines):
            m = TEST_CASE_RE.search(line)
            if not m:
                continue
            total += 1
            verdict, detail = classify(body_of(lines, i))
            if verdict != "OK":
                findings.append((str(path.relative_to(REPO_ROOT)), m.group(1), detail))

    print(f"# Vacuous-test survey: {args.component}")
    print()
    print(f"{total} test case(s) in {len(sources)} source(s); {len(findings)} with no "
          f"strong assertion.")
    print()
    if args.quiet or not findings:
        return 0

    per_file: dict[str, int] = {}
    for rel, _, _ in findings:
        per_file[rel] = per_file.get(rel, 0) + 1
    print("| Test source | Cases |")
    print("|-------------+-------|")
    for rel, n in sorted(per_file.items(), key=lambda kv: (-kv[1], kv[0])):
        print(f"| ={rel}= | {n} |")
    print()
    print("| Case | Only asserts |")
    print("|------+--------------|")
    for rel, name, detail in findings:
        shown = "; ".join(f"={d}=" for d in detail[:3]) or "(nothing)"
        print(f"| ={rel}=::{name} | {shown} |")
    return 0


if __name__ == "__main__":
    sys.exit(main())
