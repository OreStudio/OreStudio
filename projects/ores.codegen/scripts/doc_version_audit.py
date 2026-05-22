#!/usr/bin/env python3
"""
Audit and stamp #+version: org-mode markers across all .org files in the repo.

Every .org file in scope (excluding build/.packages/vcpkg/external/.git)
carries one of:
  #+version: 1   — legacy v1 doc, pending migration to v2
  #+version: 2   — current v2 doc (also has #+type:)

A file is classified v2 if it has #+type:, otherwise v1. Every org file is
a doc — there is no "not a doc" exception.

Modes:
  (default)   Print classification summary table.
  --list-v1   Print paths of remaining v1 docs (one per line).
  --check     Exit non-zero if any .org file is missing #+version:.
  --stamp     Insert missing #+version: markers in-place. Idempotent.

Always operates on the repo root inferred from this script's location.
"""

import argparse
import re
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[3]

# Directories never walked.
EXCLUDED_DIRS = {"build", ".packages", "vcpkg", "external", ".git"}

VERSION_RE   = re.compile(r"^#\+version:\s*([12])\s*$", re.MULTILINE | re.IGNORECASE)
TYPE_RE      = re.compile(r"^#\+type:\s*\S+",           re.MULTILINE | re.IGNORECASE)
TITLE_RE     = re.compile(r"^#\+title:",                re.MULTILINE | re.IGNORECASE)
PROP_END_RE  = re.compile(r"^:END:\s*$",                re.MULTILINE | re.IGNORECASE)

# Classification labels.
V2_TAGGED    = "v2-tagged"
V2_UNTAGGED  = "v2-untagged"
V1_TAGGED    = "v1-tagged"
V1_UNTAGGED  = "v1-untagged"


def find_org_files(root: Path):
    for path in sorted(root.rglob("*.org")):
        rel_parts = path.relative_to(root).parts
        if any(part in EXCLUDED_DIRS for part in rel_parts):
            continue
        yield path


def classify(text: str) -> str:
    version_match = VERSION_RE.search(text)
    if version_match:
        return V2_TAGGED if version_match.group(1) == "2" else V1_TAGGED
    if TYPE_RE.search(text):
        return V2_UNTAGGED
    return V1_UNTAGGED


def _insertion_point(text: str) -> tuple[int, int]:
    """
    Decide (version, line-end-offset) for where to splice #+version:.

    Anchor priority:
      1. #+type:  → v2, after that line
      2. #+title: → v1, after that line
      3. :END:    → v1, after the :PROPERTIES: block
      4. fallback → v1, at the top of the file
    """
    type_match = TYPE_RE.search(text)
    if type_match:
        return 2, type_match.end()

    title_match = TITLE_RE.search(text)
    if title_match:
        return 1, title_match.end()

    prop_end = PROP_END_RE.search(text)
    if prop_end:
        return 1, prop_end.end()

    return 1, -1  # prepend


def stamp(path: Path, text: str) -> bool:
    """Insert #+version: 1|2 once. Returns True if the file was modified."""
    if VERSION_RE.search(text):
        return False

    version, anchor_end = _insertion_point(text)

    if anchor_end < 0:
        # No anchor — prepend.
        new_text = f"#+version: {version}\n" + text
    else:
        line_end = text.find("\n", anchor_end)
        if line_end == -1:
            new_text = text + f"\n#+version: {version}\n"
        else:
            new_text = (
                text[: line_end + 1]
                + f"#+version: {version}\n"
                + text[line_end + 1 :]
            )

    # Preserve UNIX line endings — never emit CRLF.
    path.write_text(new_text, encoding="utf-8", newline="\n")
    return True


def audit():
    """Return {path: label} for every walked .org file."""
    results: dict[Path, str] = {}
    for path in find_org_files(REPO_ROOT):
        try:
            text = path.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            text = path.read_text(encoding="utf-8", errors="replace")
        results[path] = classify(text)
    return results


def cmd_summary(results):
    counts: dict[str, int] = {}
    for label in results.values():
        counts[label] = counts.get(label, 0) + 1

    v1 = counts.get(V1_TAGGED, 0)   + counts.get(V1_UNTAGGED, 0)
    v2 = counts.get(V2_TAGGED, 0)   + counts.get(V2_UNTAGGED, 0)
    docs = v1 + v2
    total = len(results)
    untagged = counts.get(V1_UNTAGGED, 0) + counts.get(V2_UNTAGGED, 0)

    print(f"Doc version audit — {total} .org files under {REPO_ROOT}")
    print()
    print(f"  v2-tagged       {counts.get(V2_TAGGED,    0):>5}")
    print(f"  v2-untagged     {counts.get(V2_UNTAGGED,  0):>5}")
    print(f"  v1-tagged       {counts.get(V1_TAGGED,    0):>5}")
    print(f"  v1-untagged     {counts.get(V1_UNTAGGED,  0):>5}")
    print()
    print(f"  Docs (v1+v2)    {docs:>5}")
    if docs:
        print(f"  v2 / docs        {100 * v2 / docs:>4.1f}%")
        print(f"  v1 remaining    {v1:>5}")
    print(f"  Untagged docs   {untagged:>5}  (need stamping)")
    return 0


def cmd_list_v1(results):
    for path, label in sorted(results.items()):
        if label in (V1_TAGGED, V1_UNTAGGED):
            print(path.relative_to(REPO_ROOT))
    return 0


def cmd_check(results):
    untagged = [
        path for path, label in sorted(results.items())
        if label in (V1_UNTAGGED, V2_UNTAGGED)
    ]
    if untagged:
        print(f"{len(untagged)} doc-shaped .org file(s) are missing #+version:", file=sys.stderr)
        for path in untagged:
            print(f"  {path.relative_to(REPO_ROOT)}", file=sys.stderr)
        print("\nRun: python projects/ores.codegen/scripts/doc_version_audit.py --stamp", file=sys.stderr)
        return 1
    return 0


def cmd_stamp():
    stamped = 0
    skipped = 0
    for path in find_org_files(REPO_ROOT):
        try:
            text = path.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            text = path.read_text(encoding="utf-8", errors="replace")
        if stamp(path, text):
            stamped += 1
        else:
            skipped += 1
    print(f"Stamped {stamped} file(s); {skipped} skipped (already tagged or not a doc).")
    return 0


def main():
    parser = argparse.ArgumentParser(
        description="Audit and stamp #+version: markers on org documents.",
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--list-v1", action="store_true",
                      help="List paths of remaining v1 docs.")
    mode.add_argument("--check",   action="store_true",
                      help="Exit non-zero if any doc lacks #+version:.")
    mode.add_argument("--stamp",   action="store_true",
                      help="Insert missing #+version: markers in-place.")
    args = parser.parse_args()

    if args.stamp:
        return cmd_stamp()

    results = audit()
    if args.list_v1:
        return cmd_list_v1(results)
    if args.check:
        return cmd_check(results)
    return cmd_summary(results)


if __name__ == "__main__":
    sys.exit(main())
