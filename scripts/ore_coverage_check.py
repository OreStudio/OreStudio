#!/usr/bin/env python3
"""
ore_coverage_check.py — Thing 2: ORE coverage gap check.

Compares the ORES golden dataset (assets/test_data/golden_dataset/) against
the upstream ORE example trades (external/ore/examples/Products/Example_Trades/).

Reports:
  - Trade files in the ORE examples not yet present in the golden dataset.
  - XML elements present in ORE example files but absent from the corresponding
    ORES golden file (field-level coverage gaps).

Usage:
  python3 scripts/ore_coverage_check.py [--repo-root <path>] [--strict]

  --strict: exit with code 1 if any gaps are found (used in CI once coverage
            is complete).

Exit codes:
  0 — no gaps (or non-strict mode)
  1 — gaps found and --strict was specified
"""

import argparse
import sys
import xml.etree.ElementTree as ET
from pathlib import Path


def collect_element_names(tree_root) -> set:
    """Recursively collect all element tag names in an XML tree."""
    names = set()
    for elem in tree_root.iter():
        # Strip namespace prefix if any
        tag = elem.tag
        if "}" in tag:
            tag = tag.split("}", 1)[1]
        names.add(tag)
    return names


def parse_xml_safe(path: Path):
    """Parse an XML file, returning (root, None) or (None, error_message)."""
    try:
        tree = ET.parse(path)
        return tree.getroot(), None
    except ET.ParseError as e:
        return None, str(e)


def check_coverage(repo_root: Path, strict: bool) -> int:
    ore_examples = repo_root / "external" / "ore" / "examples" / \
                   "Products" / "Example_Trades"
    golden_dir = repo_root / "assets" / "test_data" / "golden_dataset" / \
                 "Products" / "Example_Trades"

    if not ore_examples.exists():
        print(f"ERROR: ORE examples directory not found: {ore_examples}",
              file=sys.stderr)
        return 1

    ore_files = sorted(ore_examples.glob("*.xml"))
    if not ore_files:
        print("WARNING: No ORE example files found.", file=sys.stderr)
        return 0

    missing_golden = []
    field_gaps = {}  # filename -> set of missing elements
    parse_errors = []

    for ore_file in ore_files:
        golden_file = golden_dir / ore_file.name
        if not golden_file.exists():
            missing_golden.append(ore_file.name)
            continue

        ore_root, err = parse_xml_safe(ore_file)
        if err:
            parse_errors.append((ore_file.name, f"ORE parse error: {err}"))
            continue

        golden_root, err = parse_xml_safe(golden_file)
        if err:
            parse_errors.append((ore_file.name, f"Golden parse error: {err}"))
            continue

        ore_elements = collect_element_names(ore_root)
        golden_elements = collect_element_names(golden_root)
        gaps = ore_elements - golden_elements
        if gaps:
            field_gaps[ore_file.name] = gaps

    # Report
    total_ore = len(ore_files)
    total_covered = total_ore - len(missing_golden)
    coverage_pct = (total_covered / total_ore * 100) if total_ore > 0 else 0

    print(f"\n=== ORE Coverage Report ===")
    print(f"ORE example files:   {total_ore}")
    print(f"Golden files present: {total_covered} ({coverage_pct:.1f}%)")
    print(f"Missing golden files: {len(missing_golden)}")
    print(f"Files with field gaps: {len(field_gaps)}")

    if missing_golden:
        print(f"\n--- Missing golden files ({len(missing_golden)}) ---")
        for f in missing_golden:
            print(f"  MISSING: {f}")

    if field_gaps:
        total_gaps = sum(len(g) for g in field_gaps.values())
        print(f"\n--- Field coverage gaps ({total_gaps} elements across "
              f"{len(field_gaps)} files) ---")
        for filename, gaps in sorted(field_gaps.items()):
            print(f"  {filename}: {len(gaps)} gap(s)")
            for elem in sorted(gaps):
                print(f"    - {elem}")

    if parse_errors:
        print(f"\n--- Parse errors ({len(parse_errors)}) ---")
        for filename, err in parse_errors:
            print(f"  {filename}: {err}")

    print()

    has_gaps = bool(missing_golden or field_gaps or parse_errors)
    if has_gaps and strict:
        print("STRICT MODE: gaps found — exiting with code 1", file=sys.stderr)
        return 1
    return 0


def main():
    parser = argparse.ArgumentParser(
        description="ORE coverage gap check (Thing 2)")
    parser.add_argument(
        "--repo-root",
        type=Path,
        default=Path(__file__).parent.parent,
        help="Path to the repository root (default: parent of scripts/)")
    parser.add_argument(
        "--strict",
        action="store_true",
        help="Exit with code 1 if any gaps are found")
    args = parser.parse_args()

    return check_coverage(args.repo_root, args.strict)


if __name__ == "__main__":
    sys.exit(main())
