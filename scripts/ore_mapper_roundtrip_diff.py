#!/usr/bin/env python3
"""
ore_mapper_roundtrip_diff.py -- diff the XML we write against the XML we read.

The golden suite compares our output to a stored copy of our own output, so
it catches drift and nothing else. This check compares our output to the ORE
documents it came from and classifies every difference.

Each document yields a multiset of (path, value) pairs: the element path from
the root joined by "/", and the trimmed element text. The comparison is order
blind by construction, so two documents holding the same pairs in a different
order are equal.

A pair found on both sides is no difference. A pair found on one side only is
a difference, and each one is classified:

  numeric     the same path on both sides, holding the same number to within
              the relative tolerance. The ORE schema types these fields as
              xs:float and the example documents state more digits than that
              type holds.
  boolean     the same path on both sides, one stating an empty element and
              the other one of the ORE bool spellings. The schema's bool type
              enumerates Y, YES, TRUE, True, true, 1 and their negatives, and
              the examples spell true as an empty element.
  lost        a pair the source states that our output does not. Always a
              failure: the document we wrote is missing content.
  unexplained any other pair our output states that the source does not. A
              failure: it needs a decision.

Two scopes are reported. The in-scope pairs are the ones the bond programme
gates on: everything under a bond product element, plus the top-level trade
envelope. The out-of-scope pairs are the other instrument families, which this
programme does not own yet. They are counted and listed so the debt stays
visible, and they do not fail the run unless --all-products asks for the whole
corpus.

Usage:
  python3 scripts/ore_mapper_roundtrip_diff.py \\
      --source-dir <dir> --output-dir <dir> [--verbose] [--all-products]

Exit codes:
  0 -- every in-scope document round trips with no lost or unexplained pair
  1 -- an in-scope lost or unexplained pair, or a missing output file
"""

import argparse
import sys
import xml.etree.ElementTree as ET
from collections import Counter
from decimal import Decimal, InvalidOperation
from pathlib import Path

TOLERANCE = Decimal("1e-6")

ORE_TRUE = {"Y", "YES", "TRUE", "True", "true", "1"}

# The ORE product elements of the bond family. A path under one of these is in
# scope for the bond programme, as is the top-level trade envelope.
BOND_PRODUCTS = frozenset({
    "AscotData",
    "BondData",
    "BondFutureData",
    "BondOptionData",
    "BondRepoData",
    "BondTRSData",
    "CallableBondData",
    "ConvertibleBondData",
    "ForwardBondData",
})

TRADE_PREFIX = "/Portfolio/Trade/"
ENVELOPE_PATH = TRADE_PREFIX + "Envelope"


def in_bond_scope(path: str) -> bool:
    """True when the path sits under a bond product or the trade envelope."""
    if path == ENVELOPE_PATH or path.startswith(ENVELOPE_PATH + "/"):
        return True
    if not path.startswith(TRADE_PREFIX):
        return False
    element = path[len(TRADE_PREFIX):].split("/", 1)[0]
    return element in BOND_PRODUCTS


def local_name(tag: str) -> str:
    return tag.split("}", 1)[1] if "}" in tag else tag


def as_number(text: str):
    try:
        return Decimal(text)
    except InvalidOperation:
        return None


def normalise_value(text: str | None) -> str:
    if text is None:
        return ""
    stripped = text.strip()
    number = as_number(stripped)
    if number is None:
        return stripped
    if number == number.to_integral_value():
        return str(number.quantize(Decimal(1)))
    return str(number.normalize())


def pairs_of(root: ET.Element) -> Counter:
    pairs: Counter = Counter()

    def walk(elem: ET.Element, path: str) -> None:
        current = f"{path}/{local_name(elem.tag)}"
        pairs[(current, normalise_value(elem.text))] += 1
        for child in elem:
            walk(child, current)

    walk(root, "")
    return pairs


def group_by_path(pairs: Counter) -> dict:
    by_path: dict = {}
    for (path, value), count in pairs.items():
        by_path.setdefault(path, Counter())[value] += count
    return by_path


def parse(path: Path):
    try:
        return ET.parse(path).getroot(), None
    except ET.ParseError as e:
        return None, str(e)


def close_enough(left: str, right: str):
    a, b = as_number(left), as_number(right)
    if a is None or b is None or b == 0:
        return None
    relative = abs((a - b) / b)
    return relative if relative <= TOLERANCE else None


def is_boolean_pair(left: str, right: str) -> bool:
    return (left == "" and right in ORE_TRUE) or (right == "" and left in ORE_TRUE)


def classify(source_pairs: Counter, output_pairs: Counter):
    """Return the classified differences between two pair multisets."""
    found = {
        "numeric": [],
        "boolean": [],
        "lost": [],
        "unexplained": [],
    }
    worst = Decimal(0)
    source = group_by_path(source_pairs)
    output = group_by_path(output_pairs)

    for path in sorted(set(source) | set(output)):
        missing = list((source.get(path, Counter()) - output.get(path, Counter())).elements())
        extra = list((output.get(path, Counter()) - source.get(path, Counter())).elements())

        for value in missing:
            match = next((c for c in extra if close_enough(value, c) is not None), None)
            if match is not None:
                extra.remove(match)
                worst = max(worst, close_enough(value, match))
                found["numeric"].append((path, value))
            elif extra and is_boolean_pair(value, extra[0]):
                extra.pop(0)
                found["boolean"].append((path, value))
            else:
                found["lost"].append((path, value))

        for value in extra:
            found["unexplained"].append((path, value))

    found["worst_numeric"] = worst
    return found


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", required=True, type=Path)
    parser.add_argument("--output-dir", required=True, type=Path)
    parser.add_argument("--verbose", action="store_true")
    parser.add_argument("--all-products", action="store_true",
                        help="gate on every product, not only the bond scope")
    args = parser.parse_args()

    if not args.source_dir.exists():
        print(f"ERROR: source directory not found: {args.source_dir}", file=sys.stderr)
        return 1

    failures = 0
    documents = 0
    totals: Counter = Counter()
    scoped: Counter = Counter()
    outside: Counter = Counter()
    outside_paths: Counter = Counter()
    worst = Decimal(0)

    for source in sorted(args.source_dir.rglob("*.xml")):
        relative = source.relative_to(args.source_dir)
        target = args.output_dir / relative
        documents += 1

        if not target.exists():
            print(f"MISSING     {relative}")
            failures += 1
            continue

        source_root, source_err = parse(source)
        output_root, output_err = parse(target)
        if source_err or output_err:
            print(f"UNPARSED    {relative}: {source_err or output_err}")
            failures += 1
            continue

        found = classify(pairs_of(source_root), pairs_of(output_root))
        worst = max(worst, found["worst_numeric"])
        for kind in ("numeric", "boolean"):
            totals[kind] += len(found[kind])

        here: Counter = Counter()
        for kind in ("lost", "unexplained"):
            for path, value in found[kind]:
                if in_bond_scope(path):
                    here[kind] += 1
                else:
                    outside[kind] += 1
                    outside_paths[path] += 1
        scoped.update(here)

        differs = bool(here["lost"] or here["unexplained"]) or (
            args.all_products and bool(found["lost"] or found["unexplained"]))

        if differs:
            failures += 1
            shown = found if args.all_products else None
            print(f"DIFFERENT   {relative}: {here['lost']} lost, "
                  f"{here['unexplained']} unexplained in scope")
            for path, value in sorted(found["lost"]):
                if shown is not None or in_bond_scope(path):
                    print(f"              lost:        {path} = {value!r}")
            for path, value in sorted(found["unexplained"]):
                if shown is not None or in_bond_scope(path):
                    print(f"              unexplained: {path} = {value!r}")
        elif args.verbose:
            print(f"OK          {relative}: numeric {len(found['numeric'])}, "
                  f"boolean {len(found['boolean'])}")

    print(f"\nDocuments: {documents}  Failures: {failures}")
    print(f"Classified: numeric {totals['numeric']}, boolean {totals['boolean']}")
    print(f"In scope, bond products and the trade envelope: "
          f"lost {scoped['lost']}, unexplained {scoped['unexplained']}")
    print(f"Out of scope, the other instrument families: "
          f"lost {outside['lost']}, unexplained {outside['unexplained']}")
    if outside_paths:
        print("Out-of-scope paths by pair count:")
        for path, count in outside_paths.most_common(10):
            print(f"  {count:6d}  {path}")
    print(f"Worst relative error on a numeric pair: {worst:.3e} "
          f"(tolerance {TOLERANCE:g})")

    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
