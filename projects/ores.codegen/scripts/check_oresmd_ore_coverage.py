#!/usr/bin/env python3
"""Every ORE market data type is represented in oresmd, or recorded here.

ORE's own shape table defines the market data types the project supports:
one row per series type in
``projects/ores.sql/populate/ore/ore_series_key_shapes_populate.sql``.
oresmd is the native representation, and its quote types are declared in
the models under ``projects/ores.marketdata/modeling/oresmd/``.

This gate exists because the two lists were never compared. oresmd's
quote-type models were written from ORE's documentation, so a type can be
absent from them without anything noticing, and a type oresmd *does*
declare can be unable to express the keys ORE actually writes. The gap is
invisible until someone reads a real file.

The check is a set comparison, and it fails in both directions:

- an ORE type with no oresmd quote type and no entry in
  ``UNREPRESENTED`` is an unrecorded gap;
- an entry in ``UNREPRESENTED`` whose type is now modelled, or is no
  longer an ORE type, is a stale exception.

An exception earns its place only with the reason the type cannot simply
be modelled. Removing entries is the point of the list.

``--corpus`` additionally walks the ORE example corpus committed at
``external/ore/examples/`` and reports, per type, how many key lines
project to an oresmd identifier. That is the measurement behind the
exception list: a type can be modelled and still not cover the data, and
that is a different defect with a different fix.

Read-only. Usage::

    projects/ores.codegen/venv/bin/python scripts/check_oresmd_ore_coverage.py
    projects/ores.codegen/venv/bin/python scripts/check_oresmd_ore_coverage.py --corpus
"""
from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import load_org_oresmd_quote_type_model  # noqa: E402

SHAPES_SQL = REPO_ROOT / "projects/ores.sql/populate/ore/ore_series_key_shapes_populate.sql"
ORESMD_MODELING = REPO_ROOT / "projects/ores.marketdata/modeling/oresmd"
CORPUS = REPO_ROOT / "external/ore/examples"

# ORE series types with no oresmd quote type, and why.
#
# Every entry is a type ORE writes that oresmd cannot yet name. The reason
# states what the representation would have to become, so the entry can be
# acted on rather than merely tolerated.
UNREPRESENTED: dict[str, str] = {
    "BOND": (
        "Single-name bond price and yield spread. Needs a bond asset class: "
        "the identifier carries an instrument id rather than a currency, and "
        "the key has no currency, tenor or point dimension."
    ),
    "COMMODITY_OPTION": (
        "Commodity option log-normal volatility, 6, 7 and 9 segments, with "
        "delta and forward coordinates. Needs the volatility surface point "
        "the equity and FX option families also need."
    ),
    "FIXING": (
        "Index fixings. The key is TYPE/METRIC/INDEX_NAME with no currency "
        "and no point, so it is a different key space from every quote type "
        "rather than a missing member of one. The identity-core work wires "
        "this boundary; see the fixing projection task."
    ),
    "GENERIC-MD": (
        "A wrapper whose inner type and metric name the series and whose "
        "remaining segments are the point. Representing it means "
        "representing an open set of inner types, which is a grammar change "
        "rather than an added quote type."
    ),
    "INDEX_CDS_OPTION": (
        "Index CDS option log-normal volatility, keyed by index, expiry and "
        "strike. Needs the volatility surface point."
    ),
    "RATING": (
        "Rating transition probabilities, keyed by provider, from-rating and "
        "to-rating. Needs a rating asset class whose coordinate is a rating "
        "pair, which no existing asset class carries."
    ),
    "SHAPE_PROFILE": (
        "Shape factor profiles, keyed by profile name, date, second-of-day "
        "and a period code. Needs a profile asset class with a time-of-day "
        "coordinate."
    ),
}

# Quote types oresmd models whose shape does not match the keys ORE writes.
# These are model defects rather than coverage gaps: the type is declared,
# and the declaration is wrong. Recorded here so the corpus measurement can
# name them; the fix is the model.
SHAPE_MISMATCH: dict[str, str] = {
    "CPR": "real key is CPR/RATE/ISIN:<isin>, with no currency",
    "MM": "the 5-segment form ccy/settle/tenor now projects; the 6-segment form still fails where the producer spells the index ESTER, which index_family calls estr",
}

# ORE types oresmd represents through a volatility surface point rather than
# a quote-type row, and the asset class that carries them. The projection
# library dispatches these explicitly (see ``from_ir_swaption`` in
# oresmd_projections.cpp), so they do not appear in a Quote types table.
VOL_REPRESENTED: dict[str, str] = {
    "SWAPTION": "ir",
    "FX_OPTION": "fx",
    "EQUITY_OPTION": "equity",
    "CAPFLOOR": "ir",
    "ZC_INFLATIONCAPFLOOR": "inflation",
    "YY_INFLATIONCAPFLOOR": "inflation",
    "BOND_OPTION": "ir",
}

# An asset class whose model declares a ``vol`` field but whose projection
# library implements no volatility surface, so the field is a promise
# nothing keeps. Recorded rather than silently ignored; the fix is the
# projection work for that asset class.
VOL_DECLARED_UNWIRED: dict[str, str] = {
    "commodity": "COMMODITY_OPTION has no inverse projection",
    "credit": "INDEX_CDS_OPTION has no inverse projection",
}

# Modelled quote types with no projection test, because pinning the key the
# projection currently emits would enshrine the defect the shape record
# already names. Keyed by ``asset_class.enum_name``.
TEST_COVERAGE_EXEMPT: dict[str, str] = {
    "commodity.cpr": (
        "CPR is modelled as a ccy-keyed curve, and ORE writes "
        "CPR/RATE/ISIN:<isin> with no currency and no point. There is no "
        "correct key to assert until SHAPE_MISMATCH's CPR entry is fixed, "
        "and a test of the current key would pin the wrong shape."
    ),
}

# Requirement: every modelled quote type carries both a projection test and
# a round-trip test. A type with only one of the two is declared but
# unexercised in one direction, which is how a projection that emits a key
# its own inverse cannot read survives a green suite.
_REQUIRED_TEST_KINDS = ("projections", "round_trip")


def quote_type_test_coverage() -> dict[str, tuple[str, int, int]]:
    """``asset_class.enum_name`` -> (ore_type, projection cases, round trips)."""
    model = load_org_oresmd_quote_type_model(ORESMD_MODELING / "model.org")
    out: dict[str, tuple[str, int, int]] = {}
    for spec in model.get("oresmd_quote_types") or []:
        asset_class = spec.get("asset_class", "")
        cases = spec.get("test_cases") or {}
        for qt in spec.get("quote_types") or []:
            enum_name = (qt.get("enum_name") or "").strip()
            ore_type = (qt.get("ore_type") or "").strip()
            projections = round_trips = 0
            for kind, rows in cases.items():
                for row in rows:
                    uri = row.get("uri") or ""
                    expected = row.get("expected") or ""
                    if kind.startswith("projections") and (
                            f"quote={enum_name}" in uri
                            or expected.startswith(ore_type + "/")):
                        projections += 1
                    if kind.startswith("round_trip") and f"quote={enum_name}" in uri:
                        round_trips += 1
            out[f"{asset_class}.{enum_name}"] = (ore_type, projections, round_trips)
    return out


def ore_types() -> list[str]:
    """The ORE series types the shape table defines."""
    text = SHAPES_SQL.read_text(encoding="utf-8")
    return sorted(set(re.findall(r"tenant_id_fn\(\), '([A-Z_0-9-]+)'", text)))


def oresmd_ore_types() -> dict[str, str]:
    """ORESMD quote type -> ORE type, for every modelled quote type."""
    model = load_org_oresmd_quote_type_model(ORESMD_MODELING / "model.org")
    specs = model.get("oresmd_quote_types") or []
    out: dict[str, str] = {}
    for spec in specs:
        asset_class = spec.get("asset_class", "")
        for qt in spec.get("quote_types") or []:
            ore_type = (qt.get("ore_type") or "").strip()
            enum_name = (qt.get("enum_name") or "").strip()
            if ore_type:
                out[f"{asset_class}.{enum_name}"] = ore_type
    return out


def asset_classes_declaring_vol() -> list[str]:
    """Asset classes whose model declares a ``vol`` field."""
    model = load_org_oresmd_quote_type_model(ORESMD_MODELING / "model.org")
    specs = model.get("oresmd_quote_types") or []
    out = []
    for spec in specs:
        names = {f.get("name") for f in spec.get("fields") or []}
        if "vol" in names:
            out.append(spec.get("asset_class", ""))
    return sorted(a for a in out if a)


def tokenize(line: str):
    """(date, key, value), or None for a line in neither format."""
    line = line.lstrip(" \t\r")
    c1 = line.find(",")
    if c1 != -1:
        c2 = line.find(",", c1 + 1)
        if c2 == -1:
            return None
        return line[:c1], line[c1 + 1 : c2], line[c2 + 1 :].strip()
    parts = line.split(None, 2)
    if len(parts) < 3:
        return None
    return parts[0], parts[1], parts[2].strip()


def corpus_line_counts() -> dict[str, int]:
    """ORE series type -> key lines the corpus carries for it."""
    files = subprocess.run(
        ["git", "ls-files", "external/ore/examples"],
        cwd=REPO_ROOT, capture_output=True, text=True, check=True,
    ).stdout.split()
    counts: dict[str, int] = {}
    for rel in files:
        name = rel.rsplit("/", 1)[-1]
        if "market" not in name.lower() or "fixing" in name.lower():
            continue
        if not re.search(r"\.(txt|csv)$", name):
            continue
        try:
            with open(REPO_ROOT / rel, encoding="utf-8", errors="replace") as fh:
                for raw in fh:
                    s = raw.strip()
                    if not s or s.startswith("#"):
                        continue
                    row = tokenize(raw)
                    if row is None:
                        continue
                    series_type = row[1].split("/")[0]
                    counts[series_type] = counts.get(series_type, 0) + 1
        except OSError:
            continue
    return counts


def coverage_problems() -> list[str]:
    """Everything wrong with the coverage record, one line per problem.

    Two directions, because the record is only useful if it tracks the
    work: an ORE type that is neither represented nor recorded is a gap
    nobody wrote down, and a record entry that no longer applies is a
    claim the tree has outgrown.
    """
    declared = ore_types()
    modelled = oresmd_ore_types()
    modelled_types = set(modelled.values()) | set(VOL_REPRESENTED)
    uncovered = [t for t in declared if t not in modelled_types]

    problems: list[str] = []

    for t in uncovered:
        if t not in UNREPRESENTED:
            problems.append(
                f"{t}: an ORE series type with no oresmd quote type and no "
                f"entry in UNREPRESENTED"
            )

    for t in UNREPRESENTED:
        if t not in uncovered:
            why = ("it is now modelled" if t in modelled_types
                   else "it is not an ORE series type")
            problems.append(f"{t}: UNREPRESENTED entry is stale because {why}")

    # The shape verdict is a human judgement -- a key either matches the
    # corpus or it does not, and no set comparison decides that. What the
    # check can do is refuse an entry for a type that is not modelled at
    # all, which is a coverage gap wearing the wrong record.
    for t in SHAPE_MISMATCH:
        if t not in modelled_types:
            problems.append(
                f"{t}: SHAPE_MISMATCH entry for a type oresmd does not model; "
                f"it belongs in UNREPRESENTED"
            )

    # A model that declares a volatility surface field promises a
    # representation the projection library must implement. Each asset class
    # is either wired or recorded, never both and never neither.
    vol_asset_classes = asset_classes_declaring_vol()
    wired = set(VOL_REPRESENTED.values())
    for asset_class in vol_asset_classes:
        if asset_class in wired and asset_class in VOL_DECLARED_UNWIRED:
            problems.append(
                f"{asset_class}: declares a vol field and is both wired and "
                f"recorded as unwired"
            )
        elif asset_class not in wired and asset_class not in VOL_DECLARED_UNWIRED:
            problems.append(
                f"{asset_class}: declares a vol field with no inverse "
                f"projection and no entry in VOL_DECLARED_UNWIRED"
            )
    for asset_class in VOL_DECLARED_UNWIRED:
        if asset_class not in vol_asset_classes:
            problems.append(
                f"{asset_class}: VOL_DECLARED_UNWIRED entry is stale because "
                f"the model declares no vol field"
            )

    # Every modelled quote type proves itself in both directions, or says
    # why it cannot.
    coverage = quote_type_test_coverage()
    for key, (ore_type, projections, round_trips) in sorted(coverage.items()):
        missing = [k for k, n in (("projection", projections),
                                  ("round-trip", round_trips)) if not n]
        if not missing:
            continue
        if key in TEST_COVERAGE_EXEMPT:
            continue
        problems.append(
            f"{key} ({ore_type}): no {' and no '.join(missing)} test"
        )
    for key in TEST_COVERAGE_EXEMPT:
        if key not in coverage:
            problems.append(
                f"{key}: TEST_COVERAGE_EXEMPT entry names no modelled quote type"
            )
        else:
            _, projections, round_trips = coverage[key]
            if projections and round_trips:
                problems.append(
                    f"{key}: TEST_COVERAGE_EXEMPT entry is stale because the "
                    f"type now has both tests"
                )

    return problems


def main_with_args(corpus: bool) -> int:
    """The check, with the corpus walk optional. Returns a process code."""
    declared = ore_types()
    modelled = oresmd_ore_types()
    modelled_types = set(modelled.values()) | set(VOL_REPRESENTED)
    covered = [t for t in declared if t in modelled_types]
    vol_asset_classes = asset_classes_declaring_vol()
    wired = set(VOL_REPRESENTED.values())

    problems = coverage_problems()

    print(f"ORE series types:              {len(declared)}")
    print(f"oresmd quote types:            {len(modelled)} "
          f"({len(set(modelled.values()))} distinct ORE types)")
    print(f"volatility-represented types:  {len(VOL_REPRESENTED)}")
    print(f"represented:                   {len(covered)}")
    print(f"unrepresented, recorded:       {len(UNREPRESENTED)}")
    print(f"modelled with a shape mismatch: {len(SHAPE_MISMATCH)}")
    print(f"asset classes declaring vol:   {len(vol_asset_classes)} "
          f"({len(wired & set(vol_asset_classes))} wired)")

    if corpus:
        counts = corpus_line_counts()
        print()
        print(f"{'ORE type':<24}{'lines':>10}  status")
        print("-" * 68)
        total = sum(counts.values())
        on_modelled = 0
        for t in sorted(counts, key=lambda k: -counts[k]):
            if t in SHAPE_MISMATCH:
                status = "modelled, shape mismatch"
            elif t in modelled_types:
                status = "modelled"
                on_modelled += counts[t]
            elif t in UNREPRESENTED:
                status = "not represented"
            else:
                status = "not an ORE series type"
            print(f"{t:<24}{counts[t]:>10}  {status}")
        print("-" * 68)
        print(f"{'total':<24}{total:>10}")
        if total:
            print(f"lines on modelled types: {on_modelled} "
                  f"({100.0 * on_modelled / total:.1f}%)")

    print()
    if problems:
        print(f"FAIL: {len(problems)} coverage record problem(s):")
        for p in problems:
            print(f"  {p}")
        return 1

    print("Every ORE series type is represented in oresmd or recorded with a "
          "reason.")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--corpus", action="store_true",
        help="also walk external/ore/examples and report lines per type",
    )
    args = parser.parse_args()
    return main_with_args(corpus=args.corpus)


if __name__ == "__main__":
    raise SystemExit(main())
