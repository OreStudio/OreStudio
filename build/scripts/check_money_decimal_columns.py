#!/usr/bin/env python3
"""A money quantity is an exact decimal; a continuous quantity is a float.

Money: rate, spread, notional, amount, price, strike, fee, premium, margin,
barrier, face value. Continuous: volatility, correlation, weight, probability,
recovery rate, tranche fraction, conversion ratio, quantity, CPI,
participation rate, day-count fraction.

Both halves of the rule are checked, because a tree that gets one right and
the other wrong is in the state this rule exists to end: the same economic
quantity typed two ways, and a conversion between them at every seam.

  * A money column is wrong when its SQL type is floating point, or when its
    SQL type is exact (numeric) but its C++ type is a double -- exact at rest
    and lossy in the domain, which is the subtler of the two.
  * A continuous column is wrong when its SQL type is exact. Rounding a
    volatility, a weight or a day-count fraction to twelve places states a
    precision the quantity does not have, and ORE's own schema reads those
    leaves as xs:float, so the binary float is the more faithful type.

The rule is stated in
doc/knowledge/architecture/exact_numbers_and_economic_change.org and checked
by the Regime 2 criteria in data_oriented_design.org.

Offenders that existed when the rule was written live in a baseline beside
this script, one per direction, so the debt stays visible and the check fails
only on one that is not there. Regenerate with --write-baseline or
--write-continuous-baseline after deliberately fixing some.

Usage:
  python3 build/scripts/check_money_decimal_columns.py [--write-baseline
      | --write-continuous-baseline]

Exit codes:
  0 -- no offender outside its baseline
  1 -- an offender is not in its baseline
"""

import argparse
import re
import sys
from pathlib import Path

MONEY_TOKENS = frozenset({
    "rate", "spread", "notional", "amount", "price", "strike", "fee",
    "premium", "margin", "barrier", "face", "yield",
})

# Columns whose name says money and whose economics do not: a variance strike
# is volatility, a vol config's strike factor is a multiplier, and a synthetic
# generator's price is a model parameter. Keyed on the column rather than the
# file, because excluding the file would hide every later column added to it.
EXCLUDED_COLUMNS = frozenset({
    "ores.trading.fx_variance_swap_instrument::strike",
    "ores.trading.equity_variance_swap_instrument::strike",
    "ores.refdata.bond_future_volatility_config::strike_factor",
    "ores.refdata.cds_volatility_config::strike_factor",
    "ores.dq.synthetic_fx_spot_config::gmm_initial_price",
    "ores.synthetic.fx_spot_generation_config::gmm_initial_price",
})

# A token here wins over a money token, so a rate that is really a fraction is
# not misread. recovery_rate and participation_rate carry "rate"; a variance
# strike carries "strike".
CONTINUOUS_TOKENS = frozenset({
    "variance", "recovery", "participation", "ratio", "quantity", "weight",
    "probability", "correlation", "vol", "volatility", "cpi", "fraction",
    "count", "days", "day", "tenor", "period",
})

FLOAT_TYPES = frozenset({"double precision", "real", "float", "double"})

COLUMN = re.compile(r"^\*\* (\S+)\s*$")
TYPE = re.compile(r"^\s*:type:\s*(\S.*?)\s*$")
CPP_TYPE = re.compile(r"^\s*:cpp_type:\s*(\S.*?)\s*$")

# A junction declares its two columns under "* Left" and "* Right" with a
# :column: key rather than as "** name" blocks, so a scan that only knows
# headings skips a whole entity kind.
COLUMN_KEY = re.compile(r"^\s*:column:\s*(\S+)\s*$")
SECTION = re.compile(r"^\* [^*\s].*$")

BASELINE = Path(__file__).with_suffix("").with_name(
    "money_decimal_columns.baseline")
CONTINUOUS_BASELINE = Path(__file__).with_suffix("").with_name(
    "continuous_decimal_columns.baseline")


def classify(name: str) -> str:
    """Returns 'money', 'continuous' or 'other' for a column name."""
    tokens = name.lower().split("_")
    if any(t in CONTINUOUS_TOKENS for t in tokens):
        return "continuous"
    if any(t in MONEY_TOKENS for t in tokens):
        return "money"
    return "other"


# A run that scans implausibly few columns has stopped understanding the
# models and would pass vacuously -- the failure this check exists to catch,
# reproduced by the check itself. The tree holds several thousand columns, so
# the floor sits far below that and far above what a broken parse returns.
MIN_COLUMNS = 1000


def columns(root: Path):
    """Yields (path, line, column, sql_type, cpp_type) for every column.

    A field group is yielded too, with no SQL type of its own: it declares no
    column until an entity embeds it, and its members are checked on the same
    rule for that reason.
    """
    # A modelling directory sits at projects/<component>/modeling and also at
    # projects/<component>/<sub>/modeling, so the glob has to descend. The
    # one-level pattern it replaced matched 535 files where this matches 711,
    # so 176 model files were invisible to it.
    for path in sorted(root.glob("projects/**/modeling/*.org")):
        name = None
        sql_type = None
        cpp_type = None
        line_no = 0
        for i, raw in enumerate(
                path.read_text(encoding="utf-8").splitlines(), start=1):
            m = COLUMN.match(raw)
            if m:
                if name:
                    yield path, line_no, name, sql_type, cpp_type
                name, sql_type, cpp_type, line_no = m.group(1), None, None, i
                continue
            if SECTION.match(raw):
                # A section heading ends the current column, which may be the
                # last one in the file: flush it before clearing, or a field
                # group's final member is never examined.
                if name:
                    yield path, line_no, name, sql_type, cpp_type
                name, sql_type, cpp_type, line_no = None, None, None, 0
                continue
            k = COLUMN_KEY.match(raw)
            if k and name is None:
                name, sql_type, cpp_type, line_no = k.group(1), None, None, i
                continue
            if name is None:
                continue
            t = TYPE.match(raw)
            if t and sql_type is None:
                sql_type = t.group(1)
            c = CPP_TYPE.match(raw)
            if c and cpp_type is None:
                cpp_type = c.group(1)
        if name:
            yield path, line_no, name, sql_type, cpp_type


def base_type(sql_type: str) -> str:
    return sql_type.split("(")[0].strip().lower()


def money_offenders(root: Path):
    """A money quantity that is not exact in both places."""
    for path, line_no, name, sql_type, cpp_type in columns(root):
        if classify(name) != "money":
            continue
        if f"{path.stem}::{name}" in EXCLUDED_COLUMNS:
            continue
        cpp = cpp_type or ""
        if sql_type is None:
            # A field group's members become the embedding entity's columns,
            # so a money member must be exact for the same reason.
            if path.stem.endswith("_field_group") and "double" in cpp:
                yield path, line_no, name, "(embedded)", cpp
            continue
        if base_type(sql_type) in FLOAT_TYPES or \
                (base_type(sql_type) == "numeric" and "double" in cpp):
            yield path, line_no, name, sql_type, cpp


def continuous_offenders(root: Path):
    """A continuous quantity stated with an exact type it does not have."""
    for path, line_no, name, sql_type, cpp_type in columns(root):
        if classify(name) != "continuous":
            continue
        cpp = cpp_type or ""
        if sql_type is None:
            if path.stem.endswith("_field_group") and "decimal" in cpp:
                yield path, line_no, name, "(embedded)", cpp
            continue
        if base_type(sql_type) == "numeric":
            yield path, line_no, name, sql_type, cpp


def key(path: Path, line_no: int, name: str, sql_type: str, cpp_type: str):
    return f"{path.as_posix()}::{name}"


def read_baseline(path: Path) -> set[str]:
    if not path.exists():
        return set()
    return {
        line.strip()
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip()
    }


def report(label: str, found: list, baseline: Path) -> bool:
    """Prints one direction's offenders. Returns True when it is clean."""
    keys = [key(*o) for o in found]
    known = read_baseline(baseline)
    new = [k for k in keys if k not in known]
    fixed = sorted(known - set(keys))

    for path, line_no, name, sql_type, cpp_type in found:
        print(f"{path.as_posix()}:{line_no}: {name} "
              f"sql={sql_type} cpp={cpp_type or '-'}")

    print(f"\n{label}: {len(found)} (known {len(found) - len(new)}, "
          f"new {len(new)})")
    if fixed:
        print(f"fixed since the baseline was written: {len(fixed)}")
        for k in fixed:
            print(f"  {k}  (remove it from {baseline.name})")
    if new:
        print(f"\nNEW {label}, which this check exists to prevent:")
        for k in new:
            print(f"  {k}")
    return not new


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write-baseline", action="store_true")
    parser.add_argument("--write-continuous-baseline", action="store_true")
    parser.add_argument("--root", type=Path, default=Path("."))
    parser.add_argument(
        "--min-columns", type=int, default=MIN_COLUMNS,
        help="vacuity floor; lower it only when running against a subtree")
    args = parser.parse_args()

    read = sum(1 for _ in columns(args.root))
    if read < args.min_columns:
        print(f"ERROR: read only {read} columns, expected at least "
              f"{args.min_columns}. The model format may have changed, and "
              f"this check would otherwise pass without looking at anything.")
        return 1

    money = sorted(money_offenders(args.root),
                   key=lambda o: (o[0].as_posix(), o[2]))
    continuous = sorted(continuous_offenders(args.root),
                        key=lambda o: (o[0].as_posix(), o[2]))

    if args.write_baseline:
        BASELINE.write_text("\n".join(key(*o) for o in money) + "\n",
                            encoding="utf-8")
        print(f"wrote {len(money)} entries to {BASELINE.as_posix()}")
        return 0
    if args.write_continuous_baseline:
        CONTINUOUS_BASELINE.write_text(
            "\n".join(key(*o) for o in continuous) + "\n", encoding="utf-8")
        print(f"wrote {len(continuous)} entries to "
              f"{CONTINUOUS_BASELINE.as_posix()}")
        return 0

    clean = report("money columns not exact", money, BASELINE)
    clean &= report("continuous columns typed exact", continuous,
                    CONTINUOUS_BASELINE)
    print(f"\ncolumns read: {read} (floor {args.min_columns})")
    return 0 if clean else 1


if __name__ == "__main__":
    sys.exit(main())
