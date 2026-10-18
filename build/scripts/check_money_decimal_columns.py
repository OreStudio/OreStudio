#!/usr/bin/env python3
"""A money quantity is an exact decimal; a continuous quantity is a float.

Money: rate, spread, notional, amount, price, strike, fee, premium, margin,
barrier, face value. Continuous: volatility, correlation, weight, probability,
recovery rate, tranche fraction, conversion ratio, quantity, CPI,
participation rate, day-count fraction.

Both halves of the rule are checked, because a tree that gets one right and
the other wrong is in the state this rule exists to end: the same economic
quantity typed two ways, and a conversion between them at every seam.

  * A money column is wrong when its SQL type is floating point, when its SQL
    type is text, or when its SQL type is exact (numeric) but its C++ type is
    a binary float -- exact at rest and lossy in the domain, which is the
    subtler of the two. A text column holds the quantity as digits the store
    cannot compare or sum, so the money is inexact in both places. A field
    group's money member is held to the same rule: its C++ type must be the
    exact decimal, so a string or a float there is reported.
  * A column is money only when the money word is the head noun of its name.
    `premium_amount` is money; `premium_date`, `premium_currency` and
    `price_type` carry a money word and are a date, a currency and a code. A
    trailing qualifier sits after the quantity without replacing it, so the
    head noun is the token before it: `face_value` and
    `minimum_transfer_amount_pay` are money. Text is where the collisions
    concentrate, so this is what keeps the text case precise rather than
    seventy false positives.
  * A continuous column is wrong when its SQL type is exact. Rounding a
    volatility, a weight or a day-count fraction to twelve places states a
    precision the quantity does not have, and ORE's own schema reads those
    leaves as xs:float, so the binary float is the more faithful type.
  * When the name rule reads a column as money and its economics are
    continuous, the entity that states it decides. =CONTINUOUS_COLUMNS= names
    such a column with its reason, so the column is checked in the continuous
    direction -- reported when it is typed exact or text, silent when it is a
    binary float -- rather than hidden by the money baseline.
  * Two shapes stay outside the rule and are named rather than caught. A
    continuous quantity spelled as text is a code in these models -- a tenor,
    a day-count or a convention -- so text is read as a code, not as a
    continuous value. A continuous column whose name carries no continuous
    token and that has no =CONTINUOUS_COLUMNS= entry is invisible to the name
    rule; adding an entry is the fix when one appears.

The rule is stated in
doc/knowledge/architecture/exact_numbers_and_economic_change.org and checked
by the Regime 2 criteria in data_oriented_design.org.

A column whose name says money but whose economics do not -- a quote, an
identity spelling or a code that carries a money word -- is listed in
=EXCLUDED_COLUMNS= with its reason in that column's model prose, because the
head noun is enough to name the quantity and not enough to say what it holds.
Offenders that existed when the rule was written live in the same baseline,
one per direction, so the debt stays visible and the check fails only on one
that is not there. A baseline that holds only its header records no debt in
that direction, which is a statement the check then holds the tree to.
Regenerate with --write-baseline or --write-continuous-baseline after
deliberately fixing some.

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

# A token that follows the quantity without replacing it, so the head noun is
# the token before it: `face_value` is a face amount, `barrier_level` a
# barrier, `minimum_transfer_amount_pay` the pay side of an amount, and
# `independent_amount_held` an amount that is held.
HEAD_QUALIFIERS = frozenset({"value", "level", "pay", "receive", "held"})

# Columns whose name says money and whose economics do not: a variance strike
# is volatility, a vol config's strike factor is a multiplier, and a synthetic
# generator's price or initial rate is a model parameter. Keyed on the column
# rather than the file, because excluding the file would hide every later
# column added to it.
EXCLUDED_COLUMNS = frozenset({
    "projects/ores.refdata/modeling/"
    "ores.refdata.bond_future_volatility_config.org::strike_factor",
    "projects/ores.refdata/modeling/"
    "ores.refdata.cds_volatility_config.org::strike_factor",
    "projects/ores.synthetic/modeling/"
    "ores.synthetic.fx_spot_generation_config.org::gmm_initial_price",
    "projects/ores.synthetic/modeling/"
    "ores.synthetic.ir_curve_generation_config.org::initial_rate",
})

# A column whose name reads as money but whose quantity is continuous, keyed on
# the column rather than the file so a money column added to the same entity is
# still checked. `variance_strike` needs no entry because the continuous token
# already wins; `strike` on a variance swap is the same quantity under the name
# the FX model chose, so the entity is what makes it continuous.
CONTINUOUS_COLUMNS = {
    "projects/ores.trading/modeling/"
    "ores.trading.fx_variance_swap_instrument.org::strike":
        "a variance strike is volatility, not a money amount",
}

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


def money_name(name: str) -> bool:
    """True when the name's head noun is a money word.

    The head noun is the last token, after any qualifier that trails the
    quantity without naming something else. `premium_amount` is money;
    `premium_date` and `price_type` are not.
    """
    tokens = name.lower().split("_")
    while tokens and tokens[-1] in HEAD_QUALIFIERS:
        tokens.pop()
    return bool(tokens) and tokens[-1] in MONEY_TOKENS


def classify(name: str) -> str:
    """Returns 'money', 'continuous' or 'other' for a column name."""
    tokens = name.lower().split("_")
    if any(t in CONTINUOUS_TOKENS for t in tokens):
        return "continuous"
    if money_name(name):
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
    resolved = root.resolve()
    for absolute in sorted(resolved.glob("projects/**/modeling/*.org")):
        # Keys are the model path under the root, so a baseline means the same
        # thing wherever the tree is checked out.
        path = absolute.relative_to(resolved)
        name = None
        sql_type = None
        cpp_type = None
        line_no = 0
        for i, raw in enumerate(
                absolute.read_text(encoding="utf-8").splitlines(), start=1):
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


def binary_float(cpp_type: str) -> bool:
    """True when the C++ type holds the value in a binary float."""
    return re.search(r"\b(?:double|float)\b", cpp_type or "") is not None


def model_key(path: Path, name: str) -> str:
    """The key the hand-listed sets use: the model path and the column."""
    return f"{path.as_posix()}::{name}"


def quantity_kind(path: Path, name: str) -> str:
    """Returns 'money', 'continuous' or 'other' for a column in its model."""
    if model_key(path, name) in CONTINUOUS_COLUMNS:
        return "continuous"
    return classify(name)


def money_offenders(root: Path):
    """A money quantity that is not exact in both places."""
    for path, line_no, name, sql_type, cpp_type in columns(root):
        if quantity_kind(path, name) != "money":
            continue
        if model_key(path, name) in EXCLUDED_COLUMNS:
            continue
        cpp = cpp_type or ""
        if sql_type is None:
            # A field group's members become the embedding entity's columns,
            # so a money member must be exact for the same reason. Its own
            # declaration is all there is to read, so anything but the exact
            # decimal -- a string, a binary float -- is reported.
            if path.stem.endswith("_field_group") and \
                    (binary_float(cpp) or "string" in cpp):
                yield path, line_no, name, "(embedded)", cpp
            continue
        if base_type(sql_type) in FLOAT_TYPES or \
                base_type(sql_type) == "text" or \
                (base_type(sql_type) == "numeric" and binary_float(cpp)):
            yield path, line_no, name, sql_type, cpp


def continuous_offenders(root: Path):
    """A continuous quantity stated with an exact type it does not have."""
    for path, line_no, name, sql_type, cpp_type in columns(root):
        if quantity_kind(path, name) != "continuous":
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


BASELINE_HEADER = (
    "# One offender per line: <model path>::<column>. The list is the debt\n"
    "# this guard records, and an empty list says the direction has none.\n"
    "# Regenerate with --write-baseline or --write-continuous-baseline.\n"
)


def read_baseline(path: Path) -> set[str]:
    if not path.exists():
        raise FileNotFoundError(
            f"the baseline {path.as_posix()} is missing; a direction with no "
            f"debt still needs its file, or the guard is silently off")
    return {
        line.strip()
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
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
        BASELINE.write_text(
            BASELINE_HEADER + "\n".join(key(*o) for o in money) + "\n",
            encoding="utf-8")
        print(f"wrote {len(money)} entries to {BASELINE.as_posix()}")
        return 0
    if args.write_continuous_baseline:
        CONTINUOUS_BASELINE.write_text(
            BASELINE_HEADER + "\n".join(key(*o) for o in continuous) + "\n",
            encoding="utf-8")
        print(f"wrote {len(continuous)} entries to "
              f"{CONTINUOUS_BASELINE.as_posix()}")
        return 0

    try:
        clean = report("money columns not exact", money, BASELINE)
        clean &= report("continuous columns typed exact", continuous,
                        CONTINUOUS_BASELINE)
    except FileNotFoundError as missing:
        print(f"ERROR: {missing}")
        return 1
    print(f"\ncolumns read: {read} (floor {args.min_columns})")
    return 0 if clean else 1


if __name__ == "__main__":
    sys.exit(main())
