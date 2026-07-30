#!/usr/bin/env python3
"""Tolerant CSV comparison for ORE Output/ files against a reference
ExpectedOutput/ CSV.

Exact byte diff is the wrong tool here: two correct ORE builds on
different compilers/library versions routinely differ in the last 1-2
significant digits of a double (see e.g. curves.csv's discount factors),
and a newer engine version can legitimately add trailing report columns
an older ExpectedOutput reference doesn't have (see flows.csv gaining
EffectiveFloorVolatility/EffectiveCapVolatility/Amount(Base)/
DiscountFactor(Base) between versions). This compares only the columns
present in BOTH files by name, numeric columns within a relative
tolerance, and reports columns present in only one side as informational
rather than a failure.

Usage:
  compare_csv.py <actual.csv> <expected.csv> [--rtol 1e-6]

Exit status: 0 if every shared column's every row matches within
tolerance and row counts agree, 1 otherwise (with a summary of every
mismatch found, not just the first).
"""
import csv
import sys


def is_number(s: str) -> bool:
    try:
        float(s)
        return True
    except ValueError:
        return False


def main() -> int:
    args = sys.argv[1:]
    rtol = 1e-6
    if "--rtol" in args:
        i = args.index("--rtol")
        rtol = float(args[i + 1])
        del args[i : i + 2]
    if len(args) != 2:
        print(__doc__)
        return 2
    actual_path, expected_path = args

    with open(actual_path, newline="") as f:
        actual_rows = list(csv.reader(f))
    with open(expected_path, newline="") as f:
        expected_rows = list(csv.reader(f))

    if not actual_rows or not expected_rows:
        print(f"ERROR: one of the files is empty ({actual_path}, {expected_path})")
        return 1

    actual_header, expected_header = actual_rows[0], expected_rows[0]
    shared_cols = [c for c in expected_header if c in actual_header]
    only_actual = [c for c in actual_header if c not in expected_header]
    only_expected = [c for c in expected_header if c not in actual_header]

    if only_actual:
        print(f"  NEW COLUMNS (in this build, not in the reference -- review before sign-off):")
        print(f"    {only_actual}")
    if only_expected:
        print(f"  REMOVED COLUMNS (in the reference, not in this build -- review before sign-off):")
        print(f"    {only_expected}")

    actual_data, expected_data = actual_rows[1:], expected_rows[1:]
    if len(actual_data) != len(expected_data):
        print(
            f"  FAIL: row count differs: actual={len(actual_data)} "
            f"expected={len(expected_data)}"
        )
        return 1

    a_idx = {c: actual_header.index(c) for c in shared_cols}
    e_idx = {c: expected_header.index(c) for c in shared_cols}

    mismatches = 0
    for row_no, (a_row, e_row) in enumerate(zip(actual_data, expected_data), start=2):
        for col in shared_cols:
            a_val, e_val = a_row[a_idx[col]], e_row[e_idx[col]]
            if is_number(a_val) and is_number(e_val):
                a_f, e_f = float(a_val), float(e_val)
                if e_f == 0.0:
                    ok = abs(a_f) < 1e-12
                else:
                    ok = abs(a_f - e_f) <= rtol * abs(e_f)
            else:
                ok = a_val == e_val
            if not ok:
                mismatches += 1
                if mismatches <= 20:
                    print(
                        f"  FAIL: row {row_no} col '{col}': "
                        f"actual={a_val!r} expected={e_val!r}"
                    )

    if mismatches:
        print(f"  FAIL: {mismatches} value mismatch(es) (rtol={rtol}).")
        return 1

    print(
        f"  OK: {len(actual_data)} row(s) x {len(shared_cols)} shared column(s) "
        f"match within rtol={rtol}."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
