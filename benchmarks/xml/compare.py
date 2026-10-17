#!/usr/bin/env python3
"""Print what changed between two runs in the benchmark history.

Usage: compare.py <older-commit> [<newer-commit>]

With one argument the newer side is the latest commit in the file. Metrics are
printed only where both sides recorded them, so a metric added later does not
read as an improvement.
"""

import csv
import os
import sys
from collections import defaultdict


def load(path):
    runs = defaultdict(dict)
    with open(path, newline="") as handle:
        for row in csv.DictReader(handle, delimiter="\t"):
            key = (row["mode"], row["input"])
            runs[key].setdefault(row["commit"], {})[row["metric"]] = float(row["value"])
    return runs


def commits_in_order(runs):
    order = []
    for metrics in runs.values():
        for commit in metrics:
            if commit not in order:
                order.append(commit)
    return order


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    runs = load(os.path.join(os.path.dirname(os.path.abspath(__file__)), "history.tsv"))
    order = commits_in_order(runs)
    older = sys.argv[1]
    newer = sys.argv[2] if len(sys.argv) > 2 else order[-1]
    if older not in order or newer not in order:
        sys.exit(f"no rows for {older} or {newer}; known: {', '.join(order)}")

    print(f"{'case':<24}{'metric':<14}{'older':>14}{'newer':>14}{'delta':>10}")
    for (mode, name), per_commit in sorted(runs.items()):
        if older not in per_commit or newer not in per_commit:
            continue
        for metric in ("ns_per_parse", "ir", "d1_misses", "ll_misses"):
            before = per_commit[older].get(metric)
            after = per_commit[newer].get(metric)
            if before is None or after is None or before == 0:
                continue
            delta = 100.0 * (after - before) / before
            print(f"{mode + ' ' + name:<24}{metric:<14}{before:>14.4g}{after:>14.4g}{delta:>+9.1f}%")


if __name__ == "__main__":
    main()
