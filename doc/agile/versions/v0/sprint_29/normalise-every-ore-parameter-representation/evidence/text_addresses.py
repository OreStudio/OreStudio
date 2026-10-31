#!/usr/bin/env python3
"""The leaves the map leaves unconstrained, and what a rule could be.

The element map states a value set per leaf address. Some addresses take the
shared text set, which admits every value, and they fall into two groups:

- the contract states =text= for the parameter, so a person decided that the
  leaf is free text;
- the contract holds no row for the address at all, so nothing was ever
  decided and the leaf is unconstrained by omission.

The second group is what curation has to answer. This script names it and
offers, for each address, the rule its siblings take: the same element name at
another address. A sibling is evidence, not an answer, and the count of
siblings that disagree is printed beside it.

Usage: text_addresses.py [--root PATH]
"""
import argparse
import pathlib
import sys


def repository_root():
    for parent in pathlib.Path(__file__).resolve().parents:
        if (parent / "projects" / "ores.sql").is_dir():
            return parent
    raise SystemExit("not inside the repository")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=None)
    args = parser.parse_args()

    root = pathlib.Path(args.root) if args.root else repository_root()
    sys.path.insert(0, str(root / "projects" / "ores.sql" / "utility"))
    import generate_ore_element_map as g

    rows = g.seeded(g.MAP, "ores_reporting_ore_parameter_vocabularies_tbl")
    stated = g.contract_sets()

    by_element = {}
    for row in rows:
        if row["value_set_code"] != "text":
            by_element.setdefault(row["element_name"], set()).add(
                row["value_set_code"])

    decided, undocumented = [], []
    for row in rows:
        if row["value_set_code"] != "text":
            continue
        entry = {
            "address": row["xpath"],
            "schema": row["schema_name"],
            "element": row["element_name"],
        }
        if (row["schema_name"], row["xpath"]) in stated:
            decided.append(entry)
        else:
            entry["siblings"] = sorted(by_element.get(row["element_name"], []))
            undocumented.append(entry)

    print("addresses the map leaves unconstrained")
    print(f"  total                                   {len(decided) + len(undocumented):5d}")
    print(f"  the contract states text for them       {len(decided):5d}")
    print(f"  the contract holds no row for them      {len(undocumented):5d}")
    print()

    if decided:
        print("stated free text, by element")
        counts = {}
        for entry in decided:
            counts[entry["element"]] = counts.get(entry["element"], 0) + 1
        for element in sorted(counts):
            print(f"  {element:40s} {counts[element]:3d}")
        print()

    print("no rule at all, with the rule a sibling takes")
    for entry in sorted(undocumented, key=lambda e: (e["element"], e["address"])):
        siblings = "  ".join(entry["siblings"]) if entry["siblings"] else "-"
        print(f"  {entry['schema']}|{entry['address']:58s} {siblings}")
    print()

    with_rule = [e for e in undocumented if e["siblings"]]
    print(f"{len(with_rule)} of the {len(undocumented)} have a sibling with a rule;"
          " the rest need a decision from the schema and the ORE parser.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
