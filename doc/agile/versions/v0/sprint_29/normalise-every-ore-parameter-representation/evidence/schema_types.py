#!/usr/bin/env python3
"""The ORE schema's type for each leaf, against the set the map states.

The ORE parser is the first authority for a leaf's rule and the schema is the
second. This script resolves each mapped leaf's type in its schema and prints
what the two authorities say, so a leaf where the schema is narrower than the map
is seen rather than assumed absent.

A row where the map states =text= and the schema declares a narrower type is a
gap the map should close. A row where the map states a rule and the schema
declares =xs:string= is not a gap: ORE's parser accepts fewer spellings than the
schema does, and the parser is what the engine runs.

Usage: schema_types.py [--root PATH]
"""
import argparse
import collections
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
    import ore_schema_types as schema

    rows = g.seeded(g.MAP, "ores_reporting_ore_parameter_vocabularies_tbl")

    resolved, unresolved = [], []
    for row in rows:
        found = schema.declared(row["schema_name"], row["xpath"])
        if found is None:
            unresolved.append(row)
        else:
            resolved.append((row, found))

    print("the ORE schema's type for each mapped leaf")
    print(f"  leaves                                {len(rows):5d}")
    print(f"  the schema resolves a type            {len(resolved):5d}")
    print(f"  the schema does not                   {len(unresolved):5d}")
    print()

    def described(entry):
        kind = schema.KIND_OF.get(entry)
        return f"{entry}" + (f" ({kind})" if kind else "")

    print("what the two authorities say, by schema type and set")
    counts = collections.Counter((described(found), row["value_set_code"])
                                 for row, found in resolved)
    for (found, code), number in sorted(counts.items(), key=lambda kv: -kv[1]):
        print(f"  {number:4d}  {found:34s} {code}")
    print()

    print("the map states text where the schema is narrower")
    gaps = [(row, found) for row, found in resolved
            if row["value_set_code"] == "text" and found in schema.KIND_OF]
    for row, found in sorted(gaps, key=lambda pair: pair[0]["xpath"]):
        print(f"  {row['schema_name']}|{row['xpath']:66s} {found}")
    print(f"  {len(gaps)} of the {len(rows)} leaves")
    print()

    print("the schema does not resolve a type for these leaves")
    for row in sorted(unresolved, key=lambda r: (r["schema_name"], r["xpath"])):
        print(f"  {row['schema_name']}|{row['xpath']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
