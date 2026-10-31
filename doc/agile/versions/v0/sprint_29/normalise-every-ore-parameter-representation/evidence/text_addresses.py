#!/usr/bin/env python3
"""What the element map states about each leaf, read from the seeded map.

Every row of the map states a value set, where the rule comes from, and why.
This script groups the rows by what they say, so the coverage can be read from
the data rather than derived a second time:

- a leaf governed by a catalogue, an enumerated map, or a type states a rule;
- a leaf whose set is text states that no vocabulary governs the value, which is
  a decision the ORE parser and the schema justify;
- a leaf the ORE sources never name states text because nothing was ever
  decided: the extractor does not read the form the leaf is read in, so no
  parser states a rule for it.

The third group is what curation has to answer. For each of its addresses the
script prints the rule a sibling takes, meaning the same element name at another
address. A sibling is evidence, not an answer.

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

    by_element = {}
    for row in rows:
        if row["value_set_code"] != "text":
            by_element.setdefault(row["element_name"], set()).add(
                row["value_set_code"])

    ruled = [row for row in rows if row["value_set_code"] != "text"]
    decided = [row for row in rows
               if row["value_set_code"] == "text" and row["source"] != "ore_schema"]
    unread = [row for row in rows
              if row["value_set_code"] == "text" and row["source"] == "ore_schema"]

    print("the element map, by what each leaf's row states")
    print(f"  leaves                                   {len(rows):5d}")
    print(f"  a rule governs the value                 {len(ruled):5d}")
    print(f"  text, and the ORE source says why        {len(decided):5d}")
    print(f"  text, because no ORE source read names it{len(unread):6d}")
    print()

    print("a rule, by set")
    counts = {}
    for row in ruled:
        counts[row["value_set_code"]] = counts.get(row["value_set_code"], 0) + 1
    for code in sorted(counts, key=lambda c: (-counts[c], c)):
        print(f"  {code:44s} {counts[code]:3d}")
    print()

    print("text the ORE source justifies, by element")
    counts = {}
    for row in decided:
        counts[row["element_name"]] = counts.get(row["element_name"], 0) + 1
    for element in sorted(counts):
        print(f"  {element:44s} {counts[element]:3d}")
    print()

    print("text nothing was decided for, with the rule a sibling takes")
    for row in sorted(unread, key=lambda r: (r["element_name"], r["xpath"])):
        siblings = by_element.get(row["element_name"], set())
        stated = "  ".join(sorted(siblings)) if siblings else "-"
        print(f"  {row['schema_name']}|{row['xpath']:58s} {stated}")
    print()

    with_rule = [row for row in unread if by_element.get(row["element_name"])]
    print(f"{len(with_rule)} of the {len(unread)} have a sibling with a rule;"
          " the rest need a decision from the schema and the ORE parser.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
