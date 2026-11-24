#!/usr/bin/env python3
"""Where the tree states which renderings of one value mean the same fact.

The task moves that statement into the value sets. This script counts the copies
that exist while both statements do, so the task's claim about how many places
hold the knowledge is a measurement and not an impression.

Three places state it:
- projects/ores.ore/core/src/domain/conventions_fields.cpp, the reader's tables;
- projects/ores.refdata/core/include/ores.refdata.core/repository/
  business_day_convention_code.hpp, which the refdata entities read;
- tools/ore_conventions/generate_dq_seed.py, which normalises the seeded
  conventions before it writes them.

Usage: alias_copies.py [--root PATH]
"""
import argparse
import importlib.util
import pathlib
import re
import sys

READER = "projects/ores.ore/core/src/domain/conventions_fields.cpp"
REFDATA = ("projects/ores.refdata/core/include/ores.refdata.core/repository/"
           "business_day_convention_code.hpp")
SEEDER = "tools/ore_conventions/generate_dq_seed.py"

CODE_TABLE = re.compile(r"const code_table (\w+) = \{(.*?)\n\};", re.S)
SPELLING_TABLE = re.compile(
    r"const std::map<std::string, std::string, std::less<>> (\w+) = \{(.*?)\n\};", re.S)
ENTRY = re.compile(r'\{"((?:[^"\\]|\\.)*)", "((?:[^"\\]|\\.)*)"\}')


def repository_root():
    for parent in pathlib.Path(__file__).resolve().parents:
        if (parent / "projects" / "ores.sql").is_dir():
            return parent
    raise SystemExit("not inside the repository")


def reader_tables(root):
    text = (root / READER).read_text(encoding="utf-8")
    found = {name: dict(ENTRY.findall(body)) for name, body in CODE_TABLE.findall(text)}
    found.update({name: dict(ENTRY.findall(body))
                  for name, body in SPELLING_TABLE.findall(text)})
    return found


def refdata_table(root):
    """The renderings the refdata entities convert, as the header states them."""
    text = (root / REFDATA).read_text(encoding="utf-8")
    start = text.index("conventions")
    body = text[start:]
    return dict(ENTRY.findall(body))


def seeder_tables(root):
    """The seeder's normalisers, read as the module it is."""
    spec = importlib.util.spec_from_file_location("generate_dq_seed", root / SEEDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return {"day_counters": module.DAY_COUNTER,
            "business_day_conventions": module.BDC,
            "frequencies": module.FREQUENCY}


def compare(name, left, right, left_name, right_name):
    """The renderings one copy states and the other does not, both ways."""
    only_left = sorted(set(left) - set(right))
    only_right = sorted(set(right) - set(left))
    differing = sorted(spelling for spelling in set(left) & set(right)
                       if left[spelling] != right[spelling])
    print(f"  {name}: {len(left)} in {left_name}, {len(right)} in {right_name}")
    for label, renderings in ((f"only {left_name}", only_left),
                              (f"only {right_name}", only_right),
                              ("mapped differently", differing)):
        if renderings:
            print(f"    {label}: {', '.join(renderings)}")


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--root", default=None)
    args = parser.parse_args()
    root = pathlib.Path(args.root) if args.root else repository_root()

    reader = reader_tables(root)
    refdata = refdata_table(root)
    seeder = seeder_tables(root)

    print("renderings one copy of the knowledge states")
    for name in ("business_day_conventions", "day_counters", "frequencies",
                 "compoundings", "date_rules", "day_counter_spellings"):
        table = reader.get(name, {})
        forms = len(set(table.values()))
        print(f"  {name}: {len(table)} renderings over {forms} forms "
              f"({READER})")
    print(f"  business_day_conventions: {len(refdata)} renderings ({REFDATA})")
    for name, table in seeder.items():
        print(f"  {name}: {len(table)} renderings ({SEEDER})")

    print()
    print("where the copies disagree")
    compare("business_day_conventions", reader["business_day_conventions"],
            refdata, "the reader", "refdata")
    for name, table in seeder.items():
        compare(name, reader[name], table, "the reader", "the seeder")
    return 0


if __name__ == "__main__":
    sys.exit(main())
