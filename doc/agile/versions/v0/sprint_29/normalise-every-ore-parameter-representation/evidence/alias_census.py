#!/usr/bin/env python3
"""The spellings ORE accepts for one value, read from the compiled table.

The story is about one fact held once. Today the spellings live in
=projects/ores.ore/core/src/domain/conventions_fields.cpp=, where each table maps
a spelling ORE accepts onto the canonical form the store holds. This script reads
those tables and prints what they state: how many spellings each holds, how many
canonical forms they name, and which canonical forms share the most spellings.

It reads the source rather than the database, because the point of the task is
that the source is where the knowledge should not be.

Usage: alias_census.py [--root PATH]
"""
import argparse
import collections
import pathlib
import re
import sys

SOURCE = "projects/ores.ore/core/src/domain/conventions_fields.cpp"
TABLE = re.compile(r"^const code_table (\w+) = \{", re.M)
SPELLINGS = re.compile(r"^const std::map<std::string, std::string, std::less<>> "
                       r"(\w+) = \{", re.M)
PAIR = re.compile(r'\{"([^"]*)",\s*"([^"]*)"\}')


def repository_root():
    for parent in pathlib.Path(__file__).resolve().parents:
        if (parent / "projects" / "ores.sql").is_dir():
            return parent
    raise SystemExit("not inside the repository")


def tables(text):
    """Each table's spelling-to-canonical pairs, and the line it starts at."""
    starts = [(m.group(1), m.start()) for m in TABLE.finditer(text)]
    starts += [(m.group(1), m.start()) for m in SPELLINGS.finditer(text)]
    starts.sort(key=lambda pair: pair[1])
    for index, (name, offset) in enumerate(starts):
        end = starts[index + 1][1] if index + 1 < len(starts) else len(text)
        chunk = text[offset:end]
        yield name, PAIR.findall(chunk), text[:offset].count("\n") + 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=None)
    args = parser.parse_args()

    root = pathlib.Path(args.root) if args.root else repository_root()
    text = (root / SOURCE).read_text(encoding="utf-8")

    total_spellings, total_canonical = 0, 0
    for name, pairs, line in tables(text):
        canonical = collections.Counter(second for _, second in pairs)
        total_spellings += len(pairs)
        total_canonical += len(canonical)
        print(f"{name} ({SOURCE}:{line})")
        print(f"  {len(pairs):3d} spellings over {len(canonical):3d} canonical forms")
        for form, count in sorted(canonical.items(), key=lambda kv: (-kv[1], kv[0])):
            if count > 1:
                print(f"    {count:3d} spellings name {form}")
        print()

    print(f"{total_spellings} spellings over {total_canonical} canonical forms in "
          f"{len(list(tables(text)))} tables")
    return 0


if __name__ == "__main__":
    sys.exit(main())
