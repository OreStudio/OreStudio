#!/usr/bin/env python3
"""Print every distinct market data key the ORE example corpus carries, sorted.

A market payload is a .txt or .csv file under external/ore/examples whose name
contains "market" or starts with "MD_" (the dated dumps), outside an
ExpectedOutput directory and not a fixings file. A payload line is a date, a key
and a value, separated by whitespace, commas or semicolons; comments start with
'#'. The key is the line's second field.

Usage: extract_corpus_keys.py <examples dir>
"""

import re
import sys
from pathlib import Path


def is_market_payload(path: Path) -> bool:
    name = path.name
    if path.suffix not in (".txt", ".csv") or "fixing" in name.lower():
        return False
    if "ExpectedOutput" in path.parts:
        return False
    return "market" in name or name.startswith("MD_")


def main():
    root = Path(sys.argv[1])
    keys = set()
    files = 0
    skipped = 0
    for path in sorted(root.rglob("*")):
        if not path.is_file() or not is_market_payload(path):
            continue
        files += 1
        for line in path.read_text(errors="replace").splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            fields = re.split(r"[\s,;]+", line)
            if len(fields) >= 3:
                keys.add(fields[1])
            else:
                skipped += 1
    for key in sorted(keys):
        print(key)
    print(f"{len(keys)} keys from {files} files, {skipped} lines skipped as not "
          f"date, key and value", file=sys.stderr)


if __name__ == "__main__":
    main()
