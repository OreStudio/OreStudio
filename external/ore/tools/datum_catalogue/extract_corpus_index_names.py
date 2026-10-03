#!/usr/bin/env python3
"""Print every distinct index name the ORE example corpus's fixing files carry,
sorted.

A fixing payload is a .txt or .csv file under external/ore/examples whose name
contains "fixing" or starts with "FD_" (the dated dumps), outside an
ExpectedOutput directory. A payload line is a date, an index name and a value,
separated by commas, semicolons or whitespace; comment and blank lines are
skipped. The selection matches corpus_files.hpp in the marketdata core tests.

Usage: extract_corpus_index_names.py <examples dir>
"""

import re
import sys
from pathlib import Path


def is_fixing_payload(path: Path) -> bool:
    name = path.name
    if path.suffix not in (".txt", ".csv"):
        return False
    if "ExpectedOutput" in path.parts:
        return False
    return "fixing" in name or name.startswith("FD_")


def main():
    names = set()
    for path in sorted(Path(sys.argv[1]).rglob("*")):
        if not path.is_file() or not is_fixing_payload(path):
            continue
        for line in path.read_text(errors="ignore").splitlines():
            text = line.strip()
            if not text or text.startswith("#"):
                continue
            parts = re.split(r"[,;\s]+", text)
            if len(parts) >= 3:
                names.add(parts[1])
    for name in sorted(names):
        print(name)


if __name__ == "__main__":
    main()
