#!/usr/bin/env python3
"""Every line a part CMakeLists carried at the branch base that its generated
replacement no longer has, grouped so a whole missing category shows as one
line. Deliberate changes (the GLOB replaced by component_files.cmake, comments
the archetype writes differently) appear too; the point is that each group is
small enough to name.
"""

import re
import subprocess
from collections import Counter

BASE = "ed7e0a3a87"


def lines(text):
    out = []
    for ln in text.splitlines():
        s = " ".join(ln.split())
        if s and not s.startswith("#"):
            out.append(s)
    return out


def main():
    paths = subprocess.run(["git", "ls-tree", "-r", "--name-only", BASE],
                           capture_output=True, text=True).stdout.split()
    paths = [p for p in paths
             if re.search(r"projects/[^/]+/[^/]+/(src|tests)/CMakeLists\.txt$", p)]
    lost = Counter()
    files = 0
    for path in paths:
        hand = subprocess.run(["git", "show", f"{BASE}:{path}"],
                              capture_output=True, text=True).stdout
        try:
            generated = open(path).read()
        except FileNotFoundError:
            lost[f"MISSING FILE {path}"] += 1
            continue
        have = Counter(lines(generated))
        removed = []
        for ln in lines(hand):
            if have[ln]:
                have[ln] -= 1
            else:
                removed.append(ln)
        if removed:
            files += 1
            for ln in removed:
                lost[ln] += 1
    for ln, n in lost.most_common(60):
        print(f"{n:4}  {ln}")
    print(f"\n{files} of {len(paths)} files differ, "
          f"{sum(lost.values())} lines dropped")


main()
