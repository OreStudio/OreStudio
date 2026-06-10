#!/usr/bin/env python3
"""
Accumulate a tagged face dataset from facestud.io.

Iterates over the full cross-product of gender × age × ethnicity in a
randomised order, sleeping 10 minutes between requests so the script
can run in the background for several days without hammering the API.
Skips combinations already on disk — safe to kill and restart.

Output directory: assets/faces/  (relative to repo root; create before first run)
File naming:      <gender>_age<NN>_<ethnicity>.jpeg
                  e.g.  female_age34_south_asian.jpeg
"""

import itertools
import os
import random
import sys
import time
import urllib.request
import urllib.error
from pathlib import Path

# ---------------------------------------------------------------------------
# Parameters
# ---------------------------------------------------------------------------

GENDERS     = ["male", "female"]
ETHNICITIES = [
    "european",
    "african",
    "west_asian",
    "south_asian",
    "east_asian",
    "southeast_asian",
    "latin_american",
]
# Age step: every 5 years gives good coverage without 81 × 2 × 7 = 1134 images
# (that would take ~8 days).  Every 5 years → 17 steps × 2 × 7 = 238 images
# → ~1.6 days.  Change to range(0, 81) for the full set.
AGES        = list(range(0, 81, 5))

BASE_URL    = "https://facestud.io/v1/generate"
FORMAT      = "jpeg"
SLEEP_SECS  = 10 * 60   # 10 minutes between requests

# Output directory — relative to this script's location (build/tmp/),
# two levels up lands at the repo root.
SCRIPT_DIR  = Path(__file__).parent
REPO_ROOT   = SCRIPT_DIR.parent.parent
OUT_DIR     = SCRIPT_DIR / "faces"

# ---------------------------------------------------------------------------

def filename(gender: str, age: int, ethnicity: str) -> str:
    return f"{gender}_age{age:02d}_{ethnicity}.{FORMAT}"


def fetch(gender: str, age: int, ethnicity: str, dest: Path) -> bool:
    url = (
        f"{BASE_URL}"
        f"?gender={gender}"
        f"&age={age}"
        f"&ethnicity={ethnicity}"
    )
    try:
        with urllib.request.urlopen(url, timeout=30) as resp:
            data = resp.read()
        if len(data) < 1024:
            print(f"  WARNING: response suspiciously small ({len(data)} bytes) — skipping")
            return False
        dest.write_bytes(data)
        return True
    except urllib.error.HTTPError as e:
        print(f"  HTTP {e.code}: {e.reason}")
        return False
    except Exception as e:
        print(f"  ERROR: {e}")
        return False


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    combos = list(itertools.product(GENDERS, AGES, ETHNICITIES))
    random.shuffle(combos)   # randomise so early runs get good variety

    total   = len(combos)
    pending = [(g, a, e) for g, a, e in combos
               if not (OUT_DIR / filename(g, a, e)).exists()]

    print(f"Face dataset accumulator")
    print(f"  Output : {OUT_DIR}")
    print(f"  Total  : {total} combinations")
    print(f"  Done   : {total - len(pending)} already on disk")
    print(f"  Pending: {len(pending)}")
    print(f"  Sleep  : {SLEEP_SECS // 60} min between requests")
    print(f"  ETA    : ~{len(pending) * SLEEP_SECS / 3600:.1f} hours")
    print()

    if not pending:
        print("All combinations already downloaded.")
        return

    for i, (gender, age, ethnicity) in enumerate(pending, 1):
        fname = filename(gender, age, ethnicity)
        dest  = OUT_DIR / fname
        if dest.exists():
            continue  # another process may have written it

        print(f"[{i}/{len(pending)}] {fname}", end=" ... ", flush=True)
        ok = fetch(gender, age, ethnicity, dest)
        if ok:
            print(f"ok ({dest.stat().st_size // 1024} KB)")
        else:
            print("failed")

        if i < len(pending):
            try:
                time.sleep(SLEEP_SECS)
            except KeyboardInterrupt:
                print("\nInterrupted — progress saved; rerun to continue.")
                sys.exit(0)

    print("\nDone — all combinations downloaded.")


if __name__ == "__main__":
    main()
