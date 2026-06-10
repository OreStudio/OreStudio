#!/usr/bin/env python3
"""
Accumulate a tagged face dataset from facestudio.app.

Iterates over the full cross-product of gender × age × ethnicity × index in a
randomised order, sleeping 10 minutes between requests so the script can run in
the background for several days without hammering the API.
Skips combinations already on disk — safe to kill and restart.

Output directory: external/facestudio/faces/  (relative to repo root)
File naming:      <gender>_age<NN>_<ethnicity>_<NN>.jpeg
                  e.g.  female_age25_south_asian_01.jpeg
"""

import itertools
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
# Ages 20–70 in 5-year steps. Under-20 are not plausible account holders in a
# trading system; over-70 adds diminishing value.
AGES        = list(range(20, 71, 5))

# Number of images per (gender, age, ethnicity) combination.
# 11 ages × 2 genders × 7 ethnicities × 3 = 462 total images; at 60/day ≈ 8 days.
COUNT_PER_COMBO = 3

BASE_URL    = "https://facestud.io/v1/generate"
FORMAT      = "jpeg"
SLEEP_SECS  = 10 * 60   # 10 minutes between requests (~60 req/day)

# Output directory — external/facestudio/faces/ relative to repo root.
SCRIPT_DIR  = Path(__file__).parent
REPO_ROOT   = SCRIPT_DIR.parent.parent
OUT_DIR     = REPO_ROOT / "external" / "facestudio" / "faces"

# ---------------------------------------------------------------------------

def filename(gender: str, age: int, ethnicity: str, index: int) -> str:
    return f"{gender}_age{age:02d}_{ethnicity}_{index:02d}.{FORMAT}"


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
        if e.code == 429:
            print(f"\n  Daily rate limit reached (HTTP 429). Stopping cleanly.")
            print(f"  Progress saved — rerun tomorrow to continue.")
            sys.exit(1)
        print(f"  HTTP {e.code}: {e.reason}")
        return False
    except Exception as e:
        print(f"  ERROR: {e}")
        return False


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    combos = [
        (g, a, e, i)
        for g, a, e in itertools.product(GENDERS, AGES, ETHNICITIES)
        for i in range(1, COUNT_PER_COMBO + 1)
    ]
    random.shuffle(combos)   # randomise so early runs get good variety

    total   = len(combos)
    pending = [
        (g, a, e, i) for g, a, e, i in combos
        if not (OUT_DIR / filename(g, a, e, i)).exists()
    ]

    print(f"Face dataset accumulator")
    print(f"  Output : {OUT_DIR}")
    print(f"  Total  : {total} images ({COUNT_PER_COMBO} per combination)")
    print(f"  Done   : {total - len(pending)} already on disk")
    print(f"  Pending: {len(pending)}")
    print(f"  Sleep  : {SLEEP_SECS // 60} min between requests (~60/day limit)")
    print(f"  ETA    : ~{len(pending) * SLEEP_SECS / 3600:.1f} hours")
    print()

    if not pending:
        print("All combinations already downloaded.")
        return

    for i, (gender, age, ethnicity, idx) in enumerate(pending, 1):
        fname = filename(gender, age, ethnicity, idx)
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
