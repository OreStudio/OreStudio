#!/usr/bin/env python3
"""What still reads pq/connection.hpp after the cut, and what read it before.

A header's rebuild radius is the set of objects whose dependency record lists
it. This task removed exactly four edges into pq/connection.hpp and added
none, so the radius before the change can be reconstructed from the tree after
it: an object read the header before if and only if it reads it now, or reads
one of the four headers that used to include it. Those four headers' own
includers did not change, which is what makes the union exact.

Usage:

    python3 reach_pq_headers.py --build-dir build/output/linux-clang-release-ninja

The build dir must be one that has compiled the tree after the change, because
the numbers come from ninja's dependency record, not from scanning sources.
"""

from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import header_blast_radius as blast

# The headers that stopped including pq/connection.hpp in this task.
CUT = [
    "ores.database/domain/database_options.hpp",
    "ores.database/pq/codec.hpp",
    "ores.database/pq/pool.hpp",
    "ores.database/service/postgres_listener_service.hpp",
]

REPORT = [
    "ores.database/pq/connection.hpp",
    "ores.database/pq/result.hpp",
    "ores.database/pq/connection_fwd.hpp",
    "ores.database/pq/pool.hpp",
    "ores.database/pq/codec.hpp",
    "ores.database/pq/statement.hpp",
    "ores.database/pq/transaction.hpp",
    "ores.database/domain/database_options.hpp",
    "ores.database/service/postgres_listener_service.hpp",
    "libpq-fe.h",
]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", required=True)
    args = parser.parse_args()

    root = Path(__file__).resolve().parents[7]
    build_dir = Path(args.build_dir)
    if not build_dir.is_absolute():
        build_dir = (root / build_dir).resolve()

    text = blast.run_ninja_deps(build_dir)
    dependents, obj_target, objects, object_deps = blast.parse_deps(text, build_dir)

    def readers(suffix: str) -> set[str]:
        hits = [d for d in dependents if d.endswith(suffix)]
        if not hits:
            raise SystemExit(f"no dependency path ends with {suffix}")
        best = max(hits, key=lambda d: len(dependents[d]))
        return dependents[best]

    def targets(paths) -> int:
        return len({obj_target.get(p, "?") for p in paths})

    print(f"objects {len(objects)}, targets {len(set(obj_target.values()))}")
    print()
    print(f"{'objects':>8} {'targets':>8}  header")
    after = {}
    for name in REPORT:
        s = readers(name)
        after[name] = s
        print(f"{len(s):>8} {targets(s):>8}  {name}")

    cut = [readers(h) for h in CUT]
    before_connection = after["ores.database/pq/connection.hpp"].union(*cut)
    before_libpq = after["libpq-fe.h"].union(*cut)

    print()
    print("reconstructed, before the change")
    print(f"{len(before_connection):>8} {targets(before_connection):>8}  "
          f"ores.database/pq/connection.hpp")
    print(f"{len(before_libpq):>8} {targets(before_libpq):>8}  libpq-fe.h")

    print()
    print("objects that stopped reading ores.database/pq/connection.hpp: "
          f"{len(before_connection - after['ores.database/pq/connection.hpp'])}")
    print("objects that stopped reading libpq-fe.h: "
          f"{len(before_libpq - after['libpq-fe.h'])}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
