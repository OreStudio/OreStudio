#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
"""
Record and check the trading entities no composition root called.

The V04 replay found that twelve of the component's shell-facet opt-ins had a
generated handler registrar and a generated history provider registrar that
nothing referenced, so the shell published to subjects no service owned. Each
is an enumeration-shaped reference type that belongs beside its siblings in
core/src/messaging/registrar.cpp, the composition root for the entity-shaped
types.

The list below is the finding, so the tool is a record as well as a writer:
--check reports whether every one of them is wired, which is what a reviewer
reruns. Run without --check to insert them into the composition root's three
sorted blocks; the insertion is idempotent, so a second run changes nothing.

No drift gate can see this class of defect, because the composition root is
hand-written while everything it composes is generated. A component whose
opt-ins grow should be replayed (V04) rather than inspected.

Usage:
    wire_trading_registrars.py --check
    wire_trading_registrars.py
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]
REGISTRAR = REPO / "projects/ores.trading/core/src/messaging/registrar.cpp"

MISSING = (
    "activity_category",
    "amortization_type",
    "average_type",
    "barrier_type",
    "exercise_type",
    "long_short_type",
    "moment_type",
    "option_type",
    "payoff_type",
    "price_type",
    "return_type",
    "settlement_type",
)

INCLUDE_PREFIX = '#include "ores.trading.core/messaging/'


def insert_sorted(lines: list[str], new: list[str], is_member) -> list[str]:
    """Insert `new` among the contiguous run of `is_member` lines, sorted."""
    idx = [i for i, ln in enumerate(lines) if is_member(ln)]
    if not idx:
        raise SystemExit("no anchor lines found; refusing to guess a position")
    start, end = idx[0], idx[-1] + 1
    block = [ln for ln in lines[start:end] if is_member(ln)]
    block.extend(new)
    block.sort(key=lambda s: s.strip())
    return lines[:start] + block + lines[end:]


def unwired(text: str) -> list[str]:
    """The listed entities whose handler or history provider is not called."""
    return [
        e for e in MISSING
        if f"append(register_{e}_handlers" not in text
        or f"register_{e}_history_provider(hist_registry);" not in text
    ]


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true",
                    help="report every listed entity that is not wired, "
                         "and write nothing")
    args = ap.parse_args()

    original = REGISTRAR.read_text(encoding="utf-8")

    if args.check:
        outstanding = unwired(original)
        if outstanding:
            print(f"{len(outstanding)} of {len(MISSING)} listed entities are "
                  f"not wired into {REGISTRAR.relative_to(REPO)}: "
                  + ", ".join(outstanding), file=sys.stderr)
            return 1
        print(f"all {len(MISSING)} listed entities are wired into "
              f"{REGISTRAR.relative_to(REPO)}")
        return 0

    lines = original.splitlines(keepends=True)
    includes = [
        f'{INCLUDE_PREFIX}{stem}_registrar.hpp"\n'
        for e in MISSING
        for stem in (f"{e}_history_provider", e)
    ]
    lines = insert_sorted(
        lines, includes,
        lambda ln: ln.startswith(INCLUDE_PREFIX))
    lines = insert_sorted(
        lines,
        [f"    append(register_{e}_handlers(nats, ctx, verifier));\n" for e in MISSING],
        lambda ln: ln.startswith("    append(register_") and ln.rstrip().endswith(");"))
    lines = insert_sorted(
        lines,
        [f"    register_{e}_history_provider(hist_registry);\n" for e in MISSING],
        lambda ln: ln.startswith("    register_") and ln.rstrip().endswith("(hist_registry);"))

    updated = "".join(lines)
    if updated == original:
        print("nothing to do: every listed registrar is already called")
        return 0

    REGISTRAR.write_text(updated, encoding="utf-8", newline="\n")
    print(f"wired {len(MISSING)} entities into {REGISTRAR.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
