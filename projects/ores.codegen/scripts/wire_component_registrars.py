#!/usr/bin/env python3
"""Wire the trading entities whose generated registrars no umbrella calls.

V04 found that twelve of the component's shell-facet opt-ins have a
generated handler registrar and a generated history provider registrar
that nothing references, so the shell publishes to subjects no service
owns.  Each is an enumeration-shaped reference type that belongs beside
its siblings in core/src/messaging/registrar.cpp, the composition root
for the entity-shaped types.

The three blocks in that file are each kept sorted, so this inserts into
each in order rather than appending.  Run with --check to print the diff
and write nothing.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true", help="print the diff, write nothing")
    args = ap.parse_args()

    original = REGISTRAR.read_text(encoding="utf-8")
    lines = original.splitlines(keepends=True)

    includes = [
        f'{INCLUDE_PREFIX}{stem}_registrar.hpp"\n'
        for e in MISSING
        for stem in (f"{e}_history_provider", e)
    ]
    lines = insert_sorted(
        lines, includes,
        lambda ln: ln.startswith(INCLUDE_PREFIX))

    handlers = [
        f"    append(register_{e}_handlers(nats, ctx, verifier));\n" for e in MISSING
    ]
    lines = insert_sorted(
        lines, handlers,
        lambda ln: ln.startswith("    append(register_") and ln.rstrip().endswith(");"))

    providers = [
        f"    register_{e}_history_provider(hist_registry);\n" for e in MISSING
    ]
    lines = insert_sorted(
        lines, providers,
        lambda ln: ln.startswith("    register_") and ln.rstrip().endswith("(hist_registry);"))

    updated = "".join(lines)
    if updated == original:
        print("nothing to do: every registrar is already called")
        return 0

    if args.check:
        import difflib
        sys.stdout.writelines(
            difflib.unified_diff(
                original.splitlines(keepends=True), updated.splitlines(keepends=True),
                fromfile="registrar.cpp", tofile="registrar.cpp"))
        return 0

    REGISTRAR.write_text(updated, encoding="utf-8", newline="\n")
    print(f"wired {len(MISSING)} entities into {REGISTRAR.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
