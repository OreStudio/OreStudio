#!/usr/bin/env python3
"""Summarise the compile cost of one build phase from a ninja log.

The unity batch measurement builds the same tree several times: a cold build
of a base commit, an incremental build of the next commit in the same tree,
and a build of that commit in a fresh tree with a warm compiler cache. Ninja
appends every phase to one ``.ninja_log``, so a phase is the entries after the
line count recorded before it started.

Usage:
  ninja_log_summary.py phase --log <.ninja_log> --skip <lines> --wall <seconds>
      [--hits N --misses N] --out <json>
      Write the summary of the entries after the first <lines> lines.
  ninja_log_summary.py table <json>...
      Print one markdown table that compares the measured runs.
"""

import argparse
import json
import re
import sys
from collections import defaultdict
from pathlib import Path

TARGET = re.compile(r"CMakeFiles/([^/]+)\.dir/")


def read_phase(log, skip):
    """Return {output: seconds} for the entries after the first skip lines."""
    entries = {}
    lines = Path(log).read_text().splitlines()
    for line in lines[skip:]:
        if line.startswith("#"):
            continue
        fields = line.split("\t")
        if len(fields) < 4:
            continue
        start, end, _, output = fields[:4]
        entries[output] = (int(end) - int(start)) / 1000.0
    return entries


def summarise(entries):
    units = {k: v for k, v in entries.items() if k.endswith(".o")}
    by_target = defaultdict(float)
    for output, seconds in units.items():
        match = TARGET.search(output)
        by_target[match.group(1) if match else "other"] += seconds
    slowest = sorted(units.items(), key=lambda kv: -kv[1])[:5]
    return {
        "steps": len(entries),
        "units": len(units),
        "compile_seconds": round(sum(units.values()), 1),
        "slowest": [{"output": k, "seconds": round(v, 1)} for k, v in slowest],
        "targets": {k: round(v, 1) for k, v in
                    sorted(by_target.items(), key=lambda kv: -kv[1])[:10]},
    }


def phase(args):
    summary = summarise(read_phase(args.log, args.skip))
    summary["wall_seconds"] = args.wall
    if args.hits is not None and args.misses is not None:
        total = args.hits + args.misses
        summary["cache_hits"] = args.hits
        summary["cache_misses"] = args.misses
        summary["hit_rate"] = round(args.hits / total, 3) if total else None
    out = Path(args.out)
    data = json.loads(out.read_text()) if out.exists() else {}
    data.update(args.meta or {})
    data[args.name] = summary
    out.write_text(json.dumps(data, indent=2))


def minutes(seconds):
    return "-" if seconds is None else f"{seconds / 60:.1f}"


def table(args):
    rows = [json.loads(Path(p).read_text()) for p in args.files]
    rows.sort(key=lambda r: -int(r.get("batch", 0)))
    print("| Batch | Cold wall (min) | Cold CPU (min) | Cold units "
          "| Slowest unit (min) | Incremental wall (min) | Incremental units "
          "| Warm wall (min) | Warm hit rate |")
    print("|---|---|---|---|---|---|---|---|---|")
    for r in rows:
        cold, inc, warm = r.get("cold", {}), r.get("incremental", {}), r.get("warm", {})
        slowest = cold.get("slowest") or [{}]
        rate = warm.get("hit_rate")
        batch = r.get("batch", "?")
        print(f"| {'off' if batch == '0' else batch} "
              f"| {minutes(cold.get('wall_seconds'))} "
              f"| {minutes(cold.get('compile_seconds'))} "
              f"| {cold.get('units', '-')} "
              f"| {minutes(slowest[0].get('seconds'))} "
              f"| {minutes(inc.get('wall_seconds'))} "
              f"| {inc.get('units', '-')} "
              f"| {minutes(warm.get('wall_seconds'))} "
              f"| {'-' if rate is None else f'{rate:.0%}'} |")
    print()
    for r in rows:
        slowest = (r.get("cold", {}).get("slowest") or [])[:3]
        if slowest:
            batch = r.get("batch", "?")
            print(f"Slowest cold units, batch {'off' if batch == '0' else batch}:")
            for s in slowest:
                print(f"- {s['seconds'] / 60:.1f} min `{s['output']}`")
            print()


def meta_pair(text):
    key, _, value = text.partition("=")
    return key, value


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = parser.add_subparsers(dest="command", required=True)
    p = sub.add_parser("phase")
    p.add_argument("--log", required=True)
    p.add_argument("--skip", type=int, default=0)
    p.add_argument("--wall", type=float)
    p.add_argument("--hits", type=int)
    p.add_argument("--misses", type=int)
    p.add_argument("--name", required=True)
    p.add_argument("--out", required=True)
    p.add_argument("--meta", type=meta_pair, action="append")
    t = sub.add_parser("table")
    t.add_argument("files", nargs="+")
    args = parser.parse_args()
    if args.command == "phase":
        if args.meta:
            args.meta = dict(args.meta)
        phase(args)
    else:
        table(args)


if __name__ == "__main__":
    sys.exit(main())
