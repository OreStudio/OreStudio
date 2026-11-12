#!/usr/bin/env python3
"""Header parse cost: what a header costs to parse, measured by clang.

Two modes, both driven by the build's own compile_commands.json so the flags
match the release build exactly:

``headers``
    For each named header, compile a translation unit whose only content is
    ``#include "<header>"`` with ``-fsyntax-only -ftime-trace``. The reported
    cost is the wall time of the parse and, from the trace, the sum of the
    per-file frontend time of the header's own tree. A header that costs
    little on its own can still be included by most of the build; this mode
    separates "expensive" from "universal".

``tu``
    Time-trace one real translation unit out of the build (a unity unit, a
    service, a test) and report the project and third-party headers that cost
    the most parse time inside it.

Usage:

    python3 header_cost.py headers --build-dir build/output/linux-clang-release-ninja \
        --header projects/ores.database/include/ores.database/domain/context.hpp

    python3 header_cost.py tu --build-dir build/output/linux-clang-release-ninja \
        --tu ores.synthetic/core --runs 1

The union of the include paths of a few representative translation units is
used for the probe, so every project header resolves.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import re
import shlex
import subprocess
import sys
import tempfile
import time
from collections import defaultdict
from pathlib import Path

FLAG_PREFIXES = ("-I", "-D", "-std=")
# An include directory that takes its path as the next argument.
PAIRED_FLAGS = {"-isystem", "-iquote", "-idirafter"}
# Two-argument flags to drop whole, so a probe never inherits the build's
# precompiled header or its output plumbing.
DROP_PAIRS = {"-o", "-MT", "-MF", "-MQ", "-include", "-include-pch", "-x"}
DROP_SINGLE = {"-c", "-MD", "-MMD", "-emit-pch", "-Winvalid-pch",
               "-fpch-instantiate-templates"}


def load_db(build_dir: Path):
    path = build_dir / "compile_commands.json"
    if not path.is_file():
        sys.exit(f"error: no compile_commands.json in {build_dir}")
    return json.loads(path.read_text(encoding="utf-8"))


def command_of(entry) -> list[str]:
    if "arguments" in entry:
        return list(entry["arguments"])
    return shlex.split(entry["command"])


def flags_of(entry) -> list[str]:
    """Include and define flags of one entry, with output plumbing dropped."""
    args = command_of(entry)
    flags = []
    index = 0
    while index < len(args):
        arg = args[index]
        if arg == "-Xclang":
            index += 2
            continue
        if arg in DROP_PAIRS:
            index += 2
            continue
        if arg in DROP_SINGLE:
            index += 1
            continue
        if arg in PAIRED_FLAGS:
            # Joined form, so a deduplicated set keeps the directory with its
            # flag and never leaves a bare path on the command line.
            flags.append(f"{arg}{args[index + 1]}")
            index += 2
            continue
        if arg.startswith(FLAG_PREFIXES):
            flags.append(arg)
        index += 1
    return flags


def is_compile_entry(entry) -> bool:
    """A real translation-unit compile, not a precompiled-header build."""
    args = command_of(entry)
    return "-emit-pch" not in args and "-Winvalid-pch" not in args


def compiler_of(entry) -> str:
    return command_of(entry)[0]


def union_flags(db, count: int = 40) -> list[str]:
    """Include paths and defines from a spread of entries, deduplicated."""
    seen = set()
    flags = []
    usable = [e for e in db if is_compile_entry(e)]
    step = max(1, len(usable) // count)
    for entry in usable[::step]:
        for flag in flags_of(entry):
            if flag not in seen:
                seen.add(flag)
                flags.append(flag)
    return flags


def trace_sources(trace_path: Path):
    """Exclusive per-file frontend milliseconds from a clang -ftime-trace JSON.

    The trace records each file it enters as a begin/end pair, nested for the
    include tree. A file's own cost is its span minus the spans of the files it
    included, so a header that only includes others does not inherit their
    cost. The trace only writes with a real compile, so the probe compiles to
    an object even though it never links.
    """
    data = json.loads(trace_path.read_text(encoding="utf-8"))
    per_file = defaultdict(float)
    total = 0.0
    stack = []
    # The trace groups its events by category, not by time, so the begin and
    # end pairs only nest once the events are in timestamp order.
    events = sorted((e for e in data.get("traceEvents", [])
                     if e.get("name") in ("Source", "ExecuteCompiler")),
                    key=lambda e: e.get("ts", 0))
    for event in events:
        name = event.get("name", "")
        phase = event.get("ph")
        if name == "ExecuteCompiler" and phase == "X":
            total = event.get("dur", 0) / 1000.0
            continue
        if name != "Source":
            continue
        if phase == "b":
            stack.append([event.get("args", {}).get("detail", "?"),
                          event.get("ts", 0), 0.0])
        elif phase == "e" and stack:
            detail, start, children = stack.pop()
            span = event.get("ts", 0) - start
            owned = max(0.0, span - children)
            # Convert the trace's microseconds to milliseconds.
            per_file[detail] += owned / 1000.0
            if stack:
                stack[-1][2] += span
    return per_file, total


def probe(compiler: str, flags: list[str], include: str, work: Path,
          runs: int) -> tuple[float, float, dict[str, float]]:
    tu = work / "probe.cpp"
    # An include written as <name> probes a system or third-party header; one
    # written plain is a project header, resolved through the build's -I paths.
    if include.startswith("<") and include.endswith(">"):
        tu.write_text(f"#include {include}\n", encoding="utf-8")
    else:
        tu.write_text(f'#include "{include}"\n', encoding="utf-8")
    obj = work / "probe.o"
    trace = work / "probe.json"
    times = []
    per_file = {}
    total = 0.0
    for _ in range(runs):
        for path in (obj, trace):
            if path.exists():
                path.unlink()
        start = time.perf_counter()
        result = subprocess.run(
            [compiler, "-c", "-std=c++23", "-ftime-trace", "-o", str(obj),
             *flags, str(tu)],
            capture_output=True, text=True)
        times.append(time.perf_counter() - start)
        if result.returncode != 0:
            return float("nan"), float("nan"), {"error": result.stderr[-800:]}
        if trace.exists():
            candidate, candidate_total = trace_sources(trace)
            if total == 0.0 or candidate_total < total:
                per_file, total = candidate, candidate_total
    # The box is shared, so the minimum is the least contended run and the
    # honest reading of what the parse costs.
    return min(times), total, per_file


def classify(path: str, root: Path, build_dir: Path) -> str:
    if "/vcpkg_installed/" in path:
        return "vcpkg"
    if path.startswith(str(build_dir) + "/"):
        return "generated"
    if path.startswith(str(root)):
        return "project"
    return "system"


def build_shadow(work: Path, root: Path, shadows) -> list[str]:
    """Copies of project headers with one include line removed.

    Each shadow is ``path:substring``. The copy is written into a shadow
    include root that is placed first on the command line, so a probe of the
    same header measures the header as it would be with that include gone.
    This is the what-if half of the analysis: it proves a suspected dead
    include is worth removing instead of arguing it.
    """
    shadow_root = work / "shadow"
    for spec in shadows:
        path_text, _, needle = spec.partition(":")
        if not needle:
            sys.exit(f"error: --shadow wants path:substring, got '{spec}'")
        source = root / path_text
        if not source.is_file():
            sys.exit(f"error: no such header: {source}")
        lines = source.read_text(encoding="utf-8").splitlines(keepends=True)
        kept = [line for line in lines if needle not in line]
        if len(kept) == len(lines):
            sys.exit(f"error: '{needle}' not found in {path_text}")
        # Mirror only the part the compiler's include path resolves, so the
        # shadow root can be added as a single -I and still win.
        relative = (path_text.split("/include/", 1)[-1]
                    if "/include/" in path_text else path_text)
        destination = shadow_root / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text("".join(kept), encoding="utf-8")
        print(f"shadow   : {path_text} without the line holding '{needle}'")
    return [f"-I{shadow_root}"] if shadows else []


def cmd_headers(args) -> int:
    root = Path(__file__).resolve().parents[7]
    build_dir = Path(args.build_dir)
    db = load_db(build_dir)
    if not db:
        sys.exit("error: empty compile database")
    compiler = compiler_of(db[0])
    flags = union_flags(db)

    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp)
        shadow_flags = build_shadow(work, root, args.shadow or [])
        flags = shadow_flags + flags
        print(f"compiler : {compiler}")
        print(f"flags    : {len(flags)} include and define flags (union over the build)")
        print()
        print(f"{'wall_s':>7} {'trace_s':>8}  header")
        rows = []
        for header in args.header:
            wall, total, per_file = probe(compiler, flags, header, work, args.runs)
            if "error" in per_file:
                print(f"{'ERROR':>7} {'':>8}  {header}")
                print(f"    {per_file['error'].splitlines()[-1]}")
                continue
            rows.append((wall, total, header, per_file))
            print(f"{wall:>7.2f} {total:>8.2f}  {header}")
        print()
        print("== where each probe spends its trace time (top 5 per header)")
        for wall, total, header, per_file in rows:
            print(f"{header} ({wall:.2f}s wall)")
            ranked = sorted(per_file.items(), key=lambda kv: -kv[1])[:5]
            for path, ms in ranked:
                print(f"    {ms:>8.1f} ms  {path}")
        print()
        if args.json:
            Path(args.json).write_text(json.dumps([
                {"header": header, "wall_s": wall, "trace_s": total,
                 "files": dict(sorted(per_file.items(), key=lambda kv: -kv[1])[:20])}
                for wall, total, header, per_file in rows], indent=2),
                encoding="utf-8")
            print(f"json written: {args.json}")
    return 0


def cmd_compare(args) -> int:
    """A/B one include: the header as it is against the header minus a line.

    The two variants are measured alternately, because the box is shared and a
    measurement taken minutes apart can differ by more than the effect being
    measured. The reported figure is the minimum of each variant's runs.
    """
    root = Path(__file__).resolve().parents[7]
    build_dir = Path(args.build_dir)
    db = load_db(build_dir)
    usable = [e for e in db if is_compile_entry(e)]
    compiler = compiler_of(usable[0])
    flags = union_flags(db)

    print(f"rounds   : {args.runs} per variant, alternating")
    print(f"{'before_s':>9} {'after_s':>8} {'delta_s':>8}  header (include removed)")
    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp)
        for spec in args.shadow:
            path_text, _, needle = spec.partition(":")
            # The probe names a header the way the code includes it, so a
            # projects/<component>/include/ prefix is dropped here.
            include = (path_text.split("/include/", 1)[-1]
                       if "/include/" in path_text else path_text)
            before = after = float("nan")
            for _ in range(args.runs):
                wall, _t, per_file = probe(compiler, flags, include, work, 1)
                if "error" in per_file:
                    sys.exit(f"error: probe of {path_text} failed: "
                             f"{per_file['error'].splitlines()[-1]}")
                before = wall if before != before else min(before, wall)
                shadow_flags = build_shadow(work, root, [spec])
                wall, _t, per_file = probe(
                    compiler, shadow_flags + flags, include, work, 1)
                after = wall if after != after else min(after, wall)
            print(f"{before:>9.2f} {after:>8.2f} {before - after:>8.2f}  "
                  f"{path_text}  [{needle}]")
    return 0


def cmd_tu(args) -> int:
    root = Path(__file__).resolve().parents[7]
    build_dir = Path(args.build_dir)
    db = load_db(build_dir)
    entries = [e for e in db
               if args.tu in e["file"] and is_compile_entry(e)]
    if not entries:
        sys.exit(f"error: no translation unit matches '{args.tu}'")
    worst = max(entries, key=lambda e: len(command_of(e)))
    args_list = command_of(worst)
    flags = flags_of(worst)
    compiler = args_list[0]

    with tempfile.TemporaryDirectory() as tmp:
        obj = Path(tmp) / "tu.o"
        trace = Path(tmp) / "tu.json"
        start = time.perf_counter()
        result = subprocess.run(
            [compiler, "-c", "-ftime-trace", "-o", str(obj), *flags,
             worst["file"]],
            capture_output=True, text=True, cwd=worst["directory"])
        wall = time.perf_counter() - start
        if result.returncode != 0:
            print(result.stderr[-2000:])
            return 1
        per_file, total = trace_sources(trace)
        print(f"translation unit : {worst['file']}")
        print(f"wall             : {wall:.2f}s")
        print(f"frontend total   : {total:.2f}s")
        print()
        rank = defaultdict(float)
        for path, ms in per_file.items():
            rank[classify(path, root, build_dir)] += ms
        print("== trace time by origin")
        for kind, ms in sorted(rank.items(), key=lambda kv: -kv[1]):
            print(f"{ms:>9.1f} ms  {kind}")
        print()
        print("== the twenty costliest files in this unit")
        for path, ms in sorted(per_file.items(), key=lambda kv: -kv[1])[:20]:
            print(f"{ms:>9.1f} ms  {path}")
    return 0


def trace_one(entry, out_dir: Path):
    """Trace one compile entry; returns (entry, per_file, total, wall)."""
    flags = flags_of(entry)
    compiler = command_of(entry)[0]
    stem = re.sub(r"[^A-Za-z0-9]+", "_", entry["file"])[-80:]
    obj = out_dir / f"{stem}.o"
    trace = out_dir / f"{stem}.json"
    start = time.perf_counter()
    result = subprocess.run(
        [compiler, "-c", "-ftime-trace", "-o", str(obj), *flags,
         entry["file"]],
        capture_output=True, text=True, cwd=entry["directory"])
    wall = time.perf_counter() - start
    if result.returncode != 0 or not trace.exists():
        entry["__error__"] = result.stderr[-300:]
        return entry, None, 0.0, wall
    per_file, total = trace_sources(trace)
    return entry, per_file, total, wall


def cmd_sample(args) -> int:
    """Trace a sample of real translation units and rank headers by time."""
    root = Path(__file__).resolve().parents[7]
    build_dir = Path(args.build_dir)
    db = load_db(build_dir)
    usable = [e for e in db if is_compile_entry(e)]
    if args.tu_pattern:
        picked = [e for e in usable
                  if any(p in e["file"] for p in args.tu_pattern)]
    else:
        picked = usable
    # One unity unit per target, taking the largest source file of each, so the
    # sample covers many components instead of one target's first batch.
    by_target = {}
    for entry in picked:
        target = entry["file"].split("/CMakeFiles/")[-1].split(".dir/")[0] \
            if "/CMakeFiles/" in entry["file"] else "other"
        current = by_target.get(target)
        size = Path(entry["directory"], entry["file"]).stat().st_size \
            if Path(entry["directory"], entry["file"]).is_file() else 0
        if current is None or size > current[0]:
            by_target[target] = (size, entry)
    sample = [entry for _size, entry in
              sorted(by_target.values(), key=lambda item: -item[0])[:args.count]]

    print(f"sample : {len(sample)} translation units of {len(usable)} compiles")
    print()
    with tempfile.TemporaryDirectory() as tmp:
        out_dir = Path(tmp)
        results = []
        with concurrent.futures.ThreadPoolExecutor(
                max_workers=args.workers) as pool:
            for entry, per_file, total, wall in pool.map(
                    lambda e: trace_one(e, out_dir), sample):
                name = entry["file"].split("CMakeFiles/")[-1].split(".dir/")[0]
                status = "ok" if per_file is not None else "FAILED"
                print(f"{wall:>7.2f}s {status:>6}  {name}")
                if per_file is None:
                    print(f"          {entry.get('__error__', '')}".strip())
                if per_file is not None:
                    results.append((name, per_file, total))

    sampled_total = sum(total for _n, _pf, total in results)
    aggregate = defaultdict(float)
    per_tu = defaultdict(int)
    for _name, per_file, _total in results:
        for path, ms in per_file.items():
            aggregate[path] += ms
        for path in per_file:
            per_tu[path] += 1

    print()
    print(f"sampled frontend time : {sampled_total / 1000:.1f}s")
    print(f"tree frontend estimate: {sampled_total / 1000 / len(results) * len(usable):.0f}s"
          f" over {len(usable)} compiles")
    print()
    print(f"== the costliest files over the sample "
          f"(top {args.top}, pure project headers)")
    print(f"{'total_s':>8} {'share':>7} {'tus':>4} {'ms/tu':>7}  header")
    rows = []
    for path, ms in sorted(aggregate.items(), key=lambda kv: -kv[1]):
        if not is_project_header(path, root, build_dir):
            continue
        rows.append((ms, per_tu[path], path))
    for ms, tus, path in rows[:args.top]:
        share = 100.0 * ms / sampled_total if sampled_total else 0.0
        print(f"{ms / 1000:>8.2f} {share:>6.1f}% {tus:>4} "
              f"{ms / tus:>7.0f}  {rel(path, root)}")
    print()

    print(f"== the costliest third-party files over the sample (top {args.top})")
    print(f"{'total_s':>8} {'share':>7} {'tus':>4} {'ms/tu':>7}  header")
    third = []
    for path, ms in sorted(aggregate.items(), key=lambda kv: -kv[1]):
        kind = classify(path, root, build_dir)
        if kind in ("project", "generated"):
            continue
        third.append((ms, per_tu[path], path))
    for ms, tus, path in third[:args.top]:
        share = 100.0 * ms / sampled_total if sampled_total else 0.0
        label = path.split("/include/", 1)[-1] if "/include/" in path else path
        print(f"{ms / 1000:>8.2f} {share:>6.1f}% {tus:>4} "
              f"{ms / tus:>7.0f}  {label}")
    print()

    if args.json:
        Path(args.json).write_text(json.dumps({
            "sample": [name for name, _p, _t in results],
            "sampled_frontend_s": sampled_total / 1000,
            "compiles": len(usable),
            "project": [{"header": rel(p, root), "total_ms": round(ms, 1),
                         "tus": tus}
                        for ms, tus, p in rows],
            "third_party": [{"header": p, "total_ms": round(ms, 1),
                             "tus": tus} for ms, tus, p in third[:100]],
        }, indent=2), encoding="utf-8")
        print(f"json written: {args.json}")
    return 0


HEADER_SUFFIXES = {".hpp", ".h", ".hxx", ".ipp"}


def rel(path, root: Path) -> str:
    text = str(path)
    return text[len(str(root)) + 1:] if text.startswith(str(root)) else text


def is_header(path: str) -> bool:
    return Path(path).suffix in HEADER_SUFFIXES


def is_project_header(path: str, root: Path, build_dir: Path) -> bool:
    return (classify(path, root, build_dir) in ("project", "generated")
            and is_header(path))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="mode", required=True)

    headers = sub.add_parser("headers")
    headers.add_argument("--build-dir", required=True)
    headers.add_argument("--header", action="append", required=True)
    headers.add_argument("--runs", type=int, default=2)
    headers.add_argument("--json", default=None)
    headers.add_argument("--shadow", action="append", default=None,
                         metavar="PATH:SUBSTRING",
                         help="probe the header as it would be with the first "
                              "line holding SUBSTRING removed")
    headers.set_defaults(func=cmd_headers)

    tu = sub.add_parser("tu")
    tu.add_argument("--build-dir", required=True)
    tu.add_argument("--tu", required=True)
    tu.set_defaults(func=cmd_tu)

    compare = sub.add_parser("compare")
    compare.add_argument("--build-dir", required=True)
    compare.add_argument("--shadow", action="append", required=True,
                         metavar="PATH:SUBSTRING")
    compare.add_argument("--runs", type=int, default=3)
    compare.set_defaults(func=cmd_compare)

    sample = sub.add_parser("sample")
    sample.add_argument("--build-dir", required=True)
    sample.add_argument("--tu-pattern", action="append", default=None)
    sample.add_argument("--count", type=int, default=12)
    sample.add_argument("--workers", type=int, default=4)
    sample.add_argument("--top", type=int, default=25)
    sample.add_argument("--json", default=None)
    sample.set_defaults(func=cmd_sample)

    args = parser.parse_args()
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
