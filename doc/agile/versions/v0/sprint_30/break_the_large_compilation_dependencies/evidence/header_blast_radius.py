#!/usr/bin/env python3
"""Header blast radius: what rebuilds when one header changes.

Reads the build's own dependency record (``ninja -t deps``) and the project's
own include directives, and reports:

1. every project header ranked by the number of objects that depend on it, and
   by the number of targets those objects belong to;
2. every third-party header the build parses, ranked the same way, with the
   project headers that include it directly;
3. the include chain through which a heavy third-party header arrives;
4. the include cycles among project headers;
5. the cross-component includes, separated into public and detail includes.

The dependency record is per object, not per source file. This tree builds
unity translation units (``ORES_UNITY_BUILD``), so one object covers up to 16
sources and the object count understates how many sources read a header. The
target count is the honest measure of how many link products rebuild.

Usage:

    python3 header_blast_radius.py --build-dir build/output/linux-clang-release-ninja

Options:

    --top N          rows in each ranked table (default 30)
    --watch HEADER   third-party header to trace, repeatable; defaults to the
                     heavy dependencies this analysis started from
    --json PATH      also write the full data as JSON
    --no-scan        skip the source scan (dependency record only)
"""

from __future__ import annotations

import argparse
import json
from fnmatch import fnmatch
import os
import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

# The third-party headers this analysis traced, by the name each is included
# under. The first four are the ones the story named. The rest are the ones the
# measurement found expensive.
DEFAULT_WATCH = [
    "libpq-fe.h",
    "mutex",
    "condition_variable",
    "boost/asio.hpp",
    "chrono",
    "format",
    "filesystem",
    "ostream",
    "stdexcept",
    "boost/asio/ip/address.hpp",
    "boost/asio/io_context.hpp",
    "boost/lexical_cast.hpp",
    "boost/log/sources/severity_channel_logger.hpp",
    "boost/log/core.hpp",
    "boost/multiprecision/cpp_dec_float.hpp",
    "boost/uuid/uuid.hpp",
    "boost/uuid/string_generator.hpp",
    "magic_enum/magic_enum.hpp",
    "decimal.hpp",
    "rfl.hpp",
]

INCLUDE_RE = re.compile(r'^\s*#\s*include\s*([<"])([^>"]+)[>"]')
OBJECT_TARGET_RE = re.compile(r'/CMakeFiles/([^/]+)\.dir/')
SOURCE_SUFFIXES = {".hpp", ".h", ".hxx", ".ipp", ".cpp", ".cxx", ".cc"}
HEADER_SUFFIXES = {".hpp", ".h", ".hxx", ".ipp"}


def is_header(path: str) -> bool:
    return Path(path).suffix in HEADER_SUFFIXES

# A header in another component's src/ tree is a detail include: that tree holds
# the component's implementation, not its published surface.
DETAIL_MARKER = "/src/"


def run_ninja_deps(build_dir: Path) -> str:
    result = subprocess.run(
        ["ninja", "-t", "deps"],
        cwd=build_dir,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        sys.exit(f"error: ninja -t deps failed in {build_dir}:\n{result.stderr}")
    return result.stdout


def parse_deps(text: str, build_dir: Path):
    """Map dependency path -> {objects} and object -> target name.

    The record repeats a header once per object, so a plain line count per path
    is already the object count. The set is kept anyway so a duplicate line in
    one record cannot inflate the count.
    """
    dependents = defaultdict(set)
    obj_target = {}
    object_deps = defaultdict(set)
    objects = []
    current = None
    for line in text.splitlines():
        if line.startswith("    "):
            if current is None:
                continue
            dep = line.strip()
            if not dep or dep.endswith(" missing"):
                continue
            if not os.path.isabs(dep):
                dep = str(build_dir / dep)
            normalised = os.path.normpath(dep)
            dependents[normalised].add(current)
            if os.path.splitext(normalised)[1] in (".cpp", ".cxx", ".cc"):
                object_deps[current].add(normalised)
            continue
        current = line.split(":", 1)[0].strip()
        if not current:
            continue
        current = os.path.normpath(current)
        match = OBJECT_TARGET_RE.search(current)
        if not current.endswith(".o") or not match:
            current = None
            continue
        objects.append(current)
        obj_target[current] = match.group(1)
    return dependents, obj_target, objects, object_deps


GENERATED_MARKER = "AUTO-GENERATED FILE"
TEMPLATE_RE = re.compile(r"Template:\s*(\S+)")


def generated_templates(root: Path) -> dict[str, str]:
    """Map each generated header in the tree to the codegen template that wrote it.

    Codegen output is committed under projects/ rather than written into the
    build tree, so the dependency record cannot tell generated from hand-written
    code. The header's own banner can.
    """
    found = {}
    for path in (root / "projects").rglob("*"):
        if not path.is_file() or path.suffix not in HEADER_SUFFIXES:
            continue
        try:
            head = path.read_text(encoding="utf-8", errors="replace")[:2000]
        except OSError:
            continue
        if GENERATED_MARKER not in head:
            continue
        match = TEMPLATE_RE.search(head)
        found[str(path)] = match.group(1) if match else "?"
    return found


def classify(path: str, root: Path, build_dir: Path) -> str:
    if path.startswith(str(build_dir) + "/vcpkg_installed/"):
        return "vcpkg"
    if path.startswith(str(build_dir) + "/"):
        return "generated"
    if path.startswith(str(root) + "/projects/"):
        return "project"
    if path.startswith(("/usr/", "/opt/", "/nix/")):
        return "system"
    if path.startswith(str(root) + "/"):
        return "project"
    return "other"


def component_of(path: str, root: Path) -> str:
    rel = path[len(str(root)) + 1:] if path.startswith(str(root)) else path
    parts = rel.split("/")
    if parts and parts[0] == "projects" and len(parts) > 1:
        return parts[1]
    return ""


def include_roots(root: Path) -> list[Path]:
    return sorted((root / "projects").glob("*/include"))


def scan_includes(root: Path, build_dir: Path, include_dirs: list[Path]):
    """Build the project include graph from source text.

    Returns (edges, direct_third_party, files) where edges maps project file ->
    set of project files it includes, and direct_third_party maps third-party
    name -> set of project files that include it.
    """
    edges = defaultdict(set)
    direct_third = defaultdict(set)
    files = []
    roots = include_dirs + [build_dir / "projects"]

    def resolve(includer: Path, name: str):
        # A quoted include resolves beside the includer first, as the compiler
        # does, then through the include roots.
        local = includer.parent / name
        if local.is_file():
            return local
        for base in roots:
            candidate = base / name
            if candidate.is_file():
                return candidate
        return None

    for path in (root / "projects").rglob("*"):
        if not path.is_file() or path.suffix not in SOURCE_SUFFIXES:
            continue
        if "/CMakeFiles/" in str(path):
            continue
        files.append(path)
        try:
            text = path.read_text(encoding="utf-8", errors="replace")
        except OSError:
            continue
        for line in text.splitlines():
            match = INCLUDE_RE.match(line)
            if not match:
                continue
            name = match.group(2)
            resolved = resolve(path, name)
            if resolved is not None and str(resolved).startswith(str(root) + "/"):
                edges[path].add(resolved)
            else:
                direct_third[name].add(path)
    return edges, direct_third, files


def find_cycles(edges):
    """Tarjan's strongly connected components; report each non-trivial one."""
    index = {}
    low = {}
    stack = []
    on_stack = set()
    components = []
    counter = [0]

    def strongconnect(node):
        index[node] = low[node] = counter[0]
        counter[0] += 1
        stack.append(node)
        on_stack.add(node)
        for child in edges.get(node, ()):  # includes, not includers
            if child not in index:
                strongconnect(child)
                low[node] = min(low[node], low[child])
            elif child in on_stack:
                low[node] = min(low[node], index[child])
        if low[node] == index[node]:
            component = []
            while True:
                member = stack.pop()
                on_stack.discard(member)
                component.append(member)
                if member == node:
                    break
            if len(component) > 1 or node in edges.get(node, ()):
                components.append(component)

    for node in list(edges):
        if node not in index:
            strongconnect(node)
    return components


def include_chain_back(edges, target: str, limit: int = 6, max_nodes: int = 400):
    """Walk from a project header backwards to the sources that include it.

    Yields one path per included file: the chain of project headers from a
    translation unit down to the header that includes the third-party header.
    """
    incluers = defaultdict(set)
    for source, targets in edges.items():
        for included in targets:
            incluers[included].add(source)

    chains = []
    seen = set()

    def walk(node, path):
        if len(chains) >= limit or len(seen) > max_nodes:
            return
        parents = [p for p in incluers.get(node, ()) if p not in path]
        if not parents:
            chains.append(list(reversed(path)))
            return
        for parent in parents:
            seen.add(parent)
            walk(parent, path + [parent])

    walk(target, [target])
    return chains


def objects_naming(header: str, dependents, object_deps, source_includes,
                   root: Path):
    """Split the objects that read a header into those that name it and those
    that only receive it through another header.

    An object that compiles a source which includes the header directly keeps
    the dependency whatever else changes. An object that reads the header while
    none of its own sources include it is a candidate to lose the dependency
    when the header that drags it in stops including it. That second number is
    the rebuild saving a forward declaration can actually reach, so it is the
    number the work should be ordered by.
    """
    direct = source_includes.get(header, set())
    direct_names = {str(path) for path in direct}
    readers = dependents.get(header, set())
    naming = set()
    dragged = set()
    for obj in readers:
        sources = object_deps.get(obj, ())
        if direct_names & sources:
            naming.add(obj)
        else:
            dragged.add(obj)
    return naming, dragged


def reachable(roots, edges) -> set[str]:
    """Every project header reachable from a set of source files."""
    seen = set()
    stack = [str(root) for root in roots if str(root) in edges or True]
    while stack:
        node = stack.pop()
        if node in seen:
            continue
        seen.add(node)
        for child in edges.get(Path(node), ()):
            text = str(child)
            if text not in seen:
                stack.append(text)
    return seen


def cut_table(cuts, edges, object_deps, root: Path):
    """Objects that stop depending on a header when one include edge is cut.

    The dependency record says which objects read a header today. The source
    scan says how they reach it. Removing one edge and recomputing the reach
    from each object's own sources answers the question the story asks: if this
    include goes, how much of the build stops rebuilding when the included
    header changes.
    """
    resolved = {}
    for base in include_roots(root):
        for path in base.rglob("*"):
            if path.is_file():
                resolved.setdefault(
                    str(path)[len(str(base)) + 1:], path)

    by_path = {}
    for source, targets in edges.items():
        by_path[source] = targets

    rows = []
    for spec in cuts:
        includer_name, _, included_name = spec.partition(":")
        includer = resolved.get(includer_name)
        included = resolved.get(included_name)
        if includer is None or included is None:
            rows.append((spec, None, None, None))
            continue
        before = after = 0
        for obj, sources in object_deps.items():
            roots = [Path(s) for s in sources]
            if not roots:
                continue
            if str(included) not in reachable(roots, by_path):
                continue
            before += 1
            reduced = {source: {child for child in targets
                                if not (source == includer
                                        and child == included)}
                       for source, targets in by_path.items()}
            if str(included) in reachable(roots, reduced):
                after += 1
        rows.append((spec, before, after, before - after))
    return rows


def cut_set_table(specs, edges, object_deps, root: Path):
    """Objects that stop reading a header when a whole set of files drops it.

    A single edge rarely closes a door: one generated header family can be
    thousands of files, and the objects that read the header through any of them
    stop only when all of them drop it. The spec is ``header:glob``, where the
    glob selects the includers to cut, matched against the path under
    ``projects/``.
    """
    resolved = {}
    for base in include_roots(root):
        for path in base.rglob("*"):
            if path.is_file():
                resolved.setdefault(str(path)[len(str(base)) + 1:], path)

    rows = []
    for spec in specs:
        header_name, _, pattern = spec.partition(":")
        included = resolved.get(header_name)
        if included is None:
            rows.append((spec, None, None, None, 0))
            continue
        matching = {source for source in edges
                    if fnmatch(str(source), f"*/{pattern}")
                    or fnmatch(str(source), pattern)}
        reduced = {}
        for source, targets in edges.items():
            if source in matching:
                reduced[source] = {child for child in targets
                                   if child != included}
            else:
                reduced[source] = targets
        before = after = 0
        for obj, sources in object_deps.items():
            roots = [Path(s) for s in sources]
            if not roots:
                continue
            if str(included) in reachable(roots, edges):
                before += 1
            if str(included) in reachable(roots, reduced):
                after += 1
        rows.append((spec, before, after, before - after, len(matching)))
    return rows


def rel(path, root: Path) -> str:
    text = str(path)
    return text[len(str(root)) + 1:] if text.startswith(str(root)) else text


def rows_for(dependents, obj_target, predicate, root, top, extra=None):
    rows = []
    for dep, objects in dependents.items():
        if not predicate(dep):
            continue
        targets = {obj_target.get(o, "?") for o in objects}
        rows.append((len(objects), len(targets), dep, extra(dep) if extra else ""))
    rows.sort(key=lambda r: (-r[0], -r[1], r[2]))
    return rows[:top], len(rows)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", required=True)
    parser.add_argument("--top", type=int, default=30)
    parser.add_argument("--watch", action="append", default=None)
    parser.add_argument("--json", default=None)
    parser.add_argument("--no-scan", action="store_true")
    parser.add_argument("--cut", action="append", default=None,
                        metavar="INCLUDER:INCLUDED",
                        help="report the objects that stop reading INCLUDED "
                             "when INCLUDER stops including it")
    parser.add_argument("--cut-set", action="append", default=None,
                        metavar="INCLUDED:GLOB",
                        help="report the objects that stop reading INCLUDED "
                             "when every file matching GLOB drops it")
    args = parser.parse_args()

    root = Path(__file__).resolve().parents[7]
    build_dir = (root / args.build_dir).resolve() if not os.path.isabs(
        args.build_dir) else Path(args.build_dir)
    watch = args.watch or DEFAULT_WATCH

    text = run_ninja_deps(build_dir)
    dependents, obj_target, objects, object_deps = parse_deps(text, build_dir)
    total_targets = len(set(obj_target.values()))

    print(f"build dir    : {build_dir}")
    print(f"objects      : {len(objects)}")
    print(f"targets      : {total_targets}")
    print(f"headers read : {len(dependents)}")
    print()

    templates = generated_templates(root)
    project_rows, project_total = rows_for(
        dependents, obj_target,
        lambda p: classify(p, root, build_dir) == "project" and is_header(p),
        root, args.top,
        extra=lambda p: component_of(p, root) +
        (" [gen]" if p in templates else ""))
    generated_rows, generated_total = rows_for(
        dependents, obj_target,
        lambda p: classify(p, root, build_dir) == "generated" and is_header(p),
        root, args.top)

    print(f"== project headers by dependent objects (top {args.top} of {project_total})")
    print(f"{'objects':>8} {'targets':>8}  {'component':<24} header")
    for objects_n, targets_n, dep, comp in project_rows:
        print(f"{objects_n:>8} {targets_n:>8}  {comp:<24} {rel(dep, root)}")
    print()

    print(f"== codegen output: {len(templates)} generated headers in the tree, "
          f"{sum(1 for row in project_rows if row[2] in templates)} of the top "
          f"{len(project_rows)} project headers")
    by_template = defaultdict(int)
    for template in templates.values():
        by_template[template] += 1
    for template, count in sorted(by_template.items(), key=lambda kv: -kv[1])[:12]:
        print(f"{count:>8}  {template}")
    print()

    print(f"== build-tree headers by dependent objects (top {args.top} of {generated_total})")
    print(f"{'objects':>8} {'targets':>8}  header")
    for objects_n, targets_n, dep, _ in generated_rows:
        print(f"{objects_n:>8} {targets_n:>8}  {rel(dep, root)}")
    print()

    third_rows = []
    system_rows = []
    for dep, object_set in dependents.items():
        kind = classify(dep, root, build_dir)
        if kind not in ("vcpkg", "system"):
            continue
        targets = {obj_target.get(o, "?") for o in object_set}
        row = (len(object_set), len(targets), dep)
        (third_rows if kind == "vcpkg" else system_rows).append(row)
    third_rows.sort(key=lambda r: (-r[0], -r[1], r[2]))
    system_rows.sort(key=lambda r: (-r[0], -r[1], r[2]))

    print(f"== vcpkg headers by dependent objects (top {args.top} of {len(third_rows)})")
    print(f"{'objects':>8} {'targets':>8}  header")
    for objects_n, targets_n, dep in third_rows[:args.top]:
        print(f"{objects_n:>8} {targets_n:>8}  {dep.split('/include/', 1)[-1]}")
    print()

    print(f"== libc and libstdc++ headers by dependent objects "
          f"(top {min(args.top, 15)} of {len(system_rows)})")
    print(f"{'objects':>8} {'targets':>8}  header")
    for objects_n, targets_n, dep in system_rows[:min(args.top, 15)]:
        print(f"{objects_n:>8} {targets_n:>8}  {dep}")
    print()

    data = {
        "build_dir": str(build_dir),
        "objects": len(objects),
        "targets": total_targets,
        "project_headers": [
            {"header": rel(d, root), "objects": o, "targets": t, "component": c}
            for o, t, d, c in project_rows
        ],
        "generated_headers": [
            {"header": rel(d, root), "objects": o, "targets": t}
            for o, t, d, _ in generated_rows
        ],
        "third_party_headers": [
            {"header": d.split("/include/", 1)[-1], "objects": o, "targets": t}
            for o, t, d in third_rows[:100]
        ],
        "system_headers": [
            {"header": d, "objects": o, "targets": t}
            for o, t, d in system_rows[:50]
        ],
    }

    if not args.no_scan:
        include_dirs = include_roots(root)
        edges, direct_third, files = scan_includes(root, build_dir, include_dirs)
        print(f"== source scan: {len(files)} files, {sum(len(v) for v in edges.values())} project include edges")
        print()

        print("== who includes each watched third-party header (direct)")
        for name in watch:
            includers = direct_third.get(name, set())
            print(f"{name}: {len(includers)} direct includers")
            for includer in sorted(includers)[:12]:
                print(f"    {rel(includer, root)}")
            if len(includers) > 12:
                print(f"    ... {len(includers) - 12} more")
            data.setdefault("watch", {})[name] = [
                rel(i, root) for i in sorted(includers)]
        print()

        print("== include chains to the watched headers (one per leaf)")
        for name in watch:
            leaves = sorted(direct_third.get(name, set()))[:3]
            for leaf in leaves:
                for chain in include_chain_back(edges, leaf, limit=2):
                    print(f"    {name} <- " + " <- ".join(rel(c, root) for c in chain))
        print()

        source_includes = defaultdict(set)
        for source, targets in edges.items():
            for target in targets:
                source_includes[str(target)].add(str(source))

        if args.cut_set:
            print("== cutting a set of include edges: objects that still read "
                  "the header")
            print(f"{'before':>7} {'after':>6} {'saved':>6} {'files':>6}  set")
            for spec, before, after, saved, matched in cut_set_table(
                    args.cut_set, edges, object_deps, root):
                if before is None:
                    print(f"{'?':>7} {'?':>6} {'?':>6} {matched:>6}  {spec}")
                    continue
                print(f"{before:>7} {after:>6} {saved:>6} {matched:>6}  {spec}")
            print()

        if args.cut:
            print("== cutting one include edge: objects that stop reading the "
                  "included header")
            print(f"{'before':>7} {'after':>6} {'saved':>6}  edge")
            for spec, before, after, saved in cut_table(
                    args.cut, edges, object_deps, root):
                if before is None:
                    print(f"{'?':>7} {'?':>6} {'?':>6}  {spec}  (not resolved)")
                    continue
                print(f"{before:>7} {after:>6} {saved:>6}  {spec}")
            print()

        cycles = find_cycles(edges)
        print(f"== include cycles among project headers: {len(cycles)}")
        for component in cycles[:10]:
            print("    " + " <-> ".join(rel(c, root) for c in component[:6]))
        data["cycles"] = [[rel(c, root) for c in comp] for comp in cycles]
        print()

        cross = defaultdict(int)
        detail = defaultdict(int)
        for source, targets in edges.items():
            src_comp = component_of(str(source), root)
            for target in targets:
                tgt_comp = component_of(str(target), root)
                if not src_comp or not tgt_comp or src_comp == tgt_comp:
                    continue
                detail[(src_comp, tgt_comp)] += 1
                if DETAIL_MARKER in str(target):
                    cross[(src_comp, tgt_comp)] += 1
        print("== cross-component include edges (all / of another component's src)")
        for (src_comp, tgt_comp), count in sorted(
                detail.items(), key=lambda kv: -kv[1])[:args.top]:
            print(f"{count:>6} {cross.get((src_comp, tgt_comp), 0):>4}  "
                  f"{src_comp} -> {tgt_comp}")
        data["cross_component"] = [
            {"from": a, "to": b, "edges": n, "detail_edges": cross.get((a, b), 0)}
            for (a, b), n in sorted(detail.items(), key=lambda kv: -kv[1])]
        print()

    if args.json:
        Path(args.json).write_text(json.dumps(data, indent=2), encoding="utf-8")
        print(f"json written: {args.json}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
