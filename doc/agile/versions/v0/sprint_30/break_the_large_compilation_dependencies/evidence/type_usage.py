#!/usr/bin/env python3
"""What a header's includers do with each type the header defines.

The rebuild question is not how often a header is read but how many objects must
be rebuilt when it changes. A translation unit that only names a type (a
reference, a pointer, a smart-pointer member) can drop the include and take a
forward declaration instead, and then a change to the header rebuilds fewer
objects. A translation unit that uses the type in full cannot.

For one header this tool:

1. lists the types the header defines, with the namespace each lives in;
2. classifies every use of each type in every direct includer as *only named*
   or *used in full*;
3. reports, per type, how many includers could drop the include;
4. with ``--verify``, proves each candidate by compiling the includer with the
   include line replaced by a forward declaration. A failed compile is the
   answer "not a candidate", which is what makes the count trustworthy.

The classification in step 2 is syntactic and therefore a candidate list, not a
result: a file that holds ``const context&`` but calls ``ctx.tenant_id()`` needs
the definition although it only names the type. Step 4 is what settles it.

Usage:

    python3 type_usage.py --build-dir build/output/linux-clang-release-ninja \
        --header ores.database/domain/context.hpp

    python3 type_usage.py --build-dir build/output/linux-clang-release-ninja \
        --header ores.database/domain/context.hpp --verify
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
import tempfile
from collections import defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import header_blast_radius as blast
import header_cost as cost

TYPE_RE = re.compile(
    r"^\s*(?:template\s*<[^>]*>\s*)?"
    r"(class|struct|enum\s+class|enum\s+struct|enum|using|typedef)\s+"
    r"(?:ORES_\w+_EXPORT\s+)?([A-Za-z_]\w*)")
NAMESPACE_RE = re.compile(r"^\s*namespace\s+([\w:]+)\s*\{")
UNDERLYING_RE = re.compile(r"enum\s+(?:class|struct)?\s*\w+\s*:\s*([\w:]+)")
SMART_POINTERS = ("unique_ptr", "shared_ptr", "weak_ptr")


def declared_types(header: Path):
    """The types a header defines at namespace scope, with their namespace.

    Nested types belong to their enclosing class: a private ``pq_slot`` inside
    ``context`` is not something an includer can see, so brace depth decides
    what counts. A type at the namespace's own depth is one an includer names.
    """
    text = header.read_text(encoding="utf-8", errors="replace")
    namespace = ""
    for line in text.splitlines():
        match = NAMESPACE_RE.match(line)
        if match and not namespace:
            namespace = match.group(1)
            break
    depth = 0
    namespace_depth = 0
    found = []
    seen = set()
    for line in text.splitlines():
        stripped = line.strip()
        opens = line.count("{")
        closes = line.count("}")
        declaring = TYPE_RE.match(line)
        if declaring and depth == namespace_depth and namespace_depth:
            kind, name = declaring.group(1), declaring.group(2)
            if name not in seen and name not in ("final", "override"):
                seen.add(name)
                underlying = ""
                if kind.startswith("enum"):
                    under = UNDERLYING_RE.search(line)
                    if under:
                        underlying = under.group(1)
                found.append({"kind": kind, "name": name,
                              "namespace": namespace,
                              "underlying": underlying})
        depth += opens - closes
        if namespace_depth == 0 and NAMESPACE_RE.match(line):
            namespace_depth = depth
        if stripped.startswith("}") and depth < namespace_depth:
            namespace_depth = 0
    return found


def forward_declaration(entry) -> str | None:
    """The declaration that replaces the header for one type, or None."""
    kind, name, namespace = entry["kind"], entry["name"], entry["namespace"]
    if kind in ("using", "typedef"):
        return None
    if kind in ("class", "struct"):
        line = f"{kind} {name};"
    elif kind in ("enum class", "enum struct"):
        line = (f"{kind} {name} : {entry['underlying']};"
                if entry["underlying"] else f"{kind} {name};")
    else:
        line = (f"enum {name} : {entry['underlying']};"
                if entry["underlying"] else None)
    if line is None:
        return None
    return f"namespace {namespace} {{ {line} }}" if namespace else line


def strip_noise(text: str) -> str:
    """The file with comments and the include lines removed."""
    text = re.sub(r"/\*.*?\*/", " ", text, flags=re.S)
    text = re.sub(r"//[^\n]*", " ", text)
    return "\n".join(line for line in text.splitlines()
                     if not line.lstrip().startswith("#include"))


def classify_uses(text: str, name: str):
    """Per occurrence of ``name``: 'named' or 'full'."""
    body = strip_noise(text)
    verdicts = []
    for match in re.finditer(rf"\b{re.escape(name)}\b", body):
        before = body[max(0, match.start() - 40):match.start()]
        after = body[match.end():match.end() + 4]
        tail = after.lstrip()
        if tail[:1] in ("&", "*"):
            verdicts.append("named")
            continue
        # A smart pointer holding the type needs only the declaration when the
        # pointer is a member or a parameter; unique_ptr<T> and shared_ptr<T>
        # accept an incomplete type at that point.
        under = re.search(r"(\w+)<\s*(?:const\s+)?$", before)
        if under and under.group(1) in SMART_POINTERS:
            verdicts.append("named")
            continue
        verdicts.append("full")
    return verdicts


def stub_for(header: Path, declarations: list[str]) -> str:
    """A header that keeps the original includes and drops the definitions.

    Keeping the includes matters: replacing the header with nothing but forward
    declarations also removes every header it pulled in, so a unit that used a
    transitively provided type would fail for a reason unrelated to the type
    under test. The stub therefore asks one narrow question: does this unit need
    the *definition* of the type, having still been given everything else the
    header includes?
    """
    text = header.read_text(encoding="utf-8", errors="replace")
    includes = [line for line in text.splitlines()
                if line.lstrip().startswith("#include")]
    return "\n".join(includes + [""] + declarations) + "\n"


def verify_candidate(includer: Path, include_name: str, stub_text: str,
                     root: Path, flags: list[str], work: Path) -> bool:
    """Compile an includer with the header replaced by forward declarations.

    A shadow include root is placed first on the command line and holds one file
    at the header's own include path, containing only the forward declarations.
    Nothing else moves: the includer is compiled where it lives, so its relative
    includes resolve as they always do.

    A pass means this includer needs no definition from the header, so it can
    drop the include and stop rebuilding when the header changes.
    """
    stub_root = work / "stub"
    stub = stub_root / include_name
    stub.parent.mkdir(parents=True, exist_ok=True)
    stub.write_text(stub_text + "\n", encoding="utf-8")

    if includer.suffix in (".cpp", ".cxx", ".cc"):
        source = str(includer)
    else:
        probe = work / "probe.cpp"
        probe.write_text(f'#include "{includer}"\n', encoding="utf-8")
        source = str(probe)
    result = subprocess.run(
        ["clang++", "-fsyntax-only", "-std=c++23", f"-I{stub_root}", *flags,
         source], capture_output=True, text=True)
    return result.returncode == 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", required=True)
    parser.add_argument("--header", required=True,
                        help="include name, e.g. ores.database/domain/context.hpp")
    parser.add_argument("--verify", action="store_true")
    parser.add_argument("--limit", type=int, default=12)
    parser.add_argument("--json", default=None)
    args = parser.parse_args()

    root = Path(__file__).resolve().parents[7]
    build_dir = (root / args.build_dir).resolve()
    header = None
    for base in blast.include_roots(root):
        candidate = base / args.header
        if candidate.is_file():
            header = candidate
            break
    if header is None:
        sys.exit(f"error: no header {args.header} under any include root")

    dependents, obj_target, objects, _object_deps = blast.parse_deps(
        blast.run_ninja_deps(build_dir), build_dir)
    key = str(header)
    objects_that_rebuild = len(dependents.get(key, ()))

    edges, _third, _files = blast.scan_includes(
        root, build_dir, blast.include_roots(root))
    includers = defaultdict(set)
    for source, targets in edges.items():
        for target in targets:
            includers[target].add(source)
    direct = sorted(includers.get(header, set()))

    types = declared_types(header)
    print(f"header      : {args.header}")
    print(f"objects     : {objects_that_rebuild} rebuild when this header changes")
    print(f"includers   : {len(direct)} files include it directly")
    print(f"types       : {len(types)} defined here")
    print()

    report = {"header": args.header, "objects": objects_that_rebuild,
              "direct_includers": len(direct), "types": []}

    for entry in types:
        named = []
        full = []
        for includer in direct:
            verdicts = classify_uses(
                includer.read_text(encoding="utf-8", errors="replace"),
                entry["name"])
            if not verdicts:
                continue
            if all(verdict == "named" for verdict in verdicts):
                named.append(includer)
            else:
                full.append(includer)
        print(f"== {entry['kind']} {entry['namespace']}::{entry['name']} "
              f"({entry['kind']})")
        print(f"   only named : {len(named)} includers")
        print(f"   in full    : {len(full)} includers")
        print(f"   declaration: {forward_declaration(entry)}")
        for includer in named[:6]:
            print(f"      {blast.rel(includer, root)}")
        if len(named) > 6:
            print(f"      ... {len(named) - 6} more")
        report["types"].append({
            "name": entry["name"], "kind": entry["kind"],
            "namespace": entry["namespace"],
            "only_named": [blast.rel(i, root) for i in named],
            "in_full": [blast.rel(i, root) for i in full],
            "declaration": forward_declaration(entry),
        })
        print()

    if args.verify:
        db = cost.load_db(build_dir)
        usable = [e for e in db if cost.is_compile_entry(e)]
        flags = cost.union_flags(db)
        stubs = [forward_declaration(entry) for entry in types]
        stubs = [stub for stub in stubs if stub is not None]
        stub_text = stub_for(header, stubs)
        print("== verification: does the includer compile with the header "
              "replaced by forward declarations?")
        print(f"   stub holds {len(stubs)} declarations; a pass means the "
              f"includer needs no definition from this header")
        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            for entry in report["types"]:
                # A spread across the set, not the first N: the first N tend to
                # be one family of files that behave alike.
                candidates = entry["only_named"]
                if len(candidates) > args.limit:
                    step = len(candidates) / args.limit
                    candidates = [candidates[int(i * step)]
                                  for i in range(args.limit)]
                passed = failed = 0
                for relative in candidates:
                    if verify_candidate(root / relative, args.header,
                                        stub_text, root, flags, work):
                        passed += 1
                    else:
                        failed += 1
                print(f"{entry['name']}: {passed} compile, {failed} do not, "
                      f"of {len(candidates)} tried")
                entry["verified"] = {"passed": passed, "failed": failed}
        print()

    if args.json:
        Path(args.json).write_text(json.dumps(report, indent=2), encoding="utf-8")
        print(f"json written: {args.json}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
