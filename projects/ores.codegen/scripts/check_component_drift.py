#!/usr/bin/env python3
"""
Regenerate the codegen-eligible models of drift-free components and fail if
the working tree ends up dirty -- someone hand-edited a generated file
instead of its .org model source, or a model changed without running
`compass codegen regenerate` afterwards.

The check runs in exactly two modes:

  --all           regenerate every component in the known-drift-free
                  registry below; the local compass-pr-raise gate covers this set
  --component X   regenerate one named component, by catalogue slug

An ad hoc multi-component list is never a valid invocation: a component
whose committed tree predates a template receives the newer per-entity
families as untracked files on regeneration, and git diff cannot see
untracked files. The check fails when regeneration materializes
untracked files that were not already in the tree, so a component joins
the registry only when its regeneration leaves the tree fully clean.
See the regen-byproduct-hygiene memory in doc/llm/memory/.

The TypeScript projection is mid-rollout, one component at a time, so the
check regenerates a component at a TypeScript facet only once that facet's
output is committed for it. The other technical spaces are regenerated as
before. The facets and their output patterns are read from the template
library rather than listed here, so a component joins the TypeScript side
of the gate the moment its first generated file lands, and no list needs
maintaining. An explicit --address bypasses the scoping and is used as
given.

Usage:
  check_component_drift.py --all
  check_component_drift.py --component refdata
  check_component_drift.py --all --address ores.cpp
"""
from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path
from types import SimpleNamespace

REPO_ROOT = Path(__file__).resolve().parents[3]
CODEGEN_DIR = REPO_ROOT / "projects" / "ores.codegen"
sys.path.insert(0, str(CODEGEN_DIR / "src"))

from codegen.generate import cmd_regenerate  # noqa: E402
from codegen.logging_config import configure  # noqa: E402
from codegen.physical_space import load_graph  # noqa: E402

# Components verified to regenerate byte-identical to their committed
# tree, with no untracked materialization. --all checks exactly this
# set, the drift gate the compass-pr-raise skill runs. A component joins only
# when compass-codegen-fix-drift step 7 verifies its regeneration leaves the
# tree fully clean.
KNOWN_DRIFT_FREE = (
    "analytics-cpp",
    "refdata",
    "reporting",
    "marketdata",
    "compute-cpp",
    "iam",
    "iam-cpp",
    "synthetic",
    "shell",
)

TEMPLATES_DIR = CODEGEN_DIR / "library" / "templates"

# The technical space the gate scopes per component while its projection
# rolls out; every other space is regenerated for every component.
TS_SPACE = "ores.ts"


def _technical_spaces(graph) -> list:
    """Every technical space the template library declares, e.g. ``ores.cpp``."""
    return sorted({".".join(address.split(".")[:2])
                   for address in graph.facet_archetypes})


def _component_dir_names(component: str) -> list:
    """Directory names a component's generated files land under.

    A catalogue name may carry a suffix that its models do not, e.g. the
    ``compute-cpp`` component generates into ``generated/compute`` because
    its models declare ``#+component: compute``. Both spellings are tried.
    """
    names = [component]
    if component.endswith("-cpp"):
        names.append(component[: -len("-cpp")])
    return names


def _committed_ts_facets(component: str, graph) -> list:
    """TypeScript facets whose output this component has already committed.

    A facet counts only when its output directory exists for the component
    and holds a file. Regenerating a facet a component has not committed
    would materialise files nobody has reviewed and fail the check for a
    reason that has nothing to do with drift -- which is what a hierarchical
    entity does today, since its response carries a hand-written utility type
    with no projection.
    """
    names = _component_dir_names(component)
    committed = set()
    for facet, archetypes in sorted(graph.facet_archetypes.items()):
        if not facet.startswith(f"{TS_SPACE}."):
            continue
        for archetype in archetypes:
            pattern = archetype.get("output") or ""
            if not pattern:
                continue
            for name in names:
                # Only {component} is substituted: {entity} stands for the
                # file name, and the directory above it is what is committed.
                directory = pattern.replace("{component}", name).rsplit("/", 1)[0]
                path = REPO_ROOT / directory
                if path.is_dir() and any(path.iterdir()):
                    committed.add(facet)
                    break
    return sorted(committed)


def _addresses_for(component: str, graph) -> list:
    """Addresses to regenerate for one component.

    Every technical space the library declares except TypeScript, plus the
    TypeScript facets this component has committed.
    """
    addresses = [space for space in _technical_spaces(graph) if space != TS_SPACE]
    addresses.extend(_committed_ts_facets(component, graph))
    return addresses


def _untracked_files() -> set:
    ls = subprocess.run(
        ["git", "ls-files", "--others", "--exclude-standard"],
        cwd=REPO_ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    if ls.returncode != 0:
        print(f"git ls-files --others failed:\n{ls.stderr}", file=sys.stderr)
        sys.exit(ls.returncode)
    return {line for line in ls.stdout.splitlines() if line}


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    modes = ap.add_mutually_exclusive_group(required=True)
    modes.add_argument(
        "--all",
        action="store_true",
        help="regenerate every known-drift-free component "
        f"({', '.join(KNOWN_DRIFT_FREE)})",
    )
    modes.add_argument(
        "--component",
        metavar="NAME",
        help="regenerate one component by catalogue slug (e.g. refdata)",
    )
    ap.add_argument("--address", default="ores", metavar="ADDRESS")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    configure(verbose=args.verbose)

    components = list(KNOWN_DRIFT_FREE) if args.all else [args.component]
    untracked_before = _untracked_files()
    graph = load_graph(TEMPLATES_DIR)

    for component in components:
        addresses = ([args.address] if args.address != "ores"
                     else _addresses_for(component, graph))
        for address in addresses:
            print(f"Regenerating component {component!r} at address {address!r}...")
            regen_args = SimpleNamespace(
                component=component, all=False, address=address,
                entity=None, dry_run=False,
            )
            rc = cmd_regenerate(regen_args, CODEGEN_DIR)
            if rc != 0:
                print(f"codegen regenerate failed for component {component!r} "
                      f"at address {address!r}", file=sys.stderr)
                materialized = sorted(_untracked_files() - untracked_before)
                if materialized:
                    print("The failed run materialized untracked files "
                          "(sweep these before retrying):", file=sys.stderr)
                    for path in materialized:
                        print(f"  {path}", file=sys.stderr)
                return rc

    failures = 0
    diff = subprocess.run(["git", "diff"], cwd=REPO_ROOT, check=False,
                          capture_output=True, text=True)
    if diff.stdout:
        stat = subprocess.run(["git", "diff", "--stat"], cwd=REPO_ROOT, check=False,
                              capture_output=True, text=True)
        print(f"\n--- drifted file(s) ---\n{stat.stdout}", file=sys.stderr)
        print(f"--- unified diff ---\n{diff.stdout}", file=sys.stderr)
        print(
            "Generated output does not match what's checked in -- a "
            "generated file was hand-edited, or its .org model changed "
            "without running `compass codegen regenerate` afterwards. "
            "Run the regenerate command locally and commit the result.",
            file=sys.stderr,
        )
        failures += 1

    materialized = sorted(_untracked_files() - untracked_before)
    if materialized:
        print("\n--- untracked file(s) materialized by regeneration ---",
              file=sys.stderr)
        for path in materialized:
            print(f"  {path}", file=sys.stderr)
        print(
            "Regeneration materialized untracked files: the component's "
            "committed tree predates a template family, or a generated "
            "file was never committed. git diff cannot see untracked "
            "files, so the check fails on them explicitly. Commit the "
            "files or bring the component to a fully committed state "
            "before it can pass.",
            file=sys.stderr,
        )
        failures += 1

    if failures:
        return 1

    print("No drift: regenerated output matches the checked-in tree.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
