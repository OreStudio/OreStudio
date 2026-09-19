#!/usr/bin/env python3
"""
Check that the codegen-eligible models of drift-free components still match
their committed generated output -- someone hand-edited a generated file
instead of its .org model source, or a model changed without running
`compass codegen regenerate` afterwards.

Two selectors choose what to check:

  --all           check every component in the known-drift-free registry
                  below; the local compass-pr-raise gate covers this set
  --component X   check one named component, by catalogue slug

Two modes choose whether it writes, and the difference is the point of
this script:

  (default)       WRITES. It regenerates each component in place and then
                  compares the working tree with `git diff`. This is the
                  authoritative check, and it is destructive: it overwrites
                  the component's generated files, and a model sitting in a
                  modeling directory materialises its whole output even when
                  nobody authorised it.
  --dry-run       WRITES NOTHING. It renders each component's models into a
                  throw-away temporary root and compares that tree with the
                  repository's files, reporting what would change and what
                  would be created. No byte lands in the repository: no
                  cmd_regenerate, no in-place generation, no temporary file
                  inside the tree. Reach for this one whenever a model is
                  being drafted, because a draft model in a modeling
                  directory is exactly what makes the in-place mode unsafe.

Both modes cover the same components and addresses. The TypeScript
projection is mid-rollout, one component at a time, so a component is
checked at a TypeScript facet only once that facet's output is committed
for it. The other technical spaces are checked as before. The facets and
their output patterns are read from the template library rather than listed
here, so a component joins the TypeScript side of the gate the moment its
first generated file lands, and no list needs maintaining. An explicit
--address bypasses the scoping and is used as given.

An ad hoc multi-component list is never a valid invocation: a component
whose committed tree predates a template receives the newer per-entity
families as untracked files on regeneration, and git diff cannot see
untracked files. The in-place check fails when regeneration materializes
untracked files that were not already in the tree, so a component joins
the registry only when its regeneration leaves the tree fully clean.
See the regen-byproduct-hygiene memory in doc/llm/memory/.

Usage:
  check_component_drift.py --all
  check_component_drift.py --component refdata
  check_component_drift.py --all --address ores.cpp
  check_component_drift.py --all --dry-run
"""
from __future__ import annotations

import argparse
import difflib
import io
import logging
import subprocess
import sys
import tempfile
from contextlib import redirect_stdout
from pathlib import Path
from types import SimpleNamespace

REPO_ROOT = Path(__file__).resolve().parents[3]
CODEGEN_DIR = REPO_ROOT / "projects" / "ores.codegen"
sys.path.insert(0, str(CODEGEN_DIR / "src"))

from codegen.generate import _generate_single, cmd_regenerate  # noqa: E402
from codegen.logging_config import configure  # noqa: E402
from codegen.manifest import discover_models, get_component  # noqa: E402
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


# The .clang-format copied into the temporary root so clang-format discovers
# the repository style. It is scaffolding, never generated output.
_SEEDED_CLANG_FORMAT = Path(".clang-format")

# Lines printed for a changed file before --verbose asks for the whole diff.
_FIRST_HUNK_LIMIT = 80


def _first_hunk(diff_lines: list, limit: int = _FIRST_HUNK_LIMIT) -> list:
    """The unified-diff header plus its first hunk, capped at ``limit`` lines."""
    out: list = []
    hunks = 0
    for line in diff_lines:
        if line.startswith("@@"):
            hunks += 1
            if hunks > 1:
                break
        out.append(line)
        if len(out) >= limit:
            break
    return out


def _compare_generated_tree(tmp_root: Path) -> tuple:
    """(would_change, would_create) repository-relative paths.

    The generated tree holds the same relative paths the in-place run would
    write. A repository file with different bytes would change; a path with
    no repository file would be created.
    """
    would_change: list = []
    would_create: list = []
    for path in sorted(tmp_root.rglob("*")):
        if not path.is_file():
            continue
        rel = path.relative_to(tmp_root)
        if rel == _SEEDED_CLANG_FORMAT:
            continue
        repo_path = REPO_ROOT / rel
        if not repo_path.is_file():
            would_create.append(rel)
        elif repo_path.read_bytes() != path.read_bytes():
            would_change.append(rel)
    return would_change, would_create


def _report_drift(tmp_root: Path, verbose: bool) -> int:
    """Print the would-change / would-create report and gate on it."""
    would_change, would_create = _compare_generated_tree(tmp_root)
    for rel in would_change:
        print(f"\nwould change: {rel}")
        repo_text = (REPO_ROOT / rel).read_text(encoding="utf-8", errors="replace")
        gen_text = (tmp_root / rel).read_text(encoding="utf-8", errors="replace")
        full = list(difflib.unified_diff(
            repo_text.splitlines(keepends=True),
            gen_text.splitlines(keepends=True),
            fromfile=f"a/{rel}", tofile=f"b/{rel}"))
        sys.stdout.writelines(full if verbose else _first_hunk(full))
    for rel in would_create:
        print(f"\nwould create: {rel}")

    if not would_change and not would_create:
        print("No drift: generated output matches the checked-in tree.")
        return 0

    total = len(would_change) + len(would_create)
    print(f"\n{len(would_change)} file(s) would change, "
          f"{len(would_create)} file(s) would be created "
          f"({total} file(s) in total).")
    print("The tree does not match its models. Run without --dry-run to "
          "apply the change, then commit the result.")
    return 1


def _render_components(components: list, address: str, tmp_root: Path) -> int:
    """Render every component/address into ``tmp_root``; write nothing in the repo.

    Each model goes through the same ``_generate_single`` the in-place mode
    uses, so the units are identical; ``output_root`` redirects the write
    target to the temporary root.
    """
    graph = load_graph(TEMPLATES_DIR)
    for component in components:
        try:
            comp = get_component(component)
        except ValueError as exc:
            print(f"{exc}", file=sys.stderr)
            return 1
        model_files = discover_models(comp, REPO_ROOT)
        if not model_files:
            print(f"No models found for component {component!r} "
                  f"(modeling_dir: {comp.modeling_dir or '(no modeling dir)'})",
                  file=sys.stderr)
            continue
        addresses = ([address] if address != "ores"
                     else _addresses_for(component, graph))
        for one_address in addresses:
            print(f"Checking component {component!r} at address "
                  f"{one_address!r} (dry run, no writes)...")
            for model_path in model_files:
                captured = io.StringIO()
                # The in-place run narrates every write; the dry run only
                # needs errors, so silence INFO and WARNING for the render.
                logging.disable(logging.WARNING)
                try:
                    with redirect_stdout(captured):
                        rc = _generate_single(
                            model_path, False, CODEGEN_DIR,
                            address=one_address, component_mode=True,
                            output_root=tmp_root)
                finally:
                    logging.disable(logging.NOTSET)
                if rc != 0:
                    print(f"Rendering failed for component {component!r} at "
                          f"address {one_address!r} ({model_path.name}).",
                          file=sys.stderr)
                    sys.stderr.write(captured.getvalue())
                    return rc
    return 0


def _dry_run(components: list, address: str, verbose: bool) -> int:
    """The safe mode: render to a temporary root, report, write nothing."""
    with tempfile.TemporaryDirectory(prefix="ores-codegen-dry-run-") as tmpdir:
        tmp_root = Path(tmpdir)
        # clang-format discovers .clang-format only by walking up from the
        # formatted file; a bare tempdir has no such ancestor, so without
        # this seed the diff would show the LLVM default style as drift.
        repo_clang_format = REPO_ROOT / ".clang-format"
        if repo_clang_format.is_file():
            (tmp_root / _SEEDED_CLANG_FORMAT).write_bytes(
                repo_clang_format.read_bytes())
        rc = _render_components(components, address, tmp_root)
        if rc != 0:
            return rc
        return _report_drift(tmp_root, verbose)


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
    ap.add_argument(
        "--dry-run",
        action="store_true",
        help="render into a temporary root and report every file that would "
        "change or be created; write nothing into the repository (safe while "
        "a model is being drafted)",
    )
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    configure(verbose=args.verbose)

    components = list(KNOWN_DRIFT_FREE) if args.all else [args.component]

    if args.dry_run:
        return _dry_run(components, args.address, args.verbose)

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
