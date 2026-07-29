#!/usr/bin/env python3
"""
Regenerate src/component_files.cmake and tests/component_files.cmake for
components that have a live modeling/component_overview.org -- the
explicit, checked-in `set(files ...)` list that CMakeLists.txt
`include()`s in place of `file(GLOB_RECURSE ...)`.

Renders the ores.cmake.component.files_{src,tests} archetypes
(projects/ores.codegen/library/templates/cmake_component_files_
{src,tests}.mustache) via the same generate_from_model() the rest of
codegen uses, so the file list is a directory scan injected into the
render context (see core.py's _COMPONENT_FILES_TEMPLATES branch), not
free-hand string formatting.

Deliberately NOT routed through `compass codegen regenerate`: that
command regenerates every archetype in the ores.cmake.component facet
for a component, including root/src/tests CMakeLists.txt -- which have
been hand-customised (extra target_link_libraries, etc.) since their
one-shot scaffold and would be silently clobbered by a full facet
regenerate. This script renders only the two files archetypes,
touching nothing else.

Usage:
  regenerate_cmake_component_files.py --all
  regenerate_cmake_component_files.py --component ores.assets.core
  regenerate_cmake_component_files.py --component ores.assets.core --check
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
CODEGEN_DIR = REPO_ROOT / "projects" / "ores.codegen"
sys.path.insert(0, str(CODEGEN_DIR / "src"))

from codegen.core import generate_from_model  # noqa: E402

DATA_DIR = CODEGEN_DIR / "library" / "data"
TEMPLATES_DIR = CODEGEN_DIR / "library" / "templates"

_FULL_NAME_RE = re.compile(r"^#\+full_name:\s*(\S+)\s*$", re.MULTILINE)

_UNITS = (
    ("cmake_component_files_src.mustache", "src", "component_files.cmake"),
    ("cmake_component_files_tests.mustache", "tests", "component_files.cmake"),
)


def discover_models() -> dict[str, Path]:
    """component full_name -> its modeling/component_overview.org path."""
    out: dict[str, Path] = {}
    for path in sorted((REPO_ROOT / "projects").glob("**/modeling/component_overview.org")):
        text = path.read_text(encoding="utf-8")
        m = _FULL_NAME_RE.search(text)
        if m:
            out[m.group(1)] = path
    return out


def regenerate_one(model_path: Path, check: bool) -> list[Path]:
    """Render both component_files units for one component; return files touched."""
    touched: list[Path] = []
    component_root = model_path.parent.parent
    for template_name, subdir, filename in _UNITS:
        out_dir = component_root / subdir
        # Not every modeled component is C++ (ores.compass, ores.lisp,
        # ores.seeder are Python/Elisp tooling with a component_overview.org
        # for the physical-space graph but no CMakeLists.txt) -- only emit
        # into a dir that's actually a CMake target.
        if not out_dir.is_dir() or not (out_dir / "CMakeLists.txt").exists():
            continue
        output_path = out_dir / filename
        before = output_path.read_text(encoding="utf-8") if output_path.exists() else None
        if check:
            # Render into a scratch copy so --check never writes.
            rc = generate_from_model(
                str(model_path), DATA_DIR, TEMPLATES_DIR, out_dir,
                is_processing_batch=False,
                target_template=template_name, target_output=filename + ".check-tmp",
            )
            tmp = out_dir / (filename + ".check-tmp")
            after = tmp.read_text(encoding="utf-8") if tmp.exists() else None
            if tmp.exists():
                tmp.unlink()
            if rc:
                continue
        else:
            rc = generate_from_model(
                str(model_path), DATA_DIR, TEMPLATES_DIR, out_dir,
                is_processing_batch=False,
                target_template=template_name, target_output=filename,
            )
            after = output_path.read_text(encoding="utf-8") if output_path.exists() else None
        if rc:
            continue
        if before != after:
            touched.append(output_path)
    return touched


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    group = ap.add_mutually_exclusive_group(required=True)
    group.add_argument("--all", action="store_true", help="Regenerate every modeled component.")
    group.add_argument("--component", help="Regenerate one component (its #+full_name:, e.g. ores.assets.core).")
    ap.add_argument("--check", action="store_true",
                    help="Exit non-zero if any file is stale; write nothing.")
    args = ap.parse_args(argv)

    models = discover_models()
    if args.component:
        if args.component not in models:
            print(f"Unknown component (no modeling/component_overview.org with "
                  f"#+full_name: {args.component}): known = "
                  f"{', '.join(sorted(models)) or '(none)'}", file=sys.stderr)
            return 1
        targets = {args.component: models[args.component]}
    else:
        targets = models

    stale: list[Path] = []
    for full_name, model_path in sorted(targets.items()):
        try:
            touched = regenerate_one(model_path, args.check)
        except Exception as exc:  # noqa: BLE001 — one bad model must not abort the batch
            print(f"  [skip] {full_name} ({model_path.relative_to(REPO_ROOT)}): {exc}",
                  file=sys.stderr)
            continue
        for p in touched:
            stale.append(p)
            verb = "stale" if args.check else "wrote"
            print(f"{verb}: {p.relative_to(REPO_ROOT)}")

    if args.check and stale:
        print(f"\n{len(stale)} file(s) would change. Run without --check to fix.",
              file=sys.stderr)
        return 1
    if not stale:
        print("All component_files.cmake up to date.")
    else:
        print(f"\n{len(stale)} file(s) updated.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
