"""
Convert ``ores_<...>_component.json`` codegen component models
to literate ``<full_name>_component.org`` files under each
component's ``modeling/`` directory.

Component models scaffold the per-package CMake projects + C++
stubs (root + src + tests + modeling CMakeLists, export.hpp,
component header, stub header/impl, test main, stub_tests.cpp).
The JSON shape is uniform across all 31 entries — four scalars
only: ``name``, ``full_name``, ``brief``, ``description``.

The org file mirrors the JSON one-for-one: frontmatter for the
three short scalars (``name``, ``full_name``, ``brief``) and the
free-form ``description`` as the document body. Codegen rounds
back to the same dict.

Output paths are derived from ``full_name``:

- ``ores.X``        → ``projects/ores.X/modeling/ores.X_component.org``
- ``ores.X.Y``      → ``projects/ores.X/Y/modeling/ores.X.Y_component.org``

This matches the post-regroup C++ layout where a leaf component
``ores.iam.service`` lives at ``projects/ores.iam/service/``.

Usage::

    python3 projects/ores.codegen/src/codegen/migrate_component_to_org.py \\
        --models-root projects/ores.codegen/models \\
        --projects-root projects
"""
import argparse
import json
import sys
import uuid
from datetime import date
from pathlib import Path


def _frontmatter(comp, fid):
    today = date.today().isoformat()
    lines = [
        f":PROPERTIES:\n:ID: {fid}\n:END:",
        f"#+title: {comp['full_name']}",
        f"#+description: Codegen component model for {comp['full_name']}.",
        "#+type: ores.codegen.component",
        "#+filetags: :codegen:component:literate:",
        f"#+created: {today}",
        f"#+updated: {today}",
        f"#+name: {comp['name']}",
        f"#+full_name: {comp['full_name']}",
        f"#+brief: {comp['brief']}",
    ]
    return "\n".join(lines)


def _output_path(projects_root: Path, full_name: str) -> Path:
    """ores.X        → projects/ores.X/modeling/ores.X_component.org
    ores.X.Y      → projects/ores.X/Y/modeling/ores.X.Y_component.org"""
    parts = full_name.split(".")
    if len(parts) == 2:
        # ores.X
        return projects_root / f"ores.{parts[1]}" / "modeling" / f"{full_name}_component.org"
    if len(parts) == 3:
        # ores.X.Y
        return projects_root / f"ores.{parts[1]}" / parts[2] / "modeling" / f"{full_name}_component.org"
    raise ValueError(f"Unsupported full_name shape: {full_name!r}")


def convert(model_path: Path, out_path: Path) -> Path:
    model = json.loads(model_path.read_text(encoding="utf-8"))
    comp = model["component"]
    fid = str(uuid.uuid4()).upper()
    parts = [_frontmatter(comp, fid), ""]
    description = comp.get("description")
    if description:
        parts += [description.rstrip(), ""]
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text("\n".join(parts).rstrip() + "\n", encoding="utf-8")
    return out_path


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--models-root", required=True, type=Path)
    p.add_argument("--projects-root", required=True, type=Path)
    args = p.parse_args(argv)

    for json_path in sorted(args.models_root.rglob("*_component.json")):
        full_name = json.loads(json_path.read_text(encoding="utf-8"))["component"]["full_name"]
        out_path = _output_path(args.projects_root, full_name)
        convert(json_path, out_path)
        print(f"Wrote {out_path}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
