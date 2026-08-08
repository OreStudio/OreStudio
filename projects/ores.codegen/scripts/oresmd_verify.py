#!/usr/bin/env python3
"""Verify oresmd codegen output against hand-crafted files.

Loads all oresmd spec org files, renders the Mustache templates,
and checks that the output matches the committed source files byte-for-byte
(after clang-format normalisation).
"""

import argparse
import difflib
import re
import subprocess
import sys
from pathlib import Path
from typing import Any

import pystache


def repo_root() -> Path:
    current = Path(__file__).resolve()
    for parent in current.parents:
        if (parent / ".git").exists():
            return parent
    raise RuntimeError("Repository root not found")


def load_oresmd_specs(spec_dir: Path, root: Path) -> list[dict[str, Any]]:
    """Load all oresmd quote type spec org files."""
    # The codegen Python package lives under projects/ores.codegen/src/.
    sys.path.insert(0, str(root / "projects" / "ores.codegen" / "src"))
    from codegen.org_loader import load_org_oresmd_quote_type_model

    specs = []
    for path in sorted(spec_dir.glob("*_quote_type.org")):
        model = load_org_oresmd_quote_type_model(path)
        qt = model["oresmd_quote_type"]

        # Add display-friendly enum names from the quote type names.
        for q in qt.get("quote_types", []):
            q["enum_name"] = q.get("enum_name", q.get("name", ""))
            if "notes" in q:
                q["notes"] = [q["notes"]]
            else:
                q["notes"] = []

        specs.append(qt)
    return specs


def _extract_mustache(org_path: Path) -> str:
    """Extract the Mustache template from an archetype org file's source block."""
    text = org_path.read_text(encoding="utf-8")
    # Find the first #+begin_src mustache ... #+end_src block.
    m = re.search(r'#\+begin_src mustache.*?\n(.*?)#\+end_src', text, re.DOTALL)
    if not m:
        raise ValueError(f"No mustache source block found in {org_path}")
    return m.group(1)


def render_enums(specs: list[dict], template_dir: Path) -> str:
    """Render oresmd_enums.hpp from template + specs."""
    template_path = template_dir / "ores.marketdata.oresmd" / "enums.org"
    mustache = _extract_mustache(template_path)
    context = {"oresmd_quote_types": specs}
    return pystache.render(mustache, context)


def clang_format(text: str) -> str:
    """Run clang-format on text and return the result."""
    proc = subprocess.run(
        ["clang-format", "-style=file"],
        input=text,
        capture_output=True,
        text=True,
    )
    if proc.returncode != 0:
        raise RuntimeError(f"clang-format failed: {proc.stderr}")
    return proc.stdout


def main() -> None:
    parser = argparse.ArgumentParser(description="Verify oresmd codegen output")
    parser.add_argument("--check", action="store_true",
                        help="Exit non-zero if generated output differs from committed files")
    parser.add_argument("--print", action="store_true",
                        help="Print generated output instead of diffing")
    args = parser.parse_args()

    root = repo_root()
    spec_dir = root / "projects" / "ores.marketdata" / "modeling" / "oresmd"
    template_dir = root / "projects" / "ores.codegen" / "library" / "templates"
    enums_path = root / "projects" / "ores.marketdata" / "api" / "include" / \
                 "ores.marketdata.api" / "domain" / "oresmd_enums.hpp"

    specs = load_oresmd_specs(spec_dir, root)
    if not specs:
        print("No oresmd spec files found.", file=sys.stderr)
        sys.exit(1)

    print(f"Loaded {len(specs)} spec(s): {[s['asset_class'] for s in specs]}")

    # Render enums.
    generated = render_enums(specs, template_dir)
    generated = clang_format(generated)

    if args.print:
        print(generated)
        return

    committed = enums_path.read_text(encoding="utf-8")
    committed = clang_format(committed)

    if generated == committed:
        print("✅ oresmd_enums.hpp: zero diffs (byte-identical after clang-format)")
    else:
        print("❌ oresmd_enums.hpp: diffs found")
        diff = difflib.unified_diff(
            committed.splitlines(keepends=True),
            generated.splitlines(keepends=True),
            fromfile="oresmd_enums.hpp (committed)",
            tofile="oresmd_enums.hpp (generated)",
        )
        sys.stdout.writelines(diff)
        if args.check:
            sys.exit(1)


if __name__ == "__main__":
    main()
