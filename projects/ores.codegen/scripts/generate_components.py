#!/usr/bin/env python3
"""Generate components.json from component_catalogue.org.

Usage:
    python generate_components.py [--check]

Without --check: writes library/components.json next to the org source.
With --check: compares generated output to the committed file via parsed JSON
and exits non-zero on mismatch (used by the CI drift check).
"""
import argparse
import json
import re
import sys
from pathlib import Path

_LIBRARY = Path(__file__).parent.parent / "library"
_SOURCE = _LIBRARY / "component_catalogue.org"
_TARGET = _LIBRARY / "components.json"


def _parse_org_table(lines):
    """Parse all rows from a flat list of org table lines.
    Separator rows (|----|) are skipped. Returns list of cell-string lists.
    """
    rows = []
    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("|"):
            break
        if re.match(r"^\|[-+|]+\|?$", stripped):
            continue
        cells = [c.strip() for c in stripped.strip("|").split("|")]
        rows.append(cells)
    return rows


def _parse_component_catalogue(source: Path) -> dict:
    text = source.read_text(encoding="utf-8")
    lines = text.splitlines()

    # Find the table inside the * Components section to avoid false matches
    # from any other table that may appear earlier in the file.
    table_lines = []
    in_section = False
    in_table = False
    for line in lines:
        stripped = line.strip()
        if stripped == "* Components":
            in_section = True
            continue
        if in_section and stripped.startswith("* "):
            break  # left the section
        if not in_table:
            if in_section and stripped.startswith("| name"):
                in_table = True
                table_lines.append(line)
        else:
            if stripped.startswith("|"):
                table_lines.append(line)
            else:
                break

    rows = _parse_org_table(table_lines)
    if not rows:
        raise ValueError(f"No component table found in {source}")

    headers = [h.lower() for h in rows[0]]
    components = {}
    for row in rows[1:]:
        entry = dict(zip(headers, row))
        name = entry["name"]
        excl = entry.get("exclude_suffix", "") or None
        components[name] = {
            "models_dir": entry["models_dir"],
            "entity_glob": entry["entity_glob"],
            # exclude_suffix is explicit null (not omitted) to distinguish
            # "no exclusion" (*-cpp components) from "use default exclusion".
            "exclude_suffix": excl,
            # modeling_dir is omitted entirely when absent; the loader's
            # .get() returns None, which is the correct "no second root" signal.
            "modeling_dir": entry.get("modeling_dir") or None,
        }
        if components[name]["modeling_dir"] is None:
            del components[name]["modeling_dir"]

    return {"components": components}


def _write_components_json(data: dict, target: Path) -> None:
    # indent=4 matches the pre-existing components.json formatting; do not
    # normalise to indent=2 (profiles.json uses 2 — they are intentionally different).
    target.write_text(json.dumps(data, indent=4) + "\n", encoding="utf-8")


def _check(data: dict, target: Path) -> bool:
    if not target.exists():
        print(f"ERROR: {target} does not exist", file=sys.stderr)
        return False
    committed = json.loads(target.read_text(encoding="utf-8"))
    if data == committed:
        return True
    print(f"DRIFT: generated components.json does not match {target}", file=sys.stderr)
    gen_names = set(data["components"])
    com_names = set(committed["components"])
    for name in sorted(gen_names - com_names):
        print(f"  + {name} (in generated, not in committed)", file=sys.stderr)
    for name in sorted(com_names - gen_names):
        print(f"  - {name} (in committed, not in generated)", file=sys.stderr)
    for name in sorted(gen_names & com_names):
        g = data["components"][name]
        c = committed["components"][name]
        if g != c:
            print(f"  ~ {name}: generated={g}  committed={c}", file=sys.stderr)
    return False


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true",
                    help="Compare to committed file; exit 1 on mismatch")
    args = ap.parse_args()

    data = _parse_component_catalogue(_SOURCE)

    if args.check:
        return 0 if _check(data, _TARGET) else 1

    _write_components_json(data, _TARGET)
    print(f"Wrote {_TARGET} ({len(data['components'])} components)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
