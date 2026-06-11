#!/usr/bin/env python3
"""Generate profiles.json from facet_catalogue.org.

Usage:
    python generate_profiles.py [--check]

Without --check: writes library/profiles.json next to the org source.
With --check: compares generated output to the committed file via parsed JSON
and exits non-zero on mismatch (used by the CI drift check).
"""
import argparse
import json
import re
import sys
from pathlib import Path

_LIBRARY = Path(__file__).parent.parent / "library"
_SOURCE = _LIBRARY / "facet_catalogue.org"
_TARGET = _LIBRARY / "profiles.json"

_HEADING_RE = re.compile(r"^\*\* (.+?) :(\w+):$")
_PROP_KEY_RE = re.compile(r"^:(\w[\w-]*):\s+(.*?)\s*$")


def _parse_org_table(lines, pos):
    """Parse an org-mode table starting at lines[pos]. Returns (rows, next_pos).
    Each row is a list of stripped cell strings. Separator rows are skipped.
    """
    rows = []
    while pos < len(lines):
        line = lines[pos].strip()
        if not line.startswith("|"):
            break
        if re.match(r"^\|[-+|]+\|?$", line):
            pos += 1
            continue
        cells = [c.strip() for c in line.strip("|").split("|")]
        rows.append(cells)
        pos += 1
    return rows, pos


def _parse_facet_catalogue(source: Path) -> dict:
    lines = source.read_text(encoding="utf-8").splitlines()
    profiles = {}
    i = 0
    n = len(lines)

    while i < n:
        m = _HEADING_RE.match(lines[i])
        if not m:
            i += 1
            continue

        name = m.group(1)
        stereotype = m.group(2)
        i += 1

        # Read PROPERTIES drawer
        props = {}
        if i < n and lines[i].strip() == ":PROPERTIES:":
            i += 1
            while i < n and lines[i].strip() != ":END:":
                pm = _PROP_KEY_RE.match(lines[i].strip())
                if pm:
                    props[pm.group(1)] = pm.group(2)
                i += 1
            i += 1  # skip :END:

        if "description" not in props or "model_types" not in props:
            print(
                f"Warning: profile '{name}' missing required property "
                f"(description/model_types) — skipped",
                file=sys.stderr,
            )
            continue

        profile = {
            "description": props["description"],
            "model_types": props["model_types"].split(),
        }

        if "includes" in props:
            profile["includes"] = props["includes"].split()
        else:
            # Scan forward for a table, skipping only blank lines after :END:.
            # NOTE: a prose paragraph between :END: and the table would break
            # detection — keep no non-blank content between the drawer and the
            # template table when authoring new profiles.
            j = i
            while j < n and not lines[j].strip():
                j += 1
            if j < n and lines[j].strip().startswith("|"):
                rows, _ = _parse_org_table(lines, j)
                if rows:
                    headers = [h.lower() for h in rows[0]]
                    has_mt_col = "model types" in headers
                    templates = []
                    for row in rows[1:]:
                        entry = {
                            "template": row[0],
                            "output": row[1],
                        }
                        if has_mt_col and len(row) > 2 and row[2]:
                            entry["model_types"] = row[2].split()
                        templates.append(entry)
                    profile["templates"] = templates

        profiles[name] = profile

    return {"profiles": profiles}


def _write_profiles_json(data: dict, target: Path) -> None:
    target.write_text(json.dumps(data, indent=2) + "\n", encoding="utf-8")


def _check(data: dict, target: Path) -> bool:
    if not target.exists():
        print(f"ERROR: {target} does not exist", file=sys.stderr)
        return False
    committed = json.loads(target.read_text(encoding="utf-8"))
    if data == committed:
        return True
    print(f"DRIFT: generated profiles.json does not match {target}", file=sys.stderr)
    gen_names = set(data["profiles"])
    com_names = set(committed["profiles"])
    for name in sorted(gen_names - com_names):
        print(f"  + {name} (in generated, not in committed)", file=sys.stderr)
    for name in sorted(com_names - gen_names):
        print(f"  - {name} (in committed, not in generated)", file=sys.stderr)
    for name in sorted(gen_names & com_names):
        g = data["profiles"][name]
        c = committed["profiles"][name]
        if g != c:
            print(f"  ~ {name}: generated={g}  committed={c}", file=sys.stderr)
    return False


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true",
                    help="Compare to committed file; exit 1 on mismatch")
    args = ap.parse_args()

    data = _parse_facet_catalogue(_SOURCE)

    if args.check:
        return 0 if _check(data, _TARGET) else 1

    _write_profiles_json(data, _TARGET)
    print(f"Wrote {_TARGET} ({len(data['profiles'])} profiles)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
