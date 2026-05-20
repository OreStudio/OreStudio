#!/usr/bin/env python3
"""
Validate ORE Studio component documentation structure.

Checks every projects/ores.*/ directory for:
  - modeling/component_overview.org presence
  - Required v2 frontmatter (:ID:, #+type: component, #+description:)
  - Required sections (Summary, Inputs, Outputs, Entry points,
    Dependencies, See also)
  - At least one .puml file in modeling/

Exits 0 if all checks pass, 1 if any violations are found.

Exceptions are read from docs_exceptions.txt in the same directory as
this script.  Format: CHECK_CODE component_name (one per line).
"""

import re
import sys
from pathlib import Path


REQUIRED_SECTIONS = [
    "Summary",
    "Inputs",
    "Outputs",
    "Entry points",
    "Dependencies",
    "See also",
]

MISSING_OVERVIEW     = "MISSING_OVERVIEW"
MISSING_ID           = "MISSING_ID"
MISSING_TYPE         = "MISSING_TYPE"
MISSING_DESCRIPTION  = "MISSING_DESCRIPTION"
MISSING_SECTION      = "MISSING_SECTION"
MISSING_PUML         = "MISSING_PUML"


def load_exceptions(path: Path) -> set[tuple[str, str]]:
    """Return set of (check_code, component_name) pairs to suppress."""
    result: set[tuple[str, str]] = set()
    if not path.exists():
        return result
    for line in path.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.split(None, 1)
        if len(parts) == 2:
            result.add((parts[0], parts[1]))
    return result


def check_component(component_dir: Path) -> list[tuple[str, str]]:
    """
    Run all checks on one component directory.
    Returns list of (check_code, human-readable detail) tuples.
    """
    violations: list[tuple[str, str]] = []
    name = component_dir.name
    modeling_dir = component_dir / "modeling"

    if not modeling_dir.is_dir():
        return []

    overview = modeling_dir / "component_overview.org"
    if not overview.exists():
        violations.append((
            MISSING_OVERVIEW,
            f"{name}: modeling/component_overview.org not found",
        ))
        return violations  # remaining checks all require the file

    text = overview.read_text(encoding="utf-8")

    if not re.search(r":ID:\s+\S+", text):
        violations.append((MISSING_ID, f"{name}: no :ID: UUID in :PROPERTIES: block"))

    if not re.search(r"#\+type:\s*component", text, re.IGNORECASE):
        violations.append((MISSING_TYPE, f"{name}: #+type: component not found"))

    m = re.search(r"#\+description:\s*(.+)", text)
    if not m or not m.group(1).strip():
        violations.append((MISSING_DESCRIPTION, f"{name}: #+description: missing or empty"))

    missing = [
        s for s in REQUIRED_SECTIONS
        if not re.search(r"^\*\s+" + re.escape(s) + r"\s*$", text, re.MULTILINE)
    ]
    if missing:
        violations.append((
            MISSING_SECTION,
            f"{name}: missing section(s): {', '.join(missing)}",
        ))

    if not list(modeling_dir.glob("*.puml")):
        violations.append((MISSING_PUML, f"{name}: no .puml file in modeling/"))

    return violations


def main() -> int:
    script_dir = Path(__file__).resolve().parent
    projects_dir = script_dir.parent
    exceptions = load_exceptions(script_dir / "docs_exceptions.txt")

    components = sorted(
        d for d in projects_dir.iterdir()
        if d.is_dir() and d.name.startswith("ores.")
    )

    violations: list[tuple[str, str]] = []
    for component_dir in components:
        for code, detail in check_component(component_dir):
            if (code, component_dir.name) not in exceptions:
                violations.append((code, detail))

    if not violations:
        print(f"OK: all {len(components)} components pass documentation checks.")
        return 0

    print(f"FAIL: {len(violations)} violation(s) found:\n")
    for code, detail in violations:
        print(f"  [{code}] {detail}")
    print()
    return 1


if __name__ == "__main__":
    sys.exit(main())
