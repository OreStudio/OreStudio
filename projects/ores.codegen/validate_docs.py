#!/usr/bin/env python3
"""
Validate ORE Studio component documentation structure.

Checks every projects/ores.*/ directory, including its sub-component
(composite part) directories, for:
  - modeling/component_overview.org presence wherever modeling/ holds a
    component overview
  - Required v2 frontmatter (:ID:, #+type: ores.codegen.component,
    #+description:) and required sections (Summary, Inputs, Outputs,
    Entry points, Dependencies, See also) on every component overview
  - At least one .puml file in the modeling/ dir of every component
    overview
  - Top-level component names: no ores.<a>.<b> sibling of an existing
    ores.<a> component (NAME_COLLISION)

A modeling/ directory at the root of a composite component (one whose
parts each carry their own component overview) is a group-level index
-- sections Sub-components, Entity modules, no #+type: ores.codegen.component
requirement -- and is exempt.  Component docs are then only optional
there; the parts are what the per-component checks validate.
Sub-component overviews are named by their fully-qualified component
name (ores.<group>.<part>) in output and in the exceptions file.

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

COMPONENT_TYPE_RE = re.compile(r"#\+type:\s*(?:ores\.codegen\.)?component", re.IGNORECASE)

MISSING_OVERVIEW     = "MISSING_OVERVIEW"
MISSING_ID           = "MISSING_ID"
MISSING_TYPE         = "MISSING_TYPE"
MISSING_DESCRIPTION  = "MISSING_DESCRIPTION"
MISSING_SECTION      = "MISSING_SECTION"
MISSING_PUML         = "MISSING_PUML"
NAME_COLLISION       = "NAME_COLLISION"


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


def check_component_overview(modeling_dir: Path, owner: str) -> list[tuple[str, str, str]]:
    """
    Run the per-component checks on one component overview.
    Returns list of (check_code, component_name, detail) tuples.
    """
    overview = modeling_dir / "component_overview.org"
    text = overview.read_text(encoding="utf-8")
    violations: list[tuple[str, str, str]] = []

    if not re.search(r":ID:\s+\S+", text):
        violations.append((MISSING_ID, owner, f"{owner}: no :ID: UUID in :PROPERTIES: block"))

    if not COMPONENT_TYPE_RE.search(text):
        violations.append((
            MISSING_TYPE,
            owner,
            f"{owner}: #+type: component (or ores.codegen.component) not found",
        ))

    m = re.search(r"#\+description:\s*(.+)", text)
    if not m or not m.group(1).strip():
        violations.append((MISSING_DESCRIPTION, owner, f"{owner}: #+description: missing or empty"))

    missing = [
        s for s in REQUIRED_SECTIONS
        if not re.search(r"^\*\s+" + re.escape(s) + r"\s*$", text, re.MULTILINE)
    ]
    if missing:
        violations.append((
            MISSING_SECTION,
            owner,
            f"{owner}: missing section(s): {', '.join(missing)}",
        ))

    if not list(modeling_dir.glob("*.puml")):
        violations.append((MISSING_PUML, owner, f"{owner}: no .puml file in modeling/"))

    return violations


def part_dirs(component_dir: Path) -> list[Path]:
    """Immediate sub-directories of the component (its composite parts)."""
    return sorted(
        d for d in component_dir.iterdir()
        if d.is_dir() and d.name != "modeling"
    )


def composite_with_part_overviews(component_dir: Path) -> bool:
    """True when at least one part dir carries its own component overview."""
    for part in part_dirs(component_dir):
        overview = part / "modeling" / "component_overview.org"
        if overview.exists():
            return True
    return False


def check_name_collisions(components: list[Path]) -> list[tuple[str, str, str]]:
    """
    Flag dotted top-level names that extend an existing component name.
    Example: projects/ores.<group>.<part>/ next to projects/ores.<group>/,
    which claims a parentage the tree does not have.
    """
    names = {c.name for c in components}
    violations: list[tuple[str, str, str]] = []
    for component in components:
        parts = component.name.split(".")
        for i in range(2, len(parts)):
            prefix = ".".join(parts[:i])
            if prefix in names:
                violations.append((
                    NAME_COLLISION,
                    component.name,
                    f"{component.name}: top-level sibling of component {prefix}; "
                    "absorb it into that component as a sub-component or rename it",
                ))
                break
    return violations


def main() -> int:
    script_dir = Path(__file__).resolve().parent
    projects_dir = script_dir.parent
    exceptions = load_exceptions(script_dir / "docs_exceptions.txt")

    components = sorted(
        d for d in projects_dir.iterdir()
        if d.is_dir() and d.name.startswith("ores.")
    )

    violations: list[tuple[str, str, str]] = []
    for component_dir in components:
        grouped = composite_with_part_overviews(component_dir)

        # The root modeling/ dir of a composite is a group-level index:
        # nothing is enforced there beyond what the parts already carry.
        if not grouped:
            root_modeling = component_dir / "modeling"
            if root_modeling.is_dir():
                if not (root_modeling / "component_overview.org").exists():
                    violations.append((
                        MISSING_OVERVIEW,
                        component_dir.name,
                        f"{component_dir.name}: modeling/component_overview.org not found",
                    ))
                else:
                    violations.extend(
                        check_component_overview(root_modeling, component_dir.name)
                    )

        for part in part_dirs(component_dir):
            modeling_dir = part / "modeling"
            if not modeling_dir.is_dir():
                continue
            owner = f"{component_dir.name}.{part.name}"
            if not (modeling_dir / "component_overview.org").exists():
                violations.append((
                    MISSING_OVERVIEW,
                    owner,
                    f"{owner}: modeling/component_overview.org not found",
                ))
            else:
                violations.extend(check_component_overview(modeling_dir, owner))

    violations.extend(check_name_collisions(components))

    violations = [
        (code, name, detail) for code, name, detail in violations
        if (code, name) not in exceptions
    ]

    if not violations:
        print(f"OK: all {len(components)} components pass documentation checks.")
        return 0

    print(f"FAIL: {len(violations)} violation(s) found:\n")
    for code, _name, detail in violations:
        print(f"  [{code}] {detail}")
    print()
    return 1


if __name__ == "__main__":
    sys.exit(main())
