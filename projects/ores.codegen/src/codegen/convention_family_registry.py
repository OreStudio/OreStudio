"""Derive the convention family registry from the refdata modelling tree.

A convention entity model joins the registry by declaring the family key the
conventions document carries for it::

    #+type: ores.codegen.entity
    #+convention_family: ois

The declaration lives on the entity the family is, so adding a convention
family is one model edit and never an edit to a central list. Scope is derived
from the model: a family whose columns include ``party_id`` is party-scoped,
one without is world-scoped. The domain type, the protocol subject stem and
the entity name all follow from the frontmatter and the entity name.

A family's write mode follows from its scope, except where the document writes
it by row key rather than by party: such a model declares
``#+convention_write: by_id``. The mode is ``party`` by default, ``world`` when
the table has no ``party_id`` column, and ``by_id`` when declared.

This module derives the list and nothing else. The three C++ artefacts that
state it -- the registry header, the apply fragment and the includes fragment
-- are rendered from the ``ores.cpp.convention-registry`` facet, which the
generator feeds with :func:`codegen.core.convention_family_context`. Python
therefore owns the derivation and the templates own the rendering, so the
output has one source and the templates are editable like every other
generated artefact.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

from .manifest import discover_models, get_component

COMPONENT = "refdata"
MARKER = "convention_family"

WRITE_MODE_KEYWORD = "convention_write"
DECLARABLE_WRITE_MODES = ("party", "by_id")

_FRONTMATTER_RE = re.compile(r"^#\+([A-Za-z0-9_-]+):\s*(.*?)\s*$")
_ENTRY_RE = re.compile(
    r'\{\s*"([^"]*)",\s*"([^"]*)",\s*"([^"]*)",\s*"([^"]*)",\s*'
    r'(true|false),\s*convention_write_mode::([a-z_]+)\s*\}'
)


@dataclass(frozen=True)
class FamilyEntry:
    """One convention family as the registry states it."""

    key: str
    entity: str
    domain_type: str
    subject_stem: str
    party_scoped: bool
    write_mode: str


def _frontmatter(path: Path) -> dict[str, str]:
    """The file's #+key: frontmatter values, before the first heading."""
    values: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if line.startswith("*"):
            break
        match = _FRONTMATTER_RE.match(line)
        if match:
            values[match.group(1).lower()] = match.group(2)
    return values


def _column_names(path: Path) -> set[str]:
    """The column headings of the file's * Columns section."""
    columns: set[str] = set()
    in_columns = False
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if line.startswith("* "):
            in_columns = line.strip() == "* Columns"
            continue
        if in_columns and line.startswith("** "):
            columns.add(line[3:].strip().split()[0])
    return columns


def _write_mode(frontmatter: dict[str, str], party_scoped: bool, path: Path) -> str:
    """The write mode a model declares, or the one its scope implies.

    ``world`` is never declared: a table without a party_id column is written
    by adding only the rows the tenant does not already hold.
    """
    declared = (frontmatter.get(WRITE_MODE_KEYWORD) or "").strip()
    if not declared:
        return "party" if party_scoped else "world"
    if declared not in DECLARABLE_WRITE_MODES:
        raise ValueError(
            f"{path}: #+{WRITE_MODE_KEYWORD}: {declared!r} is not one of"
            f" {', '.join(DECLARABLE_WRITE_MODES)}; world follows from the"
            " absence of a party_id column"
        )
    if declared == "party" and not party_scoped:
        raise ValueError(
            f"{path}: #+{WRITE_MODE_KEYWORD}: party requires a party_id column"
        )
    return declared


def _entry_for(path: Path, key: str) -> FamilyEntry:
    """The registry entry a marked entity model declares."""
    frontmatter = _frontmatter(path)
    entity = (frontmatter.get("entity_singular") or path.stem.split(".")[-1]).strip()
    component = (frontmatter.get("component") or "").strip()
    plural = (frontmatter.get("entity_plural") or f"{entity}s").strip()
    if not component:
        raise ValueError(f"{path}: a marked convention family must declare #+component:")
    party_scoped = "party_id" in _column_names(path)
    return FamilyEntry(
        key=key,
        entity=entity,
        domain_type=f"ores::{component}::domain::{entity}",
        subject_stem=f"{component}.v1.{plural}",
        party_scoped=party_scoped,
        write_mode=_write_mode(frontmatter, party_scoped, path),
    )


def discover_entries(project_root: Path) -> list[FamilyEntry]:
    """Every convention family the modelling tree declares, by family key.

    A marker on a file that is not an entity model is an error: the registry
    would name a type no protocol serves.
    """
    component = get_component(COMPONENT)
    entries: list[FamilyEntry] = []
    seen: dict[str, Path] = {}
    for path in discover_models(component, project_root):
        frontmatter = _frontmatter(path)
        key = frontmatter.get(MARKER)
        if not key:
            continue
        if frontmatter.get("type") != "ores.codegen.entity":
            raise ValueError(
                f"{path}: {MARKER} is only valid on #+type: ores.codegen.entity"
            )
        if key in seen:
            raise ValueError(
                f"{path}: family key {key!r} is already declared by {seen[key]}"
            )
        seen[key] = path
        entries.append(_entry_for(path, key))
    return sorted(entries, key=lambda entry: entry.key)


def parse_header(text: str) -> list[FamilyEntry]:
    """Every entry a rendered registry header states, in file order.

    The header is rendering now, so this reads it back the way the reader of
    the generated file does: the model's declaration and the rendered table
    are compared through it.
    """
    start = text.find("inline constexpr convention_family_entry convention_families[]")
    if start < 0:
        return []
    end = text.find("\n};", start)
    if end < 0:
        return []
    entries: list[FamilyEntry] = []
    for match in _ENTRY_RE.finditer(text[start:end]):
        entries.append(
            FamilyEntry(
                key=match.group(1),
                entity=match.group(2),
                domain_type=match.group(3),
                subject_stem=match.group(4),
                party_scoped=match.group(5) == "true",
                write_mode=match.group(6),
            )
        )
    return entries
