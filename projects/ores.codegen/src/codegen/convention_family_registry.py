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

The registry is rendered as a C++ header rather than an org or JSON artefact
because the C++ conventions service is the consumer that must stop carrying a
hand-written call per family, and a constexpr table gives it a compile-checked
enumeration; the web BFF reaches the same data through its generated protocol
twins in a later slice.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

from .manifest import discover_models, get_component

COMPONENT = "refdata"
MARKER = "convention_family"

HEADER_RELATIVE_PATH = Path(
    "projects/ores.refdata/api/include/ores.refdata.api/domain"
    "/convention_family_registry.hpp"
)

_FRONTMATTER_RE = re.compile(r"^#\+([A-Za-z0-9_-]+):\s*(.*?)\s*$")
_ENTRY_RE = re.compile(
    r'\{\s*"([^"]*)",\s*"([^"]*)",\s*"([^"]*)",\s*"([^"]*)",\s*'
    r'(true|false)\s*\}'
)

_LICENSE = """\
/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */"""

_PREAMBLE = """\
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generator: projects/ores.codegen/scripts/regenerate_convention_family_registry.py
 * To change an entry, edit the convention entity model's
 * #+convention_family: declaration and regenerate.
 */
#ifndef ORES_REFDATA_API_DOMAIN_CONVENTION_FAMILY_REGISTRY_HPP
#define ORES_REFDATA_API_DOMAIN_CONVENTION_FAMILY_REGISTRY_HPP

#include <cstddef>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One convention family the conventions document carries.
 *
 * key is the family's field name in the document, entity is the entity model
 * that serves it, domain_type is the generated C++ value type, subject_stem is
 * the protocol prefix every verb of that entity shares, and party_scoped is
 * true when the family's table carries a party_id column.
 */
struct convention_family_entry {
    std::string_view key;
    std::string_view entity;
    std::string_view domain_type;
    std::string_view subject_stem;
    bool party_scoped;
};

/**
 * @brief Every convention family, ordered by family key.
 */
inline constexpr convention_family_entry convention_families[] = {
"""

_EPILOGUE = """\
};

inline constexpr std::size_t convention_family_count =
    sizeof(convention_families) / sizeof(convention_families[0]);

}

#endif
"""


@dataclass(frozen=True)
class FamilyEntry:
    """One convention family as the registry states it."""

    key: str
    entity: str
    domain_type: str
    subject_stem: str
    party_scoped: bool


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


def _entry_for(path: Path, key: str) -> FamilyEntry:
    """The registry entry a marked entity model declares."""
    frontmatter = _frontmatter(path)
    entity = (frontmatter.get("entity_singular") or path.stem.split(".")[-1]).strip()
    component = (frontmatter.get("component") or "").strip()
    plural = (frontmatter.get("entity_plural") or f"{entity}s").strip()
    if not component:
        raise ValueError(f"{path}: a marked convention family must declare #+component:")
    return FamilyEntry(
        key=key,
        entity=entity,
        domain_type=f"ores::{component}::domain::{entity}",
        subject_stem=f"{component}.v1.{plural}",
        party_scoped="party_id" in _column_names(path),
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


def render_header(entries: list[FamilyEntry]) -> str:
    """The committed registry header, byte for byte."""
    rows = "".join(
        f'    {{"{entry.key}",\n'
        f'     "{entry.entity}",\n'
        f'     "{entry.domain_type}",\n'
        f'     "{entry.subject_stem}",\n'
        f'     {"true" if entry.party_scoped else "false"}}},\n'
        for entry in entries
    )
    return _LICENSE + "\n" + _PREAMBLE + rows + _EPILOGUE


def parse_header(text: str) -> list[FamilyEntry]:
    """Every entry a committed registry header states, in file order."""
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
            )
        )
    return entries
