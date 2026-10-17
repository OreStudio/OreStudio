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

Three artefacts are rendered from the registry: a C++ header that states every
family and its write mode, and an include fragment that carries one write call
per family. The conventions service includes the fragment inside the anonymous
namespace that defines its row helpers, so the hand-written call per family
cannot drift from the modelling tree. The header is rendered as C++ rather
than an org or JSON artefact because a constexpr table gives the service a
compile-checked enumeration; the web BFF reaches the same data through its
generated protocol twins in a later slice.
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

FRAGMENT_RELATIVE_PATH = Path(
    "projects/ores.refdata/core/src/service/convention_family_apply.inc"
)

# The translation unit that compiles the fragment names no repository itself,
# so the includes it needs are generated beside it rather than kept by hand:
# a family added to the registry then arrives with its own header.
INCLUDES_RELATIVE_PATH = Path(
    "projects/ores.refdata/core/src/service/convention_family_apply_includes.inc"
)

WRITE_MODE_KEYWORD = "convention_write"
DECLARABLE_WRITE_MODES = ("party", "by_id")

_FRONTMATTER_RE = re.compile(r"^#\+([A-Za-z0-9_-]+):\s*(.*?)\s*$")
_ENTRY_RE = re.compile(
    r'\{\s*"([^"]*)",\s*"([^"]*)",\s*"([^"]*)",\s*"([^"]*)",\s*'
    r'(true|false),\s*convention_write_mode::([a-z_]+)\s*\}'
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
 * #+convention_family: or #+convention_write: declaration and regenerate.
 */
#ifndef ORES_REFDATA_API_DOMAIN_CONVENTION_FAMILY_REGISTRY_HPP
#define ORES_REFDATA_API_DOMAIN_CONVENTION_FAMILY_REGISTRY_HPP

#include <cstddef>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief How the conventions document writes a family's rows.
 *
 * party replaces the rows the importing party holds, by_id replaces the rows
 * the document names by key, and world adds only rows the tenant does not
 * already hold.
 */
enum class convention_write_mode { party, by_id, world };

/**
 * @brief One convention family the conventions document carries.
 *
 * key is the family's field name in the document, entity is the entity model
 * that serves it, domain_type is the generated C++ value type, subject_stem is
 * the protocol prefix every verb of that entity shares, party_scoped is true
 * when the family's table carries a party_id column, and write_mode is how the
 * document writes the family.
 */
struct convention_family_entry {
    std::string_view key;
    std::string_view entity;
    std::string_view domain_type;
    std::string_view subject_stem;
    bool party_scoped;
    convention_write_mode write_mode;
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


def render_header(entries: list[FamilyEntry]) -> str:
    """The committed registry header, byte for byte."""
    rows = "".join(
        f'    {{"{entry.key}",\n'
        f'     "{entry.entity}",\n'
        f'     "{entry.domain_type}",\n'
        f'     "{entry.subject_stem}",\n'
        f'     {"true" if entry.party_scoped else "false"},\n'
        f'     convention_write_mode::{entry.write_mode}}},\n'
        for entry in entries
    )
    return _LICENSE + "\n" + _PREAMBLE + rows + _EPILOGUE


_INCLUDES_PREAMBLE = """\
// AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
// Generator: projects/ores.codegen/scripts/regenerate_convention_family_registry.py
// One repository per family the apply fragment calls. Include this outside the
// namespace that compiles the fragment.
"""


_FRAGMENT_PREAMBLE = """\
// AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
// Generator: projects/ores.codegen/scripts/regenerate_convention_family_registry.py
// To change a call, edit the convention entity model's #+convention_family: or
// #+convention_write: declaration and regenerate. Include this fragment where
// replace_party_rows, replace_rows_by_id and add_missing_world_rows are
// visible, with <rfl/json.hpp> and the stamp helper already included.
"""


def _repository(entry: FamilyEntry) -> str:
    """The accessor that names the family's repository."""
    return f"{entry.entity}_repository()"


def _apply_party_body(entries: list[FamilyEntry]) -> str:
    lines = [
        "void apply_party_families(const ores::database::context& ctx,",
        "                          messaging::conventions_document& v) {",
    ]
    for entry in entries:
        lines.extend(
            [
                "    replace_party_rows(ctx,",
                f"                       {_repository(entry)},",
                f"                       std::move(v.{entry.key}));",
            ]
        )
    lines.append("}")
    return "\n".join(lines)


def _apply_by_id_body(entries: list[FamilyEntry]) -> str:
    lines = [
        "void apply_families_by_id(const ores::database::context& ctx,",
        "                         messaging::conventions_document& v) {",
    ]
    for entry in entries:
        lines.extend(
            [
                "    replace_rows_by_id(ctx,",
                f"                       {_repository(entry)},",
                f"                       std::move(v.{entry.key}));",
            ]
        )
    lines.append("}")
    return "\n".join(lines)


def _apply_world_body(entries: list[FamilyEntry]) -> str:
    lines = [
        "void apply_world_families(const ores::database::context& ctx,",
        "                          messaging::conventions_document& v,",
        "                          conventions_save_result& result) {",
    ]
    for entry in entries:
        lines.extend(
            [
                "    add_missing_world_rows(ctx,",
                f"                           {_repository(entry)},",
                f"                           v.{entry.key},",
                "                           result.world_kept);",
            ]
        )
    lines.append("}")
    return "\n".join(lines)


def _read_body(name: str, entries: list[FamilyEntry]) -> str:
    signature = f"void {name}("
    lines = [
        f"{signature}const ores::database::context& ctx,",
        f"{' ' * len(signature)}messaging::conventions_document& v) {{",
    ]
    for entry in entries:
        assignment = f"    v.{entry.key} = {_repository(entry)}.read_latest(ctx);"
        if len(assignment) <= 100:
            lines.append(assignment)
        else:
            lines.extend(
                [
                    f"    v.{entry.key} =",
                    f"        {_repository(entry)}.read_latest(ctx);",
                ]
            )
    lines.append("}")
    return "\n".join(lines)


def _staged_apply_body(entries: list[FamilyEntry]) -> str:
    """One branch per family, decoding a staged payload into its own type."""
    lines = [
        "// Staged families: each branch decodes one family's payload into its own",
        "// type, so no instantiation sees the whole document.",
        "ORES_REFDATA_CORE_EXPORT bool apply_staged_family(const ores::database::context& ctx,",
        "                                                  const std::string& family,",
        "                                                  const std::string& payload,",
        "                                                  conventions_save_result& result) {",
    ]
    for entry in entries:
        lines.append(f'    if (family == "{entry.key}") {{')
        lines.extend(
            [
                "        const auto decoded =",
                f"            rfl::json::read<{entry.domain_type}>(payload);",
                "        if (!decoded)",
                "            throw std::runtime_error(",
                '                "Staged conventions payload for family "',
                f'                "\\"{entry.key}\\" does not decode.");',
                f"        auto rows = std::vector<{entry.domain_type}>{{",
                "            std::move(*decoded)};",
                "        ores::service::messaging::stamp(rows.front(), ctx);",
            ]
        )
        if entry.write_mode == "world":
            lines.extend(
                [
                    "        add_missing_world_rows(ctx,",
                    f"                               {_repository(entry)},",
                    "                               std::move(rows),",
                    "                               result.world_kept);",
                ]
            )
        else:
            helper = (
                "replace_party_rows"
                if entry.write_mode == "party"
                else "replace_rows_by_id"
            )
            lines.extend(
                [
                    f"        {helper}(ctx,",
                    f"                           {_repository(entry)},",
                    "                           std::move(rows));",
                ]
            )
        lines.extend(["        return true;", "    }"])
    lines.extend(["    return false;", "}"])
    return "\n".join(lines)


def render_fragment(entries: list[FamilyEntry]) -> str:
    """The include fragment the conventions service compiles, byte for byte."""
    by_mode = {
        mode: [entry for entry in entries if entry.write_mode == mode]
        for mode in ("party", "by_id", "world")
    }
    bodies = [
        "// Party families: the import replaces the rows the party holds.\n"
        + _apply_party_body(by_mode["party"]),
        "// By-id families: the import replaces the rows the document names.\n"
        + _apply_by_id_body(by_mode["by_id"]),
        "// World families: the import adds only the rows the tenant is missing.\n"
        + _apply_world_body(by_mode["world"]),
        "// The document reads each mode back from the same registry.\n"
        + _read_body("read_party_families", by_mode["party"]),
        _read_body("read_families_by_id", by_mode["by_id"]),
        _read_body("read_world_families", by_mode["world"]),
        _staged_apply_body(entries),
    ]
    return _LICENSE + "\n" + _FRAGMENT_PREAMBLE + "\n\n" + "\n\n".join(bodies) + "\n"


def render_includes(entries: list[FamilyEntry]) -> str:
    """The repository includes the apply fragment needs, byte for byte.

    One line per family, sorted, so the translation unit that compiles the
    fragment declares every repository the fragment calls. The includes stay
    outside the namespace, which is why they travel as their own fragment
    rather than being prepended to the code one.
    """
    headers = sorted({_repository(entry).removesuffix("()") for entry in entries})
    includes = "\n".join(
        f'#include "ores.refdata.core/repository/{header}.hpp"' for header in headers
    )
    return _LICENSE + "\n" + _INCLUDES_PREAMBLE + "\n\n" + includes + "\n"


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
                write_mode=match.group(6),
            )
        )
    return entries
