# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

#!/usr/bin/env python3
"""Emit a language-neutral protocol IR from ORE Studio codegen entity org files.

The org files under `projects/<component>/**/modeling/*.org` are the single
source of truth codegen already renders into RFL structs, SQL schema, and Qt
controllers. This script reads the same `* Columns` model and writes the
protocol-relevant subset as JSON, so any target language can consume the
entity's message shape without re-parsing C++.

Scope: entity CRUD messages only (list/save/delete/history), which is what the
meta-model defines. Hand-written protocols such as `iam.v1.auth.login` are not
entity-derived and are out of scope here; they need a reflection-based emitter
that produces the same IR shape.

Usage:
    emit_protocol_ir.py --checkout <path> --component iam --out protocol.ir.json
    emit_protocol_ir.py --checkout <path> --all --out protocol.ir.json

Exit codes:
    0  IR written (or, with --check, already up to date)
    1  an entity could not be read, or --check found a difference
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Iterable

IR_VERSION = 1

# Property lines look like `:cpp_type:    std::string`.
PROPERTY_RE = re.compile(r"^:([a-z_]+):\s*(.*?)\s*$")
HEADER_PROPERTY_RE = re.compile(r"^#\+([a-z_]+):\s*(.*?)\s*$")

# Headings, by exact star depth. The model puts `* Columns` at level 1 and each
# column at level 2, so depth has to be matched exactly rather than as a range.
HEADING_RE = re.compile(r"^(\*+)\s+(.*?)\s*$")

# A heading any deeper than a level-3 is a sub-part of a section we ignore.
DEEP_HEADING_DEPTH = 4


@dataclass
class Column:
    """One entity column, as the codegen model declares it."""

    name: str
    kind: str
    cpp_type: str
    nullable: bool
    primary_key: bool
    natural_key: bool
    skip_uuid_check: bool
    doc: str
    generator: str | None


@dataclass
class Entity:
    """One codegen entity, reduced to what a protocol message needs."""

    component: str
    singular: str
    plural: str
    title: str
    schema: str
    profile: str
    has_tenant_id: bool
    source: str
    columns: list[Column] = field(default_factory=list)


class OrgReadError(RuntimeError):
    """An org file does not carry the model this script needs."""


def read_entity(path: Path) -> Entity:
    """Reads one entity org file into the IR model.

    Raises:
        OrgReadError: when the file is not a codegen entity, or has no columns.
    """
    text = path.read_text(encoding="utf-8", errors="replace")
    header = read_header(text)

    if header.get("type") != "ores.codegen.entity":
        raise OrgReadError(
            f"{path}: #+type is {header.get('type')!r}, expected 'ores.codegen.entity'"
        )

    flags = read_flags(text)
    columns = read_columns(text)
    if not columns:
        raise OrgReadError(f"{path}: no columns found under '* Columns'")

    singular = header.get("entity_singular", "")
    plural = header.get("entity_plural", "")
    if not singular or not plural:
        raise OrgReadError(f"{path}: missing #+entity_singular or #+entity_plural")

    return Entity(
        component=header.get("component", ""),
        singular=singular,
        plural=plural,
        title=header.get("entity_title", singular),
        schema=flags.get("schema", "public"),
        profile=flags.get("profile", ""),
        has_tenant_id=flags.get("has_tenant_id", "").lower() == "true",
        source=str(path),
        columns=columns,
    )


def read_header(text: str) -> dict[str, str]:
    """Reads the `#+key: value` header block."""
    header: dict[str, str] = {}
    for line in text.splitlines():
        match = HEADER_PROPERTY_RE.match(line)
        if match:
            header[match.group(1)] = match.group(2)
    return header


def read_flags(text: str) -> dict[str, str]:
    """Reads the `* Flags` property drawer."""
    return read_property_drawer(text, section="Flags")


def read_property_drawer(text: str, *, section: str) -> dict[str, str]:
    """Reads the property drawer that follows a named level-1 section.

    The drawer is returned empty when the section is absent, so a model that
    omits an optional section is not an error.
    """
    inside = False
    drawer: dict[str, str] = {}
    for line in text.splitlines():
        heading = HEADING_RE.match(line)
        if heading is not None:
            # A new level-1 heading ends the section we were reading.
            if inside:
                break
            inside = (
                len(heading.group(1)) == 1 and heading.group(2).strip().lower() == section.lower()
            )
            continue
        if not inside:
            continue
        stripped = line.strip()
        if stripped == ":PROPERTIES:":
            drawer = {}
            continue
        if stripped == ":END:":
            return drawer
        match = PROPERTY_RE.match(line)
        if match:
            drawer[match.group(1)] = match.group(2)
    return drawer


def read_columns(text: str) -> list[Column]:
    """Reads every column under `* Columns`.

    The section is a level-1 heading, each column is a level-2 heading carrying
    a property drawer with `:type:` and `:cpp_type:`, and a column body may hold
    a `#+begin_src cpp :name generator` block. The section ends at the next
    level-1 heading.
    """
    columns: list[Column] = []
    in_columns = False
    current_name: str | None = None
    current: dict[str, str] = {}
    current_body: list[str] = []
    in_drawer = False

    def flush() -> None:
        nonlocal current_name, current, current_body, in_drawer
        if current_name is not None:
            columns.append(build_column(current_name, current, current_body))
        current_name = None
        current = {}
        current_body = []
        in_drawer = False

    for line in text.splitlines():
        heading = HEADING_RE.match(line)
        if heading is not None:
            stars, title = heading.group(1), heading.group(2)

            if len(stars) == 1:
                flush()
                in_columns = title.strip().lower() == "columns"
                continue

            if not in_columns:
                continue

            if len(stars) == 2:
                flush()
                current_name = title.strip()
                continue

            # Deeper headings belong to the current column's documentation.
            if len(stars) >= DEEP_HEADING_DEPTH:
                continue

        if not in_columns or current_name is None:
            continue

        stripped = line.strip()
        if stripped == ":PROPERTIES:":
            in_drawer = True
            continue
        if stripped == ":END:":
            in_drawer = False
            continue
        if in_drawer:
            match = PROPERTY_RE.match(line)
            if match:
                current[match.group(1)] = match.group(2)
            continue
        current_body.append(line)

    flush()
    return columns


def build_column(name: str, properties: dict[str, str], body: list[str]) -> Column:
    """Builds a {@link Column} from its drawer and body."""
    return Column(
        name=name,
        kind=properties.get("type", ""),
        cpp_type=properties.get("cpp_type", ""),
        nullable=properties.get("nullable", "false").lower() == "true",
        primary_key=properties.get("primary_key", "false").lower() == "true",
        natural_key=properties.get("natural_key", "false").lower() == "true",
        skip_uuid_check=properties.get("skip_uuid_check", "false").lower() == "true",
        doc=first_prose(body),
        generator=first_generator(body),
    )


def first_prose(body: list[str]) -> str:
    """Returns the first non-empty prose line of a column body."""
    for line in body:
        stripped = line.strip()
        if stripped and not stripped.startswith("#+"):
            return stripped
    return ""


def first_generator(body: list[str]) -> str | None:
    """Returns the text of a `#+begin_src cpp :name generator` block."""
    lines: list[str] = []
    collecting = False
    for line in body:
        stripped = line.strip()
        if stripped.startswith("#+begin_src cpp") and "generator" in stripped:
            collecting = True
            continue
        if collecting and stripped.startswith("#+end_src"):
            return "\n".join(lines).strip()
        if collecting:
            lines.append(line)
    return None


def protocol_messages(entity: Entity) -> list[dict[str, Any]]:
    """Builds the CRUD message set the meta-model defines for an entity.

    The shapes mirror `entity_meta_model_cpp_protocol.org`: a list pair, a save
    pair, a delete pair, and a history pair, with subjects of the form
    `<component>.v1.<plural>.<verb>`.
    """
    plural = entity.plural
    singular = entity.singular
    prefix = f"{entity.component}.v1.{plural}"
    key_columns = [column for column in entity.columns if column.primary_key]

    return [
        {
            "name": f"get_{plural}_request",
            "subject": f"{prefix}.list",
            "response": f"get_{plural}_response",
            "direction": "request",
            "fields": [
                {"name": "offset", "cpp_type": "std::uint32_t", "nullable": False, "default": "0"},
                {"name": "limit", "cpp_type": "std::uint32_t", "nullable": False, "default": "100"},
            ],
        },
        {
            "name": f"get_{plural}_response",
            "subject": None,
            "response": None,
            "direction": "response",
            "fields": [
                {
                    "name": plural,
                    "cpp_type": f"std::vector<ores::{entity.component}::domain::{singular}>",
                    "nullable": False,
                    "default": None,
                },
                {
                    "name": "total_available_count",
                    "cpp_type": "int",
                    "nullable": False,
                    "default": "0",
                },
                {"name": "success", "cpp_type": "bool", "nullable": False, "default": "false"},
                {"name": "message", "cpp_type": "std::string", "nullable": False, "default": None},
            ],
        },
        {
            "name": f"save_{singular}_request",
            "subject": f"{prefix}.save",
            "response": f"save_{singular}_response",
            "direction": "request",
            "fields": [
                {
                    "name": "data",
                    "cpp_type": f"ores::{entity.component}::domain::{singular}",
                    "nullable": False,
                    "default": None,
                }
            ],
        },
        {
            "name": f"save_{singular}_response",
            "subject": None,
            "response": None,
            "direction": "response",
            "fields": [
                {"name": "success", "cpp_type": "bool", "nullable": False, "default": "false"},
                {"name": "message", "cpp_type": "std::string", "nullable": False, "default": None},
            ],
        },
        {
            "name": f"delete_{singular}_request",
            "subject": f"{prefix}.delete",
            "response": f"delete_{singular}_response",
            "direction": "request",
            "fields": [
                {
                    "name": f"{column.name}s" if not column.skip_uuid_check else "ids",
                    "cpp_type": "std::vector<std::string>",
                    "nullable": False,
                    "default": None,
                }
                for column in key_columns
            ],
        },
        {
            "name": f"delete_{singular}_response",
            "subject": None,
            "response": None,
            "direction": "response",
            "fields": [
                {"name": "success", "cpp_type": "bool", "nullable": False, "default": "false"},
                {"name": "message", "cpp_type": "std::string", "nullable": False, "default": None},
            ],
        },
        {
            "name": f"get_{singular}_history_request",
            "subject": f"{prefix}.history",
            "response": f"get_{singular}_history_response",
            "direction": "request",
            "fields": [
                {
                    "name": column.name,
                    "cpp_type": "std::string",
                    "nullable": False,
                    "default": None,
                }
                for column in key_columns
            ],
        },
        {
            "name": f"get_{singular}_history_response",
            "subject": None,
            "response": None,
            "direction": "response",
            "fields": [
                {
                    "name": "history",
                    "cpp_type": f"std::vector<ores::{entity.component}::domain::{singular}>",
                    "nullable": False,
                    "default": None,
                },
                {"name": "success", "cpp_type": "bool", "nullable": False, "default": "false"},
                {"name": "message", "cpp_type": "std::string", "nullable": False, "default": None},
            ],
        },
    ]


def entity_ir(entity: Entity) -> dict[str, Any]:
    """Builds the IR document for one entity."""
    return {
        "component": entity.component,
        "singular": entity.singular,
        "plural": entity.plural,
        "title": entity.title,
        "schema": entity.schema,
        "profile": entity.profile,
        "hasTenantId": entity.has_tenant_id,
        "source": entity.source,
        "columns": [
            {
                "name": column.name,
                "kind": column.kind,
                "cppType": column.cpp_type,
                "nullable": column.nullable,
                "primaryKey": column.primary_key,
                "naturalKey": column.natural_key,
                "doc": column.doc,
            }
            for column in entity.columns
        ],
        "messages": protocol_messages(entity),
    }


def discover(checkout: Path, components: Iterable[str] | None) -> list[Path]:
    """Finds entity org files, optionally limited to some components."""
    wanted = set(components) if components else None
    found: list[Path] = []
    for path in sorted(checkout.glob("projects/**/modeling/*.org")):
        if wanted is not None and not any(f".{name}." in path.name for name in wanted):
            continue
        try:
            if "#+type: ores.codegen.entity" in path.read_text(encoding="utf-8", errors="replace"):
                found.append(path)
        except OSError:
            continue
    return found


def build_document(checkout: Path, components: Iterable[str] | None) -> dict[str, Any]:
    """Builds the whole IR document, reporting entities it could not read."""
    entities: list[dict[str, Any]] = []
    errors: list[str] = []
    for path in discover(checkout, components):
        try:
            entities.append(entity_ir(read_entity(path)))
        except OrgReadError as error:
            errors.append(str(error))

    entities.sort(key=lambda item: (item["component"], item["singular"]))
    return {
        "irVersion": IR_VERSION,
        "generator": "ores.codegen emit_protocol_ir.py",
        "checkout": str(checkout),
        "entityCount": len(entities),
        "skipped": errors,
        "entities": entities,
    }


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--checkout", required=True, type=Path)
    parser.add_argument("--component", action="append", dest="components")
    parser.add_argument("--all", action="store_true", help="Include every component")
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument(
        "--check",
        action="store_true",
        help="Fail when the on-disk IR differs from what would be generated",
    )
    args = parser.parse_args(argv)

    if not args.all and not args.components:
        parser.error("pass --all or at least one --component")

    if not args.checkout.is_dir():
        print(f"checkout not found: {args.checkout}", file=sys.stderr)
        return 1

    document = build_document(args.checkout, None if args.all else args.components)
    rendered = json.dumps(document, indent=2, sort_keys=False) + "\n"

    if args.check:
        if not args.out.exists():
            print(f"{args.out}: missing", file=sys.stderr)
            return 1
        if args.out.read_text(encoding="utf-8") != rendered:
            print(f"{args.out}: stale, re-run without --check", file=sys.stderr)
            return 1
        print(f"{args.out}: up to date ({document['entityCount']} entities)")
        return 0

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(rendered, encoding="utf-8")
    print(f"wrote {args.out}: {document['entityCount']} entities")
    for error in document["skipped"]:
        print(f"  skipped: {error}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
