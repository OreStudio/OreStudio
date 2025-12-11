#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""
Generate org-mode documentation for the ORE Studio binary protocol.

Parses C++ header files to extract message types, struct definitions, and field
information, then generates comprehensive protocol documentation in org-mode format.
"""

import re
import sys
from pathlib import Path
from dataclasses import dataclass, field
from typing import Optional


@dataclass
class Field:
    """Represents a struct field."""
    name: str
    type: str
    comment: Optional[str] = None


@dataclass
class Message:
    """Represents a protocol message struct."""
    name: str
    description: str
    fields: list[Field] = field(default_factory=list)
    wire_format: list[str] = field(default_factory=list)


@dataclass
class MessageType:
    """Represents a message type enum entry."""
    name: str
    value: int
    subsystem: str


@dataclass
class ErrorCode:
    """Represents an error code enum entry."""
    name: str
    value: int


@dataclass
class ProtocolInfo:
    """Contains all parsed protocol information."""
    version_major: int = 0
    version_minor: int = 0
    version_comment: str = ""
    message_types: list[MessageType] = field(default_factory=list)
    error_codes: list[ErrorCode] = field(default_factory=list)
    messages: dict[str, Message] = field(default_factory=dict)


def get_subsystem(value: int) -> str:
    """Determine subsystem based on message type value."""
    if 0x0000 <= value <= 0x0FFF:
        return "Core"
    elif 0x1000 <= value <= 0x1FFF:
        return "Risk"
    elif 0x2000 <= value <= 0x2FFF:
        return "Accounts"
    elif 0x3000 <= value <= 0x3FFF:
        return "Variability"
    return "Unknown"


def parse_message_types(filepath: Path) -> tuple[int, int, str, list[MessageType], list[ErrorCode]]:
    """Parse message_types.hpp for version, message types, and error codes."""
    content = filepath.read_text()

    # Extract version
    major_match = re.search(r'PROTOCOL_VERSION_MAJOR\s*=\s*(\d+)', content)
    minor_match = re.search(r'PROTOCOL_VERSION_MINOR\s*=\s*(\d+)', content)
    version_major = int(major_match.group(1)) if major_match else 0
    version_minor = int(minor_match.group(1)) if minor_match else 0

    # Extract version comment (multi-line comment before version constants)
    version_comment = ""
    version_comment_match = re.search(
        r'(// Protocol version.*?)(?=constexpr std::uint16_t PROTOCOL_VERSION_MAJOR)',
        content, re.DOTALL
    )
    if version_comment_match:
        lines = version_comment_match.group(1).strip().split('\n')
        version_comment = '\n'.join(line.lstrip('/ ').strip() for line in lines if line.strip())

    # Extract message types
    message_types = []
    enum_match = re.search(r'enum class message_type\s*\{([^}]+)\}', content, re.DOTALL)
    if enum_match:
        enum_body = enum_match.group(1)
        for match in re.finditer(r'(\w+)\s*=\s*(0x[0-9A-Fa-f]+)', enum_body):
            name = match.group(1)
            if name == 'last_value':
                continue
            value = int(match.group(2), 16)
            subsystem = get_subsystem(value)
            message_types.append(MessageType(name=name, value=value, subsystem=subsystem))

    # Extract error codes
    error_codes = []
    error_enum_match = re.search(r'enum class error_code\s*\{([^}]+)\}', content, re.DOTALL)
    if error_enum_match:
        enum_body = error_enum_match.group(1)
        for match in re.finditer(r'(\w+)\s*=\s*(0x[0-9A-Fa-f]+)', enum_body):
            name = match.group(1)
            if name == 'last_value':
                continue
            value = int(match.group(2), 16)
            error_codes.append(ErrorCode(name=name, value=value))

    return version_major, version_minor, version_comment, message_types, error_codes


def parse_protocol_header(filepath: Path) -> dict[str, Message]:
    """Parse a protocol header file for struct definitions."""
    content = filepath.read_text()
    messages = {}

    # Pattern to match struct with preceding doxygen comment
    # Match: /** @brief Description */ struct name final {
    struct_pattern = re.compile(
        r'/\*\*\s*\n\s*\*\s*@brief\s+([^\n]+(?:\n\s*\*\s+[^\n]+)*)\s*\*/\s*'
        r'struct\s+(\w+)\s+final\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}',
        re.MULTILINE
    )

    for match in struct_pattern.finditer(content):
        description_raw = match.group(1)
        struct_name = match.group(2)
        struct_body = match.group(3)

        # Clean up description (remove * prefixes and join lines)
        description_lines = description_raw.split('\n')
        description = ' '.join(
            line.lstrip('* ').strip()
            for line in description_lines
        ).strip()

        # Extract fields (lines with type and name, excluding methods)
        fields = []
        # Match: type name; or type name = value; with optional /// comment
        field_pattern = re.compile(
            r'(?:///\s*([^\n]+)\n\s*)?'  # Optional preceding comment
            r'((?:std::)?[\w:<>,\s]+?)\s+'  # Type (possibly with templates)
            r'(\w+)\s*(?:=\s*[^;]+)?;',  # Name and optional default
            re.MULTILINE
        )

        for field_match in field_pattern.finditer(struct_body):
            comment = field_match.group(1)
            field_type = field_match.group(2).strip()
            field_name = field_match.group(3)

            # Skip method declarations
            if '(' in field_type or field_name in ('serialize', 'deserialize'):
                continue
            # Skip if type looks like a return type of a method
            if field_type in ('static', 'std::vector<std::byte>'):
                continue

            fields.append(Field(name=field_name, type=field_type, comment=comment))

        # Extract wire format from serialize method comment
        wire_format = []
        format_match = re.search(
            r'Format:\s*\n((?:\s*\*\s*-[^\n]+\n)+)',
            struct_body
        )
        if format_match:
            format_lines = format_match.group(1).split('\n')
            for line in format_lines:
                cleaned = re.sub(r'^\s*\*\s*-\s*', '', line).strip()
                if cleaned:
                    wire_format.append(cleaned)

        messages[struct_name] = Message(
            name=struct_name,
            description=description,
            fields=fields,
            wire_format=wire_format
        )

    return messages


def generate_org_document(info: ProtocolInfo, org_id: str) -> str:
    """Generate org-mode document from protocol information."""
    lines = []

    # Preamble
    lines.append(":PROPERTIES:")
    lines.append(f":ID: {org_id}")
    lines.append(":END:")
    lines.append("#+title: ORE Studio Binary Protocol Reference")
    lines.append("#+author: Auto-generated by generate_protocol_docs.py")
    lines.append("#+options: <:nil c:nil todo:nil ^:nil d:nil date:nil author:nil toc:t html-postamble:nil")
    lines.append("#+startup: overview")
    lines.append("")

    # Introduction
    lines.append("This document provides a comprehensive reference for the ORE Studio binary")
    lines.append("protocol. It is auto-generated from the C++ header files and should be")
    lines.append("regenerated whenever protocol changes are made.")
    lines.append("")

    # Protocol Version
    lines.append("* Protocol Version")
    lines.append("")
    lines.append(f"Current version: *{info.version_major}.{info.version_minor}*")
    lines.append("")
    if info.version_comment:
        lines.append("Version history:")
        lines.append("")
        for line in info.version_comment.split('\n'):
            if line.strip():
                lines.append(f"- {line}")
        lines.append("")

    # Group messages by subsystem
    subsystems = {
        "Core": (0x0000, 0x0FFF),
        "Risk": (0x1000, 0x1FFF),
        "Accounts": (0x2000, 0x2FFF),
        "Variability": (0x3000, 0x3FFF),
    }

    for subsystem_name, (min_val, max_val) in subsystems.items():
        subsystem_messages = [
            mt for mt in info.message_types
            if mt.subsystem == subsystem_name
        ]

        if not subsystem_messages:
            continue

        lines.append(f"* {subsystem_name} Subsystem (0x{min_val:04X}-0x{max_val:04X})")
        lines.append("")

        # Messages overview table
        lines.append("** Messages Overview")
        lines.append("")
        lines.append("| ID | Name | Description |")
        lines.append("|----+------+-------------|")

        for mt in sorted(subsystem_messages, key=lambda x: x.value):
            desc = ""
            if mt.name in info.messages:
                desc = info.messages[mt.name].description[:50]
                if len(info.messages[mt.name].description) > 50:
                    desc += "..."
            lines.append(f"| 0x{mt.value:04X} | {mt.name} | {desc} |")

        lines.append("")

        # Detailed message sections
        for mt in sorted(subsystem_messages, key=lambda x: x.value):
            msg = info.messages.get(mt.name)
            lines.append(f"** {mt.name} (0x{mt.value:04X})")
            lines.append("")

            if msg:
                lines.append(msg.description)
                lines.append("")

                if msg.fields:
                    lines.append("*** Fields")
                    lines.append("")
                    lines.append("| Field | Type | Description |")
                    lines.append("|-------+------+-------------|")
                    for f in msg.fields:
                        comment = f.comment or ""
                        lines.append(f"| {f.name} | ~{f.type}~ | {comment} |")
                    lines.append("")

                if msg.wire_format:
                    lines.append("*** Wire Format")
                    lines.append("")
                    for fmt_line in msg.wire_format:
                        lines.append(f"- {fmt_line}")
                    lines.append("")
            else:
                lines.append("(No struct definition found)")
                lines.append("")

    # Error Codes section
    lines.append("* Error Codes")
    lines.append("")
    lines.append("| Code | Name |")
    lines.append("|------+------|")
    for ec in sorted(info.error_codes, key=lambda x: x.value):
        lines.append(f"| 0x{ec.value:04X} | {ec.name} |")
    lines.append("")

    return '\n'.join(lines)


def main():
    """Main entry point."""
    # Determine project root (script is in scripts/)
    script_dir = Path(__file__).parent
    project_root = script_dir.parent

    # Parse message_types.hpp
    message_types_path = project_root / "projects/ores.comms/include/ores.comms/messaging/message_types.hpp"
    if not message_types_path.exists():
        print(f"Error: {message_types_path} not found", file=sys.stderr)
        sys.exit(1)

    info = ProtocolInfo()
    (info.version_major, info.version_minor, info.version_comment,
     info.message_types, info.error_codes) = parse_message_types(message_types_path)

    print(f"Parsed protocol version {info.version_major}.{info.version_minor}")
    print(f"Found {len(info.message_types)} message types")
    print(f"Found {len(info.error_codes)} error codes")

    # Find and parse all protocol headers
    protocol_headers = list(project_root.glob("projects/**/*_protocol.hpp"))
    for header_path in protocol_headers:
        messages = parse_protocol_header(header_path)
        info.messages.update(messages)
        print(f"Parsed {header_path.name}: {len(messages)} structs")

    print(f"Total messages parsed: {len(info.messages)}")

    # Generate org document
    org_id = "A7B3C9D2-E5F1-4A8B-9C6D-3E2F1A0B8C7D"  # Fixed ID for this document
    org_content = generate_org_document(info, org_id)

    # Write output
    output_dir = project_root / "doc/modeling"
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / "protocol.org"
    output_path.write_text(org_content)

    print(f"Generated {output_path}")


if __name__ == "__main__":
    main()
