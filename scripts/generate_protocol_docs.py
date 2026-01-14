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

Output structure:
- projects/modeling/protocol.org - Main index with links to subsystem docs
- projects/ores.comms/modeling/ores.comms.protocol.org - Core protocol messages
- projects/ores.refdata/modeling/ores.refdata.protocol.org - Risk protocol messages
- projects/ores.accounts/modeling/ores.accounts.protocol.org - Accounts protocol messages
- projects/ores.variability/modeling/ores.variability.protocol.org - Variability protocol messages
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
class SubsystemInfo:
    """Information about a protocol subsystem."""
    name: str
    component: str  # e.g., "ores.comms"
    min_value: int
    max_value: int
    org_id: str
    message_types: list[MessageType] = field(default_factory=list)


@dataclass
class ProtocolInfo:
    """Contains all parsed protocol information."""
    version_major: int = 0
    version_minor: int = 0
    version_comment: str = ""
    message_types: list[MessageType] = field(default_factory=list)
    error_codes: list[ErrorCode] = field(default_factory=list)
    messages: dict[str, Message] = field(default_factory=dict)


# Subsystem configuration with fixed org-roam IDs
SUBSYSTEMS = {
    "Core": SubsystemInfo(
        name="Core",
        component="ores.comms",
        min_value=0x0000,
        max_value=0x0FFF,
        org_id="B1A2C3D4-E5F6-7890-ABCD-EF1234567890"
    ),
    "Risk": SubsystemInfo(
        name="Risk",
        component="ores.refdata",
        min_value=0x1000,
        max_value=0x1FFF,
        org_id="C2B3D4E5-F6A7-8901-BCDE-F12345678901"
    ),
    "Accounts": SubsystemInfo(
        name="Accounts",
        component="ores.accounts",
        min_value=0x2000,
        max_value=0x2FFF,
        org_id="D3C4E5F6-A7B8-9012-CDEF-123456789012"
    ),
    "Variability": SubsystemInfo(
        name="Variability",
        component="ores.variability",
        min_value=0x3000,
        max_value=0x3FFF,
        org_id="E4D5F6A7-B8C9-0123-DEFA-234567890123"
    ),
    "Assets": SubsystemInfo(
        name="Assets",
        component="ores.assets",
        min_value=0x4000,
        max_value=0x4FFF,
        org_id="F5E6A7B8-C9D0-1234-EFAB-345678901234"
    ),
    "Telemetry": SubsystemInfo(
        name="Telemetry",
        component="ores.telemetry",
        min_value=0x5000,
        max_value=0x5FFF,
        org_id="A6B7C8D9-E0F1-2345-ABCD-456789012345"
    ),
}

# Main protocol index org-roam ID
MAIN_PROTOCOL_ORG_ID = "A7B3C9D2-E5F1-4A8B-9C6D-3E2F1A0B8C7D"


def get_subsystem(value: int) -> str:
    """Determine subsystem based on message type value."""
    for name, info in SUBSYSTEMS.items():
        if info.min_value <= value <= info.max_value:
            return name
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

    return version_major, version_minor, version_comment, message_types


def parse_error_codes(filepath: Path) -> list[ErrorCode]:
    """Parse error_code.hpp for error codes."""
    content = filepath.read_text()

    error_codes = []
    error_enum_match = re.search(r'enum class error_code\s*:\s*std::uint16_t\s*\{([^}]+)\}', content, re.DOTALL)
    if error_enum_match:
        enum_body = error_enum_match.group(1)
        for match in re.finditer(r'(\w+)\s*=\s*(0x[0-9A-Fa-f]+)', enum_body):
            name = match.group(1)
            if name == 'last_value':
                continue
            value = int(match.group(2), 16)
            error_codes.append(ErrorCode(name=name, value=value))

    return error_codes


def parse_protocol_header(filepath: Path) -> dict[str, Message]:
    """Parse a protocol header file for struct definitions."""
    content = filepath.read_text()
    messages = {}

    # Pattern to match struct with preceding doxygen comment
    struct_pattern = re.compile(
        r'/\*\*\s*\n\s*\*\s*@brief\s+([^\n]+(?:\n\s*\*\s+[^\n]+)*)\s*\*/\s*'
        r'struct\s+(\w+)\s+final\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}',
        re.MULTILINE
    )

    for match in struct_pattern.finditer(content):
        description_raw = match.group(1)
        struct_name = match.group(2)
        struct_body = match.group(3)

        # Clean up description
        description_lines = description_raw.split('\n')
        description = ' '.join(
            line.lstrip('* ').strip()
            for line in description_lines
        ).strip()

        # Extract fields
        fields = []
        field_pattern = re.compile(
            r'(?:///\s*([^\n]+)\n\s*)?'
            r'((?:std::)?[\w:<>,\s]+?)\s+'
            r'(\w+)\s*(?:=\s*[^;]+)?;',
            re.MULTILINE
        )

        for field_match in field_pattern.finditer(struct_body):
            comment = field_match.group(1)
            field_type = field_match.group(2).strip()
            field_name = field_match.group(3)

            if '(' in field_type or field_name in ('serialize', 'deserialize'):
                continue
            if field_type in ('static', 'std::vector<std::byte>'):
                continue

            fields.append(Field(name=field_name, type=field_type, comment=comment))

        # Extract wire format
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


def generate_subsystem_doc(subsystem: SubsystemInfo, info: ProtocolInfo) -> str:
    """Generate org-mode document for a single subsystem."""
    lines = []

    # Preamble
    lines.append(":PROPERTIES:")
    lines.append(f":ID: {subsystem.org_id}")
    lines.append(":END:")
    lines.append(f"#+title: {subsystem.component} Protocol Reference")
    lines.append("#+author: Auto-generated by generate_protocol_docs.py")
    lines.append("#+options: <:nil c:nil todo:nil ^:nil d:nil date:nil author:nil toc:t html-postamble:nil")
    lines.append("#+startup: overview")
    lines.append("")

    # Introduction
    lines.append(f"Protocol messages for the {subsystem.name} subsystem ")
    lines.append(f"(0x{subsystem.min_value:04X}-0x{subsystem.max_value:04X}).")
    lines.append("")
    lines.append(f"See [[id:{MAIN_PROTOCOL_ORG_ID}][Protocol Reference]] for the complete protocol documentation.")
    lines.append("")

    # Get messages for this subsystem
    subsystem_messages = [
        mt for mt in info.message_types
        if mt.subsystem == subsystem.name
    ]

    if not subsystem_messages:
        lines.append("No messages defined for this subsystem.")
        return '\n'.join(lines)

    # Messages overview table
    lines.append("* Messages Overview")
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
    lines.append("* Message Details")
    lines.append("")

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

    return '\n'.join(lines)


def generate_main_protocol_doc(info: ProtocolInfo) -> str:
    """Generate main protocol index document."""
    lines = []

    # Preamble
    lines.append(":PROPERTIES:")
    lines.append(f":ID: {MAIN_PROTOCOL_ORG_ID}")
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
    lines.append("To regenerate: =cmake --build --preset linux-clang-debug --target generate_protocol_docs=")
    lines.append("")

    # Protocol Version
    lines.append("* Protocol Version")
    lines.append("")
    lines.append(f"Current version: *{info.version_major}.{info.version_minor}*")
    lines.append("")
    if info.version_comment:
        lines.append("** Version History")
        lines.append("")
        for line in info.version_comment.split('\n'):
            if line.strip():
                lines.append(f"- {line}")
        lines.append("")

    # Subsystem links
    lines.append("* Subsystems")
    lines.append("")
    lines.append("The protocol is organized into subsystems, each with its own message range:")
    lines.append("")
    lines.append("| Subsystem | Range | Component | Messages |")
    lines.append("|-----------+-------+-----------+----------|")

    for name, subsystem in SUBSYSTEMS.items():
        msg_count = len([mt for mt in info.message_types if mt.subsystem == name])
        lines.append(
            f"| [[id:{subsystem.org_id}][{name}]] | "
            f"0x{subsystem.min_value:04X}-0x{subsystem.max_value:04X} | "
            f"{subsystem.component} | {msg_count} |"
        )

    lines.append("")

    # Session Lifecycle section
    lines.append("* Session Lifecycle")
    lines.append("")
    lines.append("The following diagram illustrates the session lifecycle for authentication")
    lines.append("and account management operations.")
    lines.append("")
    lines.append("#+attr_html: :width 100%")
    lines.append("#+caption: Session Lifecycle - Authentication and Account Management")
    lines.append("[[file:protocol.session.png][file:protocol.session.png]]")
    lines.append("")
    lines.append("** Connection Establishment")
    lines.append("")
    lines.append("1. Client establishes SSL/TLS connection to server")
    lines.append("2. Three-way handshake:")
    lines.append("   - Client sends =handshake_request= with protocol version")
    lines.append("   - Server responds with =handshake_response= indicating compatibility")
    lines.append("   - Client sends =handshake_ack= to complete")
    lines.append("")
    lines.append("** Authentication")
    lines.append("")
    lines.append("1. Client sends =login_request= with credentials")
    lines.append("2. Server validates against account service")
    lines.append("3. On success, server stores session context (remote_addr -> account_id)")
    lines.append("4. Server returns =login_response= with account info and admin status")
    lines.append("")
    lines.append("** Authenticated Operations")
    lines.append("")
    lines.append("Admin-only operations (lock/unlock account) use server-side session tracking:")
    lines.append("")
    lines.append("1. Client sends request (e.g., =lock_account_request=)")
    lines.append("2. Server looks up requester's account_id from session context")
    lines.append("3. Server verifies admin privileges via account service")
    lines.append("4. If authorized, operation proceeds; otherwise error returned")
    lines.append("")
    lines.append("** Session Termination")
    lines.append("")
    lines.append("1. Client sends =logout_request=")
    lines.append("2. Server clears session context")
    lines.append("3. Server updates login_info (online=false)")
    lines.append("4. Connection closed")
    lines.append("")

    # Error Codes section
    lines.append("* Error Codes")
    lines.append("")
    lines.append("| Code | Name |")
    lines.append("|------+------|")
    for ec in sorted(info.error_codes, key=lambda x: x.value):
        lines.append(f"| 0x{ec.value:04X} | {ec.name} |")
    lines.append("")

    # All Messages Quick Reference
    lines.append("* All Messages Quick Reference")
    lines.append("")
    lines.append("| ID | Name | Subsystem |")
    lines.append("|----+------+-----------|")
    for mt in sorted(info.message_types, key=lambda x: x.value):
        lines.append(f"| 0x{mt.value:04X} | {mt.name} | {mt.subsystem} |")
    lines.append("")

    return '\n'.join(lines)


def main():
    """Main entry point."""
    script_dir = Path(__file__).parent
    project_root = script_dir.parent

    # Parse message_types.hpp
    message_types_path = project_root / "projects/ores.comms/include/ores.comms/messaging/message_types.hpp"
    if not message_types_path.exists():
        print(f"Error: {message_types_path} not found", file=sys.stderr)
        sys.exit(1)

    info = ProtocolInfo()
    (info.version_major, info.version_minor, info.version_comment,
     info.message_types) = parse_message_types(message_types_path)

    # Parse error codes from separate file
    error_code_path = project_root / "projects/ores.utility/include/ores.utility/serialization/error_code.hpp"
    if error_code_path.exists():
        info.error_codes = parse_error_codes(error_code_path)
    else:
        print(f"Warning: {error_code_path} not found, error codes will be empty", file=sys.stderr)

    print(f"Parsed protocol version {info.version_major}.{info.version_minor}")
    print(f"Found {len(info.message_types)} message types")
    print(f"Found {len(info.error_codes)} error codes")

    # Parse all protocol headers
    protocol_headers = list(project_root.glob("projects/**/*_protocol.hpp"))
    for header_path in protocol_headers:
        messages = parse_protocol_header(header_path)
        info.messages.update(messages)
        print(f"Parsed {header_path.name}: {len(messages)} structs")

    print(f"Total messages parsed: {len(info.messages)}")

    # Generate main protocol index
    main_doc = generate_main_protocol_doc(info)
    main_path = project_root / "projects/modeling/protocol.org"
    main_path.write_text(main_doc)
    print(f"Generated {main_path}")

    # Generate per-subsystem documents
    for name, subsystem in SUBSYSTEMS.items():
        subsystem_doc = generate_subsystem_doc(subsystem, info)
        subsystem_path = project_root / f"projects/{subsystem.component}/modeling/{subsystem.component}.protocol.org"
        subsystem_path.parent.mkdir(parents=True, exist_ok=True)
        subsystem_path.write_text(subsystem_doc)
        print(f"Generated {subsystem_path}")


if __name__ == "__main__":
    main()
