#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
"""
Generate skeleton PlantUML class diagrams from C++ headers.

Usage:
    generate_component_puml.py [--project NAME] [--all] [--dry-run]
    generate_component_puml.py --help

For each component, reads projects/<name>/include/<name>/**/*.hpp and emits
projects/<name>/modeling/<name>.puml.

If the target .puml already exists, only the auto-generated section (before the
manual sentinel line) is regenerated; everything after the sentinel is preserved.

Sentinel line:
    ' --- manual: everything below this line is hand-authored; the script preserves it ---
"""
from __future__ import annotations

import argparse
import difflib
import re
import sys
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

SENTINEL = "' --- manual: everything below this line is hand-authored; the script preserves it ---"
PROJECTS_ROOT = Path(__file__).resolve().parent.parent.parent / "projects"
NAMESPACE_FILL = "#F2F2F2"
GPL_HEADER = """\
' -*- mode: plantuml; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
'
' Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
'
' This program is free software; you can redistribute it and/or modify it under
' the terms of the GNU General Public License as published by the Free Software
' Foundation; either version 3 of the License, or (at your option) any later
' version.
'
' This program is distributed in the hope that it will be useful, but WITHOUT
' ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
' FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License along with
' this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
' Street, Fifth Floor, Boston, MA 02110-1301, USA.
'"""

# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------

@dataclass
class MemberInfo:
    name: str
    type_str: str
    visibility: str = "+"  # + public, - private, # protected


@dataclass
class TypeInfo:
    name: str
    kind: str           # "struct", "class", "enum", "enum class"
    members: list[MemberInfo] = field(default_factory=list)
    is_abstract: bool = False


# ---------------------------------------------------------------------------
# C++ header parser
# ---------------------------------------------------------------------------

# Matches C++ compound namespace declarations: namespace a::b::c { or namespace a { namespace b {
_NS_COMPOUND_RE = re.compile(r'^\s*namespace\s+([\w:]+)\s*\{')

# Matches struct/class/enum at namespace scope
_STRUCT_RE = re.compile(r'^\s*struct\s+(?:[A-Z][A-Z0-9_]*\s+)?(\w+)\s*(?:final\s*)?\{')
_CLASS_RE = re.compile(
    r'^\s*class\s+'
    r'(?:[A-Z][A-Z0-9_]*\s+)?'   # optional ALL_CAPS export macro
    r'(\w+)'                       # class name
    r'(?:\s+final)?'               # optional final
    r'(?:\s*:\s*[^{]+)?'           # optional base classes
    r'\s*\{'                       # opening brace
)
_ENUM_CLASS_RE = re.compile(r'^\s*enum\s+class\s+(\w+)\s*(?::\s*[\w:]+\s*)?\{')
_ENUM_RE = re.compile(r'^\s*enum\s+(\w+)\s*\{')

# Field patterns inside struct/class bodies
_FIELD_RE = re.compile(r'^\s*([\w:*&<>, ]+?)\s+(\w+)\s*(?:=\s*[^;]+)?\s*;')
_ENUM_VAL_RE = re.compile(r'^\s*(\w+)\s*(?:=\s*[^,\n]+)?\s*,?\s*$')

# Visibility labels
_VISIBILITY_RE = re.compile(r'^\s*(public|protected|private)\s*:')

# Skip patterns: template, typedef, using, macros, operators, constructors, destructors
_SKIP_LINE_RE = re.compile(
    r'^\s*(template\s*<|typedef|using\s|#|operator|~|explicit\s|'
    r'virtual\s|static\s|inline\s|friend\s|//|/\*|\*|return\s)')
_FUNC_RE = re.compile(r'\(')


def _simplify_type(t: str) -> str:
    """Shorten common std:: prefixes for readability."""
    t = t.strip()
    t = re.sub(r'\bstd::', '', t)
    t = re.sub(r'\s+', ' ', t)
    return t


def parse_header(path: Path) -> dict[tuple[str, ...], list[TypeInfo]]:
    """
    Parse a C++ header and return {namespace_tuple: [TypeInfo, ...]}.
    Skips template specialisations, anonymous types, nested classes.
    Logs a warning for lines that can't be parsed.
    """
    results: dict[tuple[str, ...], list[TypeInfo]] = defaultdict(list)
    text = path.read_text(encoding='utf-8', errors='replace')
    lines = text.splitlines()

    ns_stack: list[list[str]] = []   # stack of namespace segment lists
    brace_depth = 0
    type_brace_depth: Optional[int] = None  # brace depth when we entered current type
    type_member_depth: int = 0  # nesting inside type body (0 = top level of type)
    current_type: Optional[TypeInfo] = None
    visibility = "+"   # default: public for structs, private for classes
    preceding_was_template = False
    in_comment_block = False

    def current_ns() -> tuple[str, ...]:
        return tuple(seg for segs in ns_stack for seg in segs)

    i = 0
    while i < len(lines):
        line = lines[i]
        stripped = line.strip()

        # Block comment tracking: strip block comment content from line before parsing
        if in_comment_block:
            if '*/' in stripped:
                in_comment_block = False
                # Process only the content after the closing */
                line = line[line.index('*/') + 2:]
                stripped = line.strip()
            else:
                preceding_was_template = False
                i += 1
                continue
        if '/*' in line:
            if '*/' in line:
                # Inline block comment: remove it and continue with the rest
                line = re.sub(r'/\*.*?\*/', '', line)
                stripped = line.strip()
            else:
                in_comment_block = True
                line = line[:line.index('/*')]
                stripped = line.strip()
        if not stripped or stripped.startswith('//') or stripped.startswith('*'):
            preceding_was_template = False
            i += 1
            continue

        # Track template lines (next type declaration should be skipped)
        if stripped.startswith('template'):
            preceding_was_template = True
            i += 1
            continue

        if current_type is None:
            # --- Namespace detection ---
            m = _NS_COMPOUND_RE.match(line)
            if m:
                ns_part = m.group(1)
                segments = [s for s in ns_part.split('::') if s]
                ns_stack.append(segments)
                brace_depth += 1
                preceding_was_template = False
                i += 1
                continue

            # --- Closing brace (could close namespace or be unrelated) ---
            if stripped == '}' or stripped == '};':
                if brace_depth > 0:
                    brace_depth -= 1
                    if ns_stack and brace_depth == len(ns_stack) - 1:
                        ns_stack.pop()
                preceding_was_template = False
                i += 1
                continue

            # Count other opening braces not captured above
            if '{' in stripped and not stripped.startswith('//'):
                # Not a namespace or type we care about; just track depth
                if '}' not in stripped:
                    brace_depth += 1

            # --- Type declarations ---
            if not preceding_was_template:
                m = _ENUM_CLASS_RE.match(line)
                if m:
                    current_type = TypeInfo(name=m.group(1), kind="enum class")
                    type_brace_depth = brace_depth
                    type_member_depth = 0
                    brace_depth += 1
                    visibility = "+"
                    preceding_was_template = False
                    i += 1
                    continue

                m = _ENUM_RE.match(line)
                if m and 'class' not in line:
                    current_type = TypeInfo(name=m.group(1), kind="enum")
                    type_brace_depth = brace_depth
                    type_member_depth = 0
                    brace_depth += 1
                    visibility = "+"
                    preceding_was_template = False
                    i += 1
                    continue

                m = _STRUCT_RE.match(line)
                if m:
                    current_type = TypeInfo(name=m.group(1), kind="struct")
                    type_brace_depth = brace_depth
                    type_member_depth = 0
                    brace_depth += 1
                    visibility = "+"
                    preceding_was_template = False
                    i += 1
                    continue

                m = _CLASS_RE.match(line)
                if m:
                    name = m.group(1)
                    if name not in ('EXPORT', 'API', 'final', 'override'):
                        current_type = TypeInfo(name=name, kind="class")
                        type_brace_depth = brace_depth
                        type_member_depth = 0
                        brace_depth += 1
                        visibility = "-"  # class members default private
                        preceding_was_template = False
                        i += 1
                        continue

        else:
            # --- Inside a type body ---
            opens = stripped.count('{')
            closes = stripped.count('}')

            # Closing brace(s): check if we're leaving the type
            if closes > 0:
                brace_depth = brace_depth - closes + opens
                type_member_depth = max(0, type_member_depth - closes + opens)
                if brace_depth <= type_brace_depth:
                    ns = current_ns()
                    if current_type and current_type.name:
                        results[ns].append(current_type)
                    current_type = None
                    type_brace_depth = None
                    type_member_depth = 0
                    visibility = "+"
                preceding_was_template = False
                i += 1
                continue

            # Opening brace without closing: entering nested block (function body, etc.)
            if opens > 0:
                brace_depth += opens
                type_member_depth += opens
                i += 1
                continue

            # If we're inside a nested block (function body etc.), skip field extraction
            if type_member_depth > 0:
                i += 1
                continue

            # Visibility label
            m = _VISIBILITY_RE.match(line)
            if m:
                vis_word = m.group(1)
                visibility = "+" if vis_word == 'public' else ("-" if vis_word == 'private' else "#")
                i += 1
                continue

            # Enum values
            if current_type.kind in ("enum", "enum class"):
                if stripped and not stripped.startswith('//') and not stripped.startswith('/*'):
                    m = _ENUM_VAL_RE.match(stripped)
                    if m:
                        current_type.members.append(MemberInfo(name=m.group(1), type_str="", visibility="+"))
                i += 1
                continue

            # Skip template, using, friend, operator, etc.
            if _SKIP_LINE_RE.match(line):
                i += 1
                continue

            # Skip lines with function parameters (methods/constructors)
            if _FUNC_RE.search(stripped):
                i += 1
                continue

            # Field: type name;  (include all visibility levels)
            if stripped and ';' in stripped:
                m = _FIELD_RE.match(line)
                if m:
                    t = _simplify_type(m.group(1))
                    n = m.group(2)
                    # Avoid macro-like names and empty types
                    if t and n and not t.isupper() and not n.isupper():
                        current_type.members.append(MemberInfo(name=n, type_str=t, visibility=visibility))

        preceding_was_template = False
        i += 1

    return dict(results)


# ---------------------------------------------------------------------------
# PlantUML emitter
# ---------------------------------------------------------------------------

def _indent(depth: int) -> str:
    return "    " * depth


def _emit_type(t: TypeInfo, depth: int) -> list[str]:
    ind = _indent(depth)
    lines: list[str] = []

    if t.kind in ("enum", "enum class"):
        lines.append(f"{ind}enum {t.name} {{")
        for m in t.members:
            lines.append(f"{ind}    {m.name}")
        lines.append(f"{ind}}}")
    else:
        stereotype = " <<struct>>" if t.kind == "struct" else ""
        abstract_kw = "abstract class" if t.is_abstract else "class"
        if t.kind == "struct":
            abstract_kw = "class"
        lines.append(f"{ind}{abstract_kw} {t.name}{stereotype} {{")
        for m in t.members:
            if m.type_str:
                lines.append(f"{ind}    {m.visibility}{m.name} : {m.type_str}")
            else:
                lines.append(f"{ind}    {m.visibility}{m.name}")
        lines.append(f"{ind}}}")

    return lines


def _build_ns_tree(data: dict[tuple[str, ...], list[TypeInfo]]) -> dict:
    """Build a nested dict tree from namespace → types."""
    tree: dict = {}
    for ns_tuple, types in data.items():
        node = tree
        for seg in ns_tuple:
            node = node.setdefault(seg, {})
        node.setdefault('__types__', []).extend(types)
    return tree


def _emit_ns_tree(tree: dict, depth: int) -> list[str]:
    lines: list[str] = []
    for key, subtree in sorted(tree.items()):
        if key == '__types__':
            continue
        ind = _indent(depth)
        lines.append(f"{ind}namespace {key} {NAMESPACE_FILL} {{")
        # Emit types in this namespace
        for t in subtree.get('__types__', []):
            lines.extend(_emit_type(t, depth + 1))
        # Recurse into sub-namespaces
        lines.extend(_emit_ns_tree(subtree, depth + 1))
        lines.append(f"{ind}}}")
    return lines


def generate_puml(project_name: str, all_types: dict[tuple[str, ...], list[TypeInfo]]) -> str:
    """Render the auto-generated section of a .puml file."""
    tree = _build_ns_tree(all_types)

    out: list[str] = []
    out.append(GPL_HEADER)
    out.append("@startuml")
    out.append("")
    out.append(f"title {project_name} Component")
    out.append("")
    out.append("set namespaceSeparator ::")
    out.append("")

    if tree:
        out.extend(_emit_ns_tree(tree, 0))
    else:
        # No types found; emit a stub namespace matching the project name
        segs = project_name.split('.')
        ind_open = []
        for depth, seg in enumerate(segs):
            ind_open.append(f"{_indent(depth)}namespace {seg} {NAMESPACE_FILL} {{")
        ind_open.append(f"{_indent(len(segs))}' Core types to be added.")
        for depth in range(len(segs) - 1, -1, -1):
            ind_open.append(f"{_indent(depth)}}}")
        out.extend(ind_open)

    out.append("")
    out.append(SENTINEL)
    out.append("")
    # Empty manual section placeholder (with local-vars footer so new files get it too)
    out.append(f"' Local Variables:")
    out.append(f"' compile-command: \"java -Djava.awt.headless=true -DPLANTUML_SECURITY_PROFILE=UNSECURE -DPLANTUML_LIMIT_SIZE=65535 -jar /usr/share/plantuml/plantuml.jar {project_name}.puml\"")
    out.append(f"' End:")
    out.append("@enduml")
    return "\n".join(out) + "\n"


# ---------------------------------------------------------------------------
# File handling (sentinel-aware merge)
# ---------------------------------------------------------------------------

def merge_with_existing(new_auto: str, existing_path: Path) -> str:
    """
    Replace the auto-generated section (before sentinel) in the existing file
    while preserving everything after the sentinel.
    """
    existing = existing_path.read_text(encoding='utf-8')
    if SENTINEL not in existing:
        # No sentinel: replace entire file
        return new_auto

    after = existing.split(SENTINEL, 1)[1]
    # new_auto already ends with sentinel + newline; append the preserved tail
    new_auto_before = new_auto.split(SENTINEL, 1)[0] + SENTINEL
    return new_auto_before + after


# ---------------------------------------------------------------------------
# Project discovery and processing
# ---------------------------------------------------------------------------

def find_all_projects() -> list[str]:
    """Return all project names that have an include/ directory."""
    projects = []
    for p in sorted(PROJECTS_ROOT.iterdir()):
        if p.is_dir() and (p / "include").is_dir():
            projects.append(p.name)
    return projects


def _find_include_dir(project_name: str) -> Optional[Path]:
    """
    Find the header root for a project.
    Primary: include/<project_name>/
    Fallback: any direct subdirectory of include/ (for projects like ores.qt.*)
    """
    project_root = PROJECTS_ROOT / project_name
    primary = project_root / "include" / project_name
    if primary.is_dir():
        return primary
    include_base = project_root / "include"
    if include_base.is_dir():
        subdirs = [d for d in include_base.iterdir() if d.is_dir()]
        if len(subdirs) == 1:
            return subdirs[0]
    return None


def process_project(project_name: str, dry_run: bool) -> bool:
    """
    Parse headers, generate .puml, write or print.
    Returns True if changes were made (or would be made).
    """
    include_dir = _find_include_dir(project_name)
    if include_dir is None:
        print(f"  [WARN] No include/ directory found for {project_name}", file=sys.stderr)
        return False

    headers = sorted(include_dir.rglob("*.hpp"))
    if not headers:
        print(f"  [WARN] No .hpp files under {include_dir}", file=sys.stderr)
        return False

    # Collect all types across all headers
    all_types: dict[tuple[str, ...], list[TypeInfo]] = defaultdict(list)
    for hpp in headers:
        # Skip the aggregate include header (e.g. ores.logging.hpp)
        if hpp.name == f"{project_name}.hpp" or hpp.name == "export.hpp":
            continue
        try:
            partial = parse_header(hpp)
            for ns, types in partial.items():
                all_types[ns].extend(types)
        except Exception as exc:
            print(f"  [WARN] Failed to parse {hpp}: {exc}", file=sys.stderr)

    new_auto = generate_puml(project_name, dict(all_types))

    modeling_dir = PROJECTS_ROOT / project_name / "modeling"
    out_path = modeling_dir / f"{project_name}.puml"

    if out_path.exists():
        final_content = merge_with_existing(new_auto, out_path)
    else:
        final_content = new_auto

    if out_path.exists():
        existing = out_path.read_text(encoding='utf-8')
        if existing == final_content:
            print(f"  {project_name}: no changes")
            return False

    if dry_run:
        print(f"  {project_name}: would write {out_path}")
        # Show a diff-style preview of what changed
        if out_path.exists():
            existing = out_path.read_text(encoding='utf-8')
            diff = list(difflib.unified_diff(
                existing.splitlines(keepends=True),
                final_content.splitlines(keepends=True),
                fromfile=str(out_path),
                tofile=str(out_path) + " (new)",
                n=3
            ))
            if diff:
                print("".join(diff[:80]))  # cap output
        else:
            print(f"    (new file, {len(final_content)} bytes)")
        return True
    else:
        modeling_dir.mkdir(exist_ok=True)
        out_path.write_text(final_content, encoding='utf-8')
        action = "updated" if out_path.exists() else "created"
        print(f"  {project_name}: {action} {out_path}")
        return True


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--project", metavar="NAME",
                       help="regenerate one project by name")
    group.add_argument("--all", action="store_true",
                       help="regenerate all projects with a include/ directory")
    parser.add_argument("--dry-run", action="store_true",
                        help="print what would change without writing files")
    args = parser.parse_args()

    if args.all:
        projects = find_all_projects()
        print(f"Found {len(projects)} projects with include/ directories")
    else:
        projects = [args.project]

    changed = 0
    for proj in projects:
        if process_project(proj, dry_run=args.dry_run):
            changed += 1

    verb = "would change" if args.dry_run else "changed"
    print(f"\n{changed}/{len(projects)} projects {verb}.")


if __name__ == "__main__":
    main()
