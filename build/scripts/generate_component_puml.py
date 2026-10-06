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
    params: str = ""       # non-empty on a member function


@dataclass
class TypeInfo:
    name: str
    kind: str           # "struct", "class", "enum", "enum class"
    members: list[MemberInfo] = field(default_factory=list)
    methods: list[MemberInfo] = field(default_factory=list)
    is_abstract: bool = False


# ---------------------------------------------------------------------------
# C++ header parser
# ---------------------------------------------------------------------------

# Matches C++ compound namespace declarations: namespace a::b::c { or namespace a { namespace b {
_NS_COMPOUND_RE = re.compile(r'^\s*namespace\s+([\w:]+)\s*\{')

# An attribute or an ALL_CAPS macro may sit between the keyword and the name,
# in either order and any number of times: `class [[nodiscard]] ORES_X_EXPORT
# name` and `class ORES_X_EXPORT [[nodiscard]] name` both occur.
_MODIFIERS = r'(?:(?:\[\[[^\]]*\]\]|[A-Z][A-Z0-9_]*)\s+)*'

# Matches struct/class/enum at namespace scope
_STRUCT_RE = re.compile(r'^\s*struct\s+' + _MODIFIERS + r'(\w+)\s*(?:final\s*)?\{')
_CLASS_RE = re.compile(
    r'^\s*class\s+' + _MODIFIERS +
    r'(\w+)'                       # class name
    r'(?:\s+final)?'               # optional final
    r'(?:\s*:\s*[^{]+)?'           # optional base classes
    r'\s*\{'                       # opening brace
)
_ENUM_CLASS_RE = re.compile(r'^\s*enum\s+class\s+(\w+)\s*(?::\s*[\w:]+\s*)?\{')
_ENUM_RE = re.compile(r'^\s*enum\s+(\w+)\s*\{')

# A type declared inside another type. The pass draws types at namespace scope
# only, so a nested declaration is left to the manual section; a one-line body
# balances its braces on the line and would otherwise read as a data member
# whose type is the keyword.
_NESTED_TYPE_RE = re.compile(r'^\s*(?:enum(?:\s+class)?|struct|class|union)\s+\w')

# Field patterns inside struct/class bodies
_FIELD_RE = re.compile(
    r'^\s*([\w:*&<>, ]+?)\s+(\w+)\s*'
    r'(?:=\s*[^;]+|\{(?:[^{}]|\{[^{}]*\})*\})?\s*;')
_ENUM_VAL_RE = re.compile(r'^\s*(\w+)\s*(?:=\s*[^,\n]+)?\s*,?\s*$')
_BLOCK_COMMENT_RE = re.compile(r'/\*.*?\*/')
_LINE_COMMENT_RE = re.compile(r'//.*$')

# A member function declaration or an inline definition. The name is the last
# identifier before the parameter list, so a qualified return type, a
# `template <...>` header and any number of specifiers may precede it. A
# declaration with no return type is a constructor or a destructor.
_METHOD_RE = re.compile(
    r'^\s*(?:template\s*<[^>]*>\s*)?'
    r'(?:(?:static|virtual|inline|constexpr|explicit)\s+)*'
    r'(?P<ret>[\w:<>,\*&\s\[\]]*?)\s*'
    r'(?P<name>~?\w+)\s*'
    # A parameter's type may carry parentheses of its own: a std::function or a
    # function-pointer parameter states its signature there. Excluding them
    # dropped the whole declaration, so the method never reached the diagram.
    r'\((?P<params>(?:[^;{}()]|\([^()]*\))*)\)\s*'
    r'(?:const\s*)?(?:noexcept\s*)?(?:override\s*)?(?:final\s*)?'
    r'(?:=\s*(?P<init>0|default|delete)\s*)?'
    r'(?P<tail>;|\{)'
)

# A leading token naming a statement rather than a declaration. A function
# body is skipped before reaching here, so these guard against a macro-heavy
# header presenting a statement at member depth.
_NOT_A_METHOD = frozenset({
    "if", "for", "while", "switch", "return", "catch", "do", "else",
    "sizeof", "static_assert", "using", "typedef", "assert", "throw",
})


def _strip_trailing_comment(line: str) -> str:
    """Drops a trailing comment so an enumeration value that carries one still reads."""
    return _LINE_COMMENT_RE.sub('', _BLOCK_COMMENT_RE.sub('', line)).rstrip()

# Visibility labels
_VISIBILITY_RE = re.compile(r'^\s*(public|protected|private)\s*:')

# Skip patterns: template, typedef, using, macros, constructors, destructors
_SKIP_LINE_RE = re.compile(
    r'^\s*(template\s*<|typedef|using\s|#|~|explicit\s|'
    r'virtual\s|static\s|inline\s|friend\s|//|/\*|\*|return\s)')

# An operator function: `operator=`, `operator==`, `operator[]`, `operator()`,
# `operator<<`, `operator new`, `operator delete`. A return type precedes the
# name, so this matches anywhere in the line rather than anchoring. It is a
# function, never a data member: the `=` in `operator=` precedes the
# parenthesis, so the field-versus-method test below would otherwise read
# `application& operator=(const application&) = delete;` as a data member
# named `operator`.
_OPERATOR_FUNC_RE = re.compile(
    r'\boperator\s*(?:[=!<>+\-*/%^&|~\[\]()]+|\bnew\b|\bdelete\b)')
_FUNC_RE = re.compile(r'\(')


def _simplify_type(t: str) -> str:
    """Shorten common std:: prefixes for readability."""
    t = t.strip()
    t = re.sub(r'\bstd::', '', t)
    t = re.sub(r'\s+', ' ', t)
    return t


def _simplify_params(raw: str) -> str:
    """The parameter list of a declaration, one simplified type per parameter.

    A parameter's default value is dropped: it says how a caller may omit the
    argument, which the diagram does not show.
    """
    parts: list[str] = []
    depth = 0
    current = ""
    for char in raw:
        if char in '<([':
            depth += 1
        elif char in '>)]':
            depth -= 1
        if char == ',' and depth == 0:
            parts.append(current)
            current = ""
        else:
            current += char
    parts.append(current)

    simplified = []
    for part in parts:
        part = _simplify_type(part.split('=')[0])
        if part:
            simplified.append(part)
    return ", ".join(simplified)


def _join_lines(lines: list[str], start: int) -> tuple[str, int]:
    """One declaration beginning at `start`, with its continuation lines.

    A return type and its name may sit on different lines, and a parameter
    list may wrap over several, so the lines are joined until the parentheses
    balance and the declaration ends. Bounded, so a malformed header cannot
    run away. Returns the joined text and how many lines it used.
    """
    parts: list[str] = []
    depth = 0
    for offset in range(0, 6):
        index = start + offset
        if index >= len(lines):
            break
        part = lines[index].strip()
        if offset > 0 and not part:
            break
        parts.append(part)
        depth += part.count('(') - part.count(')')
        if depth == 0 and (part.endswith(';') or part.endswith('{')):
            break
    return " ".join(parts), len(parts)


def _parse_method(line: str) -> Optional[MemberInfo]:
    """Read a line as a member function, or return None.

    A deleted or defaulted special member is not an API the reader needs, and
    a statement that reached member depth is not a declaration at all, so both
    are refused. So is a field whose initialiser calls something, which is the
    shape `uuid tenant_id = tenant_id::system();`.
    """
    stripped = line.strip()
    if not stripped or stripped.startswith(('#', '//', '/*', '*')):
        return None
    first_paren = stripped.find('(')
    equals = stripped.find('=')
    if first_paren == -1:
        return None
    if equals != -1 and equals < first_paren:
        return None
    m = _METHOD_RE.match(line)
    if not m:
        return None
    if m.group('init') in ('delete', 'default'):
        return None
    if m.group('name') in _NOT_A_METHOD:
        return None
    return MemberInfo(
        name=m.group('name'),
        type_str=_simplify_type(m.group('ret')),
        params=_simplify_params(m.group('params')))


def _inline_body_values(line: str) -> Optional[list[str]]:
    """
    Returns the comma-separated names in a declaration body that opens and
    closes on the same line, or None when the declaration is not of that shape.

    A one-line body must not be pushed onto the type stack: the only brace on
    the line closes the declaration itself, so treating the declaration as
    open would leave the parser one level too deep and swallow every following
    type as if it were a member of this one. The names are empty for an empty
    body, such as a one-line class.
    """
    open_index = line.find('{')
    if open_index == -1 or '}' not in line[open_index:]:
        return None

    body = line[open_index + 1:line.rindex('}')]
    values = []
    for item in body.split(','):
        name = re.sub(r'=.*$', '', item).strip()
        if name:
            values.append(name)
    return values


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

            # --- Braces that are neither a namespace nor a tracked type ---
            # A namespace closes when the depth falls back to the level that
            # opened it. A declaration may open or close more than one brace on
            # a line -- a two-level brace initialiser opens two -- so the line's
            # braces are counted rather than matched one at a time. A type
            # declaration does its own bookkeeping below and is left out, or its
            # brace would be counted twice and the enclosing namespace would
            # never close.
            is_type_decl = (_STRUCT_RE.match(line) or _CLASS_RE.match(line)
                            or _ENUM_CLASS_RE.match(line) or _ENUM_RE.match(line))
            if not is_type_decl and ('{' in stripped or '}' in stripped):
                brace_depth += stripped.count('{') - stripped.count('}')
                while ns_stack and brace_depth < len(ns_stack):
                    ns_stack.pop()
                # A line that only closes braces is not a declaration.
                if stripped.count('{') == 0:
                    preceding_was_template = False
                    i += 1
                    continue

            # --- Type declarations ---
            if not preceding_was_template:
                # A declaration whose body opens and closes on the same line is
                # already complete: emit it and stay at the current brace depth.
                inline_body = _inline_body_values(line)

                def emit_inline(type_name: str, type_kind: str) -> None:
                    ti = TypeInfo(name=type_name, kind=type_kind)
                    ti.members = [MemberInfo(name=v, type_str="", visibility="+")
                                  for v in inline_body]
                    results[current_ns()].append(ti)

                m = _ENUM_CLASS_RE.match(line)
                if m:
                    if inline_body is not None:
                        emit_inline(m.group(1), "enum class")
                    else:
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
                    if inline_body is not None:
                        emit_inline(m.group(1), "enum")
                    else:
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
                    if inline_body is not None:
                        emit_inline(m.group(1), "struct")
                    else:
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
                        if inline_body is not None:
                            emit_inline(name, "class")
                        else:
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

            # A nested type declaration is not a data member. A one-line body
            # balances its braces, so without this it reaches the field pattern
            # below and is drawn as a member named after the type.
            if opens > 0 and opens == closes and _NESTED_TYPE_RE.match(stripped):
                i += 1
                continue

            # A member may carry a brace initialiser, so its line holds braces
            # that balance on the line itself. Read it before the brace
            # bookkeeping below, which would otherwise consume the line as a
            # brace event and drop the member from the diagram. A wrapped
            # declaration leaves an unmatched parenthesis on its continuation
            # line, which would otherwise read as a field.
            if (type_member_depth == 0 and opens > 0 and opens == closes
                    and not stripped.startswith('#') and ';' in stripped
                    and stripped.count('(') == stripped.count(')')):
                m = _FIELD_RE.match(line)
                if m:
                    t = _simplify_type(m.group(1))
                    n = m.group(2)
                    if t and n and not t.isupper() and not n.isupper():
                        current_type.members.append(
                            MemberInfo(name=n, type_str=t, visibility=visibility))
                        i += 1
                        continue

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
                # An inline definition opens its body on the declaration line,
                # so this is where a member function with a body is read. The
                # brace is still counted below, which is what keeps the body
                # from being read as if it were still at member depth.
                method = _parse_method(line)
                if method is not None:
                    method.visibility = visibility
                    current_type.methods.append(method)
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
                    m = _ENUM_VAL_RE.match(_strip_trailing_comment(stripped))
                    if m:
                        current_type.members.append(MemberInfo(name=m.group(1), type_str="", visibility="+"))
                i += 1
                continue

            # A member function, which the skip list would otherwise swallow
            # whenever a specifier such as `static` opens the line. The
            # declaration may be split across lines, so it is read joined. Its
            # braces are accounted for here, because consuming the line skips
            # the brace bookkeeping below: an inline body that opens without
            # closing would otherwise leave the parser reading the body as if
            # it were still at member depth.
            declaration, used = _join_lines(lines, i)
            method = _parse_method(declaration)
            if method is not None:
                method.visibility = visibility
                current_type.methods.append(method)
                consumed = lines[i:i + used]
                opens = sum(part.count('{') for part in consumed)
                closes = sum(part.count('}') for part in consumed)
                brace_depth += opens - closes
                type_member_depth += opens - closes
                i += used
                continue

            # Skip template, using, friend, operator, etc.
            if _SKIP_LINE_RE.match(line):
                i += 1
                continue

            # A parenthesis means a method, unless it belongs to a field's
            # initialiser: `uuid tenant_id = tenant_id::system();` is a field,
            # and skipping it hid tenant_id from every generated entity box and
            # any member whose initialiser calls something. A parenthesis
            # before the `=` (or with no `=` at all) is a signature, and a line
            # naming an operator is a signature whatever the `=` does.
            first_paren = stripped.find('(')
            if first_paren != -1 and (
                    _OPERATOR_FUNC_RE.search(stripped)
                    or not (stripped.find('=') != -1
                            and stripped.find('=') < first_paren)):
                i += 1
                continue

            # Field: type name;  (include all visibility levels)
            # A wrapped declaration leaves an unmatched parenthesis on its
            # continuation line, which would otherwise read as a field.
            if stripped and ';' in stripped and stripped.count('(') == stripped.count(')'):
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
        for m in t.methods:
            signature = f"{m.name}({m.params})"
            if m.type_str:
                lines.append(f"{ind}    {m.visibility}{signature} : {m.type_str}")
            else:
                lines.append(f"{ind}    {m.visibility}{signature}")
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

def merge_with_existing(new_auto: str, existing_path: Path) -> Optional[str]:
    """
    Replace the auto-generated section (before sentinel) in the existing file
    while preserving everything after the sentinel.

    Return ``None`` when the existing file carries no sentinel. Such a file is
    hand-authored, or predates the sentinel, and the generator has no safe way
    to tell its manual body from a section it may overwrite. Replacing it would
    destroy the body, so the caller refuses instead.
    """
    existing = existing_path.read_text(encoding='utf-8')
    if SENTINEL not in existing:
        return None

    after = existing.split(SENTINEL, 1)[1]
    # new_auto already ends with sentinel + newline; append the preserved tail
    new_auto_before = new_auto.split(SENTINEL, 1)[0] + SENTINEL
    return new_auto_before + after


# ---------------------------------------------------------------------------
# Project discovery and processing
# ---------------------------------------------------------------------------

def _project_dir(project_name: str) -> Optional[Path]:
    """
    Resolve a component name to its directory.

    A component directory carries the first two segments of the name, so
    ores.ore is projects/ores.ore. A composite part adds a directory per
    remaining segment, so ores.ore.core is projects/ores.ore/core. A name
    with one segment is a directory of its own.
    """
    parts = project_name.split(".")
    if len(parts) <= 1:
        candidate = PROJECTS_ROOT / project_name
    else:
        candidate = PROJECTS_ROOT / ".".join(parts[:2])
        for segment in parts[2:]:
            candidate = candidate / segment
    return candidate if candidate.is_dir() else None


def find_all_projects() -> list[str]:
    """Return every component name the generator can diagram.

    A simple component is a directory with include/. A composite's parts
    are nested one level below it, so a walk that only looked at
    projects/* never saw ores.ore.core at projects/ores.ore/core.
    """
    projects = []
    for p in sorted(PROJECTS_ROOT.iterdir()):
        if not p.is_dir():
            continue
        if (p / "include").is_dir():
            projects.append(p.name)
            continue
        for part in sorted(p.iterdir()):
            if part.is_dir() and (part / "include").is_dir():
                projects.append(f"{p.name}.{part.name}")
    return projects


def _find_include_dir(project_name: str) -> Optional[Path]:
    """
    Find the header root for a project.
    Primary: include/<project_name>/
    Fallback: any direct subdirectory of include/ (for projects whose header
    root is not named after the project)
    """
    project_root = _project_dir(project_name)
    if project_root is None:
        return None
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
    project_root = _project_dir(project_name)
    if include_dir is None or project_root is None:
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

    modeling_dir = project_root / "modeling"
    out_path = modeling_dir / f"{project_name}.puml"

    if out_path.exists():
        merged = merge_with_existing(new_auto, out_path)
        if merged is None:
            print(f"  {project_name}: SKIPPED {out_path} -- no manual sentinel, "
                  f"so the file is not generator-managed and overwriting it "
                  f"would destroy hand-authored content", file=sys.stderr)
            return False
        final_content = merged
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
