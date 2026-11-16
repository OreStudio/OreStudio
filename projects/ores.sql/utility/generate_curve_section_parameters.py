# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
"""Write the wiring that ties a curve section's columns to its ORE leaves.

A curve configuration's entry is not one table: =curve_definition= holds the
identifier and the description every entry shares, and one family table per
section holds the rest. The projection that reads and writes =curveconfig.xml=
is the authority on which column of which table holds which element, because it
is the code the import and the export already run. This script reads that
projection and writes the same facts as TypeScript, so the curve screen reads a
value by the leaf it belongs to instead of guessing it from a name.

    generate_curve_section_parameters.py          write the wiring module
    generate_curve_section_parameters.py --check  fail if the committed module differs

The wiring is written twice over one read of the projection: as TypeScript, for
the screen the browser runs today, and as C++, for the routes the gateway answers
instead. Both are committed, so the two surfaces cannot drift apart.

The script reads, in this order, the section table of the projection (the order
the schema declares the sections in), the import function of each section, the
table row the function stores an entry in, and the element each column is read
from. A column whose value the projection reads from a child element is not an
entry parameter: it is a leaf of that child, and the child is reported as a
block the entry holds. A column that records whether a child element is present
is reported as that block's presence, not as a parameter.

The XPath of each parameter is the one the ORE parameter contract keys the leaf
by, built from the section, the entry element and the element's name. The script
checks every element against the ORE schema, which is the authority on the
children an entry may hold, and fails on an element the schema does not declare
there. Whether the contract documents the leaf is not this script's question: an
undocumented leaf is a gap the screen shows, not a mapping the script may drop.
"""

import argparse
import os
import re
import sys
import textwrap

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
PROJECTION = os.path.join(
    ROOT, "projects", "ores.ore", "core", "src", "domain",
    "curve_configuration_projection.cpp")
SCHEMA = os.path.join(ROOT, "external", "ore", "xsd", "curveconfig.xsd")
OUT = os.path.join(
    ROOT, "projects", "ores.web", "packages", "bff", "src",
    "curve-sections.generated.ts")

SQL_OUT = os.path.join(
    ROOT, "projects", "ores.sql", "create", "refdata",
    "refdata_curve_configuration_parts_validate_fn_create.sql")
SCHEMA_NAME = "curveconfig.xsd"
ROOT_ELEMENT = "CurveConfiguration"

# The table that holds the identifier and the description every entry shares.
DEFINITION_ENTITY = "curve_definition"

# The column an entry's family row is keyed back to its definition by.
DEFINITION_KEY = "curve_definition_id"

LICENCE = """\
/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
"""


def source():
    with open(PROJECTION, encoding="utf-8") as handle:
        return handle.read()


def function_body(text, name):
    """The body of one function, from its opening brace to its matching one."""
    match = re.search(r"^void " + name + r"\((.*?)\)\s*\{", text, re.S | re.M)
    if match is None:
        return None
    index = match.end()
    depth = 1
    while depth > 0:
        character = text[index]
        if character == "{":
            depth += 1
        elif character == "}":
            depth -= 1
        index += 1
    return text[match.end():index - 1]


def declared_sections(text):
    """The section table: the order the schema declares the sections in."""
    names = {prefix: element for prefix, element in re.findall(
        r'constexpr std::string_view (\w+)_section =\s*"(\w+)";', text)}
    table = re.search(r"const std::vector<section_access>& sections\(\)\s*\{(.*?)\n    \};",
                      text, re.S)
    if table is None:
        raise SystemExit("the projection's section table was not found")
    sections = []
    for prefix, entry in re.findall(r"\{(\w+)_section,\s*\"(\w+)\"\}", table.group(1)):
        element = names.get(prefix)
        if element is None:
            raise SystemExit(f"the section constant {prefix}_section names no element")
        sections.append((element, entry))
    return sections


def entry_imports(text):
    """Each section's import function, as the document mapper calls it."""
    return {section: name for section, _, name in re.findall(
        r'for \(const auto& e : each\("(\w+)", "(\w+)"\)\)\s*\n\s*(import_\w+)\(', text)}


def rows_of(body):
    """The assignments of a function body, as the variable, the column and the value."""
    return [match.group(1, 2, 3) for match in
            re.finditer(r"(\w+)\.(\w+)\s*=\s*([^;]+);", body)]


def stored_rows(body):
    """The document vectors a function stores its rows in, and the variable of each."""
    return re.findall(r"out\.(\w+)\.push_back\(std::move\((\w+)\)\)", body)


def entities_of(body):
    """The declared variables of a function and the entity each one holds."""
    return {variable: entity for entity, variable in
            re.findall(r"refdata::domain::(\w+)\s+(\w+)\s*;", body)}


def definition_leaves(body):
    """The identifier and the description every entry shares, from add_definition."""
    call = re.search(r"add_definition\(out,\s*\w+_section,(.*?)\);", body, re.S)
    if call is None:
        return []
    return re.findall(r'required_text\(v,\s*"(\w+)"\)', call.group(1))


def read_element(expression):
    """The element an assignment reads, and the variable it reads it from.

    A value is read either by a helper that takes the reader first, such as
    =required_spelling(v, "Currency")=, or by a method on the reader itself, such
    as =v.spelling("InterpolationMethod")=. Anything else, such as an identifier
    the projection mints or a key it copies, is not a leaf.
    """
    flat = " ".join(expression.split())
    call = re.fullmatch(r"(\w+)\((.*)\)", flat)
    if call is not None:
        helper, arguments = call.group(1), call.group(2)
        first = arguments.split(",")[0].strip().lstrip("*")
        elements = re.findall(r'"(\w+)"', flat)
        if first.startswith("v") and elements:
            return first, elements[0]
        return None, None
    method = re.fullmatch(r"(\w+)(?:\.|->)(has|text|spelling|integer|real|flag|decimal)\("
                          r'"(\w+)"\)', flat)
    if method is not None:
        return method.group(1), method.group(3)
    return None, None


def blocks_of(body):
    """The child elements an entry holds, in the order the projection reads them."""
    found = re.findall(r'(\w+)(?:\.|->)child\("(\w+)"\)', body)
    names = []
    for variable, element in found:
        if variable == "v" and element not in names:
            names.append(element)
    return names


def block_entity(body, element, entity_of, text):
    """The entity whose row the projection writes inside one child element, if any.

    A construction an element selects is a part of the entry, or child rows of
    it, rather than a flag on the entry's row. The block is present when the
    entry has a row of the entity its branch writes: a part its reader keeps, or
    a row the branch declares itself.
    """
    opened = re.search(r'const auto (\w+) = v\.child\("' + element + r'"\); \1\) \{', body)
    if opened is None:
        return ""
    index = opened.end()
    depth = 1
    while depth > 0:
        character = body[index]
        if character == "{":
            depth += 1
        elif character == "}":
            depth -= 1
        index += 1
    branch = body[opened.end():index - 1]
    call = re.search(r"\b(import_\w+)\(ctx, [\w.]+, \*" + opened.group(1) + r"\);", branch)
    if call is not None:
        reader = definition_body(text, call.group(1)) or ""
        declared = re.search(r"refdata::domain::(\w+) \w+;", reader)
        return declared.group(1) if declared else ""
    declared = re.search(r"refdata::domain::(\w+) (\w+);", branch)
    return declared.group(1) if declared else ""


def definition_body(text, name):
    """The body of the definition of one function or method, by name.

    A declaration ends in a semicolon, so only the run whose arguments are
    followed by a brace is a definition. The methods of =import_context= are read
    this way: they are the projection's own statement of what a child row holds.
    """
    for match in re.finditer(r"\b" + name + r"\([^;{}]*\)\s*\{", text):
        index = match.end()
        depth = 1
        while depth > 0:
            character = text[index]
            if character == "{":
                depth += 1
            elif character == "}":
                depth -= 1
            index += 1
        return text[match.end():index - 1]
    return None


def collection_rules(text):
    """The child rows an entry holds, from the projection's own readers.

    A curve entry's segments are a collection: one row per =Segments= child, the
    row's kind named by its =Type=, and each row's quotes under it. The projection
    states every fact that places those leaves -- the row, the vector it fills, the
    key back to the entry, the column holding the kind, and the column a quote
    joins its segment by -- so the rule is read from it rather than restated in the
    BFF.
    """
    segment = definition_body(text, "segment")
    quotes = definition_body(text, "import_quotes")
    if segment is None:
        return [], ["the projection has no import_context::segment"]
    if quotes is None:
        return [], ["the projection has no import_quotes"]
    row = re.search(r"refdata::domain::(\w+)\s+(\w+);", segment)
    vector = re.search(r"out\.(\w+)\.push_back", segment)
    quote_row = re.search(r"refdata::domain::(\w+)\s+(\w+);", quotes)
    problems = []
    for what, found in (("the segment row", row), ("the segment vector", vector),
                        ("the quote row", quote_row)):
        if found is None:
            problems.append(f"the projection does not state {what}")
    if problems:
        return [], problems
    parent = re.search(r"\b" + row.group(2) + r"\.(\w+)\s*=\s*definition_id;", segment)
    kind = re.search(r"\b" + row.group(2) + r"\.(\w+)\s*=\s*type;", segment)
    quote_parent = re.search(
        r"\b" + quote_row.group(2) + r"\.(\w+)\s*=\s*segment_id;", quotes)
    segment_keys = {key for entity, (_, key) in part_traits(text).items()
                    if entity.startswith(row.group(1) + "_")} if row else set()
    part_parent = re.fullmatch(r"(\w+)", next(iter(segment_keys))) if len(segment_keys) == 1 \
        else None
    for what, found in (("the segment's key to its entry", parent),
                        ("the part's key to its segment", part_parent),
                        ("the segment's kind column", kind),
                        ("the quote's key to its segment", quote_parent)):
        if found is None:
            problems.append(f"the projection does not state {what}")
    if problems:
        return [], problems
    return [{
        "entity": row.group(1),
        "vector": vector.group(1),
        "parentKey": parent.group(1),
        "kindColumn": kind.group(1),
        "quoteParent": quote_parent.group(1),
        "partParent": part_parent.group(1),
    }], []


def kind_leaf(expression, reader):
    """The element a kind's assignment reads, named after the kind's own reader.

    The kinds are read from a loop variable, not from =v=, so =read_element= does
    not apply: it is the entry's reader that it knows.
    """
    flat = " ".join(expression.split())
    call = re.fullmatch(r"(\w+)\((.*)\)", flat)
    if call is not None:
        first = call.group(2).split(",")[0].strip().lstrip("*")
        elements = re.findall(r'"(\w+)"', flat)
        return elements[0] if first == reader and elements else None
    method = re.fullmatch(r"(\w+)(?:\.|->)\w+\(" r'"(\w+)"\)', flat)
    return method.group(2) if method is not None and method.group(1) == reader else None


KIND_LOOP = re.compile(
    r'for \(const auto& (\w+) : v\.children\("(\w+)"\)\) \{(.*?)\n    \}', re.S)

# The table that names the document vector each segment part fills.
# The table that names the document vector and the key of each part.
PART_TRAIT = re.compile(
    r"struct document_part<refdata::domain::(\w+)> \{\s*"
    r"static constexpr auto vector\s*=\s*&\w+::(\w+);\s*"
    r"static constexpr auto key\s*=\s*&refdata::domain::\w+::(\w+);")

# A call to one of the readers that fill a segment's parts, and the reader it
# hands on.
PART_CALL = re.compile(r"\b(import_\w+)\(ctx, \w+, (\w+)[,)]")


# A call an entry's import makes to the reader of one of its parts, handed the
# entry's own reader. A part read from a child element holds no entry leaves.
ENTRY_PART_CALL = re.compile(r"\b(import_\w+)\(ctx, [\w.]+, v\);")


def part_traits(text):
    """The document vector and the key of every part, by the part's entity."""
    return {entity: (vector, key) for entity, vector, key in PART_TRAIT.findall(text)}


def part_vectors(text):
    """The document vector of every part, by the part's entity."""
    return {entity: vector for entity, (vector, _) in part_traits(text).items()}


def part_reader(text, name):
    """The reader parameter of one part reader, and its body."""
    signature = re.search(r"^void " + name + r"\(([^)]*)\)\s*\{", text, re.M)
    if signature is None:
        return None, None
    reader = re.search(r"const record_reader& (\w+)", signature.group(1))
    return (reader.group(1) if reader else None), definition_body(text, name)


def part_leaves(text, name, vectors, problems):
    """The parts one reader fills, and the leaves it reads into them.

    A reader declares the part it fills and assigns its columns from the reader
    it is handed. A reader may hand its reader on to another, as the optional
    convention does to the convention, so the calls are followed.
    """
    reader, body = part_reader(text, name)
    if body is None:
        problems.append(f"the projection has no part reader {name}")
        return [], []
    parts, leaves = [], []
    for entity, variable in re.findall(r"refdata::domain::(\w+) (\w+);", body):
        if entity not in vectors:
            problems.append(f"the part {entity} that {name} fills has no vector")
            continue
        parts.append(entity)
        for assigned, column, expression in rows_of(body):
            ore = kind_leaf(expression, reader) if assigned == variable else None
            if ore is not None:
                leaves.append({"column": column, "ore": ore, "entity": entity,
                               "vector": vectors[entity]})
    for called, _ in PART_CALL.findall(body):
        more_parts, more_leaves = part_leaves(text, called, vectors, problems)
        parts += more_parts
        leaves += more_leaves
    return parts, leaves


# One branch of the default curve dispatch: the types it tests for, and the
# parts it returns.
DEFAULT_BRANCH = re.compile(r"(?:if|else if) \(([^{]*?)\) \{((?:[^{}]|\{[^{}]*\})*)\}", re.S)


def default_curve_types(text):
    """The default curve types that have each part, by the part's entity.

    =parts_of_default_curve_type= is the projection's dispatch on a default
    curve configuration's type, as ORE's reader dispatches on it: each branch
    names its types and sets the parts those types have, each named for its
    entity without the default_curve_ prefix.
    """
    body = definition_body(text, "parts_of_default_curve_type")
    if body is None:
        return {}, ["the projection has no parts_of_default_curve_type"]
    vectors = part_vectors(text)
    problems = []
    by_part = {}
    for condition, inner in DEFAULT_BRANCH.findall(body):
        types = re.findall(r'type == "(\w+)"', condition)
        for name in re.findall(r"\.(\w+) = true", inner):
            part = "default_curve_" + name
            if part not in vectors:
                problems.append(f"the default curve part {part} has no vector")
                continue
            seen = by_part.setdefault(part, [])
            seen.extend(t for t in types if t not in seen)
    if not by_part:
        problems.append("the projection's default curve dispatch fills no parts")
    return by_part, problems


def kind_leaves(text):
    """The parts each segment kind fills and the leaves it reads, in order.

    The segment row holds what every kind shares, and each kind's settings are
    parts of the segment: what a kind reads is the projection's dispatch, a loop
    per element that calls one reader per part the element has. =Simple= reads
    its convention, its pillar rules and its projection curve; =Direct= reads
    its convention and its pillar rules. The screen must not offer a leaf that
    belongs to another kind, and the database must not hold a part the kind does
    not have, so both are read from here.
    """
    body = definition_body(text, "import_segments")
    if body is None:
        return [], ["the projection has no import_segments"]
    vectors = part_vectors(text)
    if not vectors:
        return [], ["the projection names no segment part vectors"]
    kinds = []
    problems = []
    for loop in KIND_LOOP.finditer(body):
        element, inner = loop.group(2, 3)
        parts, leaves = [], []
        for called, _ in PART_CALL.findall(inner):
            more_parts, more_leaves = part_leaves(text, called, vectors, problems)
            parts += more_parts
            leaves += more_leaves
        if not leaves:
            problems.append(f"the projection reads no leaves for the kind {element}")
        kinds.append({"kind": element, "parts": parts, "leaves": leaves})
    if not kinds:
        problems.append("the projection reads no segment kinds")
    return kinds, problems


# The call a section makes when its entries hold segments: the block's name, and
# the reader that fills the rows.
SEGMENT_IMPORT = re.compile(
    r'const auto (\w+) = v\.child\("(\w+)"\); \1\)\s*\n\s*import_segments\(ctx, \*\1\);')


def complex_type(text, name):
    """The body of one complex type, or None when the schema does not declare it."""
    match = re.search(r'<xs:complexType name="' + name + r'">(.*?)\n  </xs:complexType>',
                      text, re.S)
    return match.group(1) if match is not None else None


def children_of(text, name):
    """The elements a complex type declares as its direct children, in order.

    A child may be declared with a type of its own or inline, so the walk counts
    the nesting of =xs:element= rather than reading a type attribute: only the
    elements at the body's own depth are children, and an inline declaration
    does not hide its siblings.
    """
    body = complex_type(text, name)
    if body is None:
        return None
    children = []
    depth = 0
    for match in re.finditer(r"<xs:element\b([^>]*?)(/?)>|</xs:element>", body):
        if match.group(0).startswith("</"):
            depth -= 1
            continue
        attributes = match.group(1)
        if depth == 0:
            found = re.search(r'\bname="(\w+)"', attributes)
            if found is not None:
                children.append(found.group(1))
        if not match.group(2):
            depth += 1
    return children


def leaves_of_schema():
    """Each section's entry element and the leaves the schema lets an entry hold.

    The schema wraps a section in a type that declares the entry element, and the
    entry's own type declares its leaves. Both steps are needed because the two
    names differ: =fxVolatilities= holds =FXVolatility=, whose type is
    =fxVolatility=. A wrapper that declares more than one element is not a
    section of entries -- =ReportConfiguration= is the document's global report
    settings -- and is left out.
    """
    with open(SCHEMA, encoding="utf-8") as handle:
        text = handle.read()
    root = complex_type(text, "curveconfiguration")
    if root is None:
        raise SystemExit(f"{SCHEMA} declares no curveconfiguration type")
    declared = {}
    for section_type, section in re.findall(
            r'<xs:element type="(\w+)" name="(\w+)"', root):
        entries = complex_type(text, section_type)
        if entries is None:
            raise SystemExit(f"{SCHEMA} declares no {section_type} type")
        declared_entries = re.findall(
            r'<xs:element type="(\w+)" name="(\w+)"', entries)
        if len(declared_entries) != 1:
            continue
        entry_type, entry = declared_entries[0]
        leaves = children_of(text, entry_type)
        if leaves is None:
            raise SystemExit(f"{SCHEMA} declares no {entry_type} type for {entry}")
        declared[section] = {"entry": entry, "leaves": leaves}
    return declared


def section_wiring(text, declared, section, entry, import_name, segment_rules):
    body = function_body(text, import_name)
    if body is None:
        return None, [f"{section}: the import function {import_name} was not found"]
    problems = []
    entity_of = entities_of(body)
    # A section whose entries the document stores as nothing but a definition --
    # =FXSpots=, whose fields live in the market data service -- has no family
    # table, and its entry is the definition row alone.
    stored = stored_rows(body)
    vectors_of = {}
    for vector, variable in stored:
        entity = entity_of.get(variable)
        if entity is None:
            problems.append(f"{section}: {variable} is not a declared row of the function")
            continue
        vectors_of[entity] = vector
    leaves = definition_leaves(body)
    if len(leaves) != 2:
        problems.append(f"{section}: the entry's identifier and description were not read")
    parameters = [{
        "entity": DEFINITION_ENTITY,
        "column": "curve_id",
        "ore": leaves[0] if leaves else "CurveId",
    }, {
        "entity": DEFINITION_ENTITY,
        "column": "description",
        "ore": leaves[1] if len(leaves) > 1 else "CurveDescription",
    }]
    blocks = blocks_of(body)
    presence = {}
    for variable, column, expression in rows_of(body):
        reader, element = read_element(expression)
        if reader != "v":
            continue
        entity = entity_of.get(variable)
        if entity is None:
            continue
        if element in blocks and column.startswith("has_"):
            presence[element] = column
            continue
        parameters.append({"entity": entity, "column": column, "ore": element})
    # An entry's construction settings are parts of the entry, read by a reader
    # per part and keyed by the definition as the entry's own row is.
    vectors = part_vectors(text)
    for called in dict.fromkeys(ENTRY_PART_CALL.findall(body)):
        parts, part_leaves_read = part_leaves(text, called, vectors, problems)
        for part in parts:
            vectors_of.setdefault(part, vectors[part])
        for leaf in part_leaves_read:
            if leaf["ore"] in blocks and leaf["column"].startswith("has_"):
                presence[leaf["ore"]] = leaf["column"]
                continue
            parameters.append(
                {"entity": leaf["entity"], "column": leaf["column"], "ore": leaf["ore"]})
    # The schema is the authority on the leaves an entry holds, so every column
    # the projection reads is checked against it rather than against the prose.
    allowed = set(declared.get(section, {}).get("leaves", []))
    schema_entry = declared.get(section, {}).get("entry")
    if schema_entry is not None and schema_entry != entry:
        problems.append(
            f"{section}: the projection writes entries as {entry}, the schema as {schema_entry}")
    for parameter in parameters:
        parameter["xpath"] = "/".join(
            [ROOT_ELEMENT, section, entry, parameter["ore"]])
        if parameter["ore"] not in allowed:
            problems.append(
                f"{section}: the schema declares no {parameter['ore']} under {entry}")
    collections = []
    segments = SEGMENT_IMPORT.search(body)
    if segments is not None:
        for rule in segment_rules:
            collections.append({"element": segments.group(2), **rule})
    elif "Segments" in blocks:
        # A section that holds segments and whose reader the pattern does not see
        # would reach the screen as a block with no rows, and nothing would say why.
        problems.append(f"{section}: holds a Segments block the collection rule does not read")
    block_rows = [{"element": element, "presence": presence.get(element, ""),
                   "presenceEntity": "" if element in presence
                   else block_entity(body, element, entity_of, text)}
                  for element in blocks]
    # The entity that states a block is one of the entry's tables, so the screen
    # reads its rows: a part read from the block's element joins them here.
    for block in block_rows:
        entity = block["presenceEntity"]
        if entity and entity not in vectors_of and entity in vectors:
            vectors_of[entity] = vectors[entity]
    return {
        "section": section,
        "entryElement": entry,
        "definitionKey": DEFINITION_KEY,
        "entities": [{"entity": entity, "vector": vector}
                     for entity, vector in vectors_of.items()],
        "parameters": parameters,
        "blocks": block_rows,
        "collections": collections,
    }, problems


def wiring(text, declared):
    imports = entry_imports(text)
    sections = []
    problems = []
    segment_rules, rule_problems = collection_rules(text)
    problems += rule_problems
    kinds, kind_problems = kind_leaves(text)
    problems += kind_problems
    for rule in segment_rules:
        rule["kinds"] = kinds
    for section, entry in declared_sections(text):
        import_name = imports.get(section)
        if import_name is None:
            problems.append(f"{section}: the document mapper does not import the section")
            continue
        found, section_problems = section_wiring(
            text, declared, section, entry, import_name, segment_rules)
        problems += section_problems
        if found is not None:
            sections.append(found)
    if list(declared) != [section for section, _ in declared_sections(text)]:
        problems.append("the projection's sections and the schema's are not the same list,"
                        " in the same order")
    return sections, problems


HEADER = """
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 *
 * Generated by projects/ores.sql/utility/generate_curve_section_parameters.py
 * from the curve configuration projection and the ORE schema. To modify,
 * change the projection or the schema and run the generator.
 */

/**
 * One column of a curve entry's table, and the leaf of the ORE document that
 * holds the value it carries.
 */
export interface CurveParameter {
    /** The table that holds the column, named as the refdata entity is. */
    readonly entity: string;
    /** The column of that table. */
    readonly column: string;
    /** The ORE element the column reads, which is the leaf's own name. */
    readonly ore: string;
    /** The XPath the ORE parameter contract keys the leaf by. */
    readonly xpath: string;
}

/** A table an entry is stored in, and the document vector that carries its rows. */
export interface CurveEntity {
    readonly entity: string;
    readonly vector: string;
}

/**
 * A child element an entry holds, and how the entry records its presence: a
 * column of the entry's rows, or the rows of an entity the element's
 * construction writes.
 */
export interface CurveBlock {
    readonly element: string;
    /** The column that records whether the entry holds the element, and empty for none. */
    readonly presence: string;
    /** The entity whose row states the element, and empty for none. */
    readonly presenceEntity: string;
}

/** One kind of a collection row, and the leaves the projection reads for it. */
export interface CurveCollectionKind {
    /** The element the row is written as, such as =Simple=. */
    readonly kind: string;
    readonly leaves: readonly CurveCollectionLeaf[];
}

/**
 * One leaf of a kind: the ORE element it is, and the part of the row that holds
 * it. A row's kinds share no settings column, so each leaf is in a part of the
 * row, joined to it by the collection's part key.
 */
export interface CurveCollectionLeaf {
    readonly column: string;
    readonly ore: string;
    /** The part table that holds the leaf. */
    readonly entity: string;
    /** The document vector that carries the part's rows. */
    readonly vector: string;
}

/** A child element an entry holds rows of, and the columns that place its leaves. */
export interface CurveCollection {
    /** The element the rows are written as, such as =Segments=. */
    readonly element: string;
    /** The table that holds a row. */
    readonly entity: string;
    /** The document vector that carries the rows. */
    readonly vector: string;
    /** The column that joins a row back to the entry it belongs to. */
    readonly parentKey: string;
    /** The column that names the element a row is written as, or empty for none. */
    readonly kindColumn: string;
    /** The column a quote of one row joins back to, or empty for no quotes. */
    readonly quoteParent: string;
    /** The column a part of one row joins back to. */
    readonly partParent: string;
    /** The kinds a row may state, and the leaves each kind reads. */
    readonly kinds: readonly CurveCollectionKind[];
}

/** One section of the document, with the columns its entries' leaves are read from. */
export interface CurveSectionWiring {
    readonly section: string;
    /** The element one entry of the section is written as. */
    readonly entryElement: string;
    /** The column every entry's table is keyed back to its definition by. */
    readonly definitionKey: string;
    readonly entities: readonly CurveEntity[];
    readonly parameters: readonly CurveParameter[];
    readonly blocks: readonly CurveBlock[];
    readonly collections: readonly CurveCollection[];
}

/**
 * The sections of an ORE curve configuration, in the order the schema declares
 * them, each with the columns its entries' leaves are read from. The order is
 * the one the ORE XSD declares, which the projection's section table states.
 */
export const CURVE_SECTIONS: readonly CurveSectionWiring[] = [
"""


def quote(value):
    return "'" + value.replace("\\", "\\\\").replace("'", "\\'") + "'"


def render_parameters(parameters):
    lines = []
    for parameter in parameters:
        lines.append(
            "            {\n"
            f"                entity: {quote(parameter['entity'])},\n"
            f"                column: {quote(parameter['column'])},\n"
            f"                ore: {quote(parameter['ore'])},\n"
            f"                xpath: {quote(parameter['xpath'])},\n"
            "            },")
    return "\n".join(lines)


def render_list(name, rows, render_one):
    """One array property, empty on a single line as the house style writes it."""
    if not rows:
        return f"        {name}: [],\n"
    return f"        {name}: [\n{render_one(rows)}\n        ],\n"


def render_entities(entities):
    lines = []
    for row in entities:
        lines.append(
            "            {\n"
            f"                entity: {quote(row['entity'])},\n"
            f"                vector: {quote(row['vector'])},\n"
            "            },")
    return "\n".join(lines)


def render_blocks(blocks):
    lines = []
    for block in blocks:
        lines.append(
            "            {\n"
            f"                element: {quote(block['element'])},\n"
            f"                presence: {quote(block['presence'])},\n"
            f"                presenceEntity: {quote(block['presenceEntity'])},\n"
            "            },")
    return "\n".join(lines)


def render_kind_leaves(leaves):
    return "\n".join(
        "                            {\n"
        f"                                column: {quote(leaf['column'])},\n"
        f"                                ore: {quote(leaf['ore'])},\n"
        f"                                entity: {quote(leaf['entity'])},\n"
        f"                                vector: {quote(leaf['vector'])},\n"
        "                            }," for leaf in leaves)


def render_kinds(kinds):
    return "\n".join(
        "                    {\n"
        f"                        kind: {quote(kind['kind'])},\n"
        f"                        leaves: [\n{render_kind_leaves(kind['leaves'])}\n"
        "                        ],\n"
        "                    }," for kind in kinds)


def render_collections(collections):
    lines = []
    for collection in collections:
        lines.append(
            "            {\n"
            f"                element: {quote(collection['element'])},\n"
            f"                entity: {quote(collection['entity'])},\n"
            f"                vector: {quote(collection['vector'])},\n"
            f"                parentKey: {quote(collection['parentKey'])},\n"
            f"                kindColumn: {quote(collection['kindColumn'])},\n"
            f"                quoteParent: {quote(collection['quoteParent'])},\n"
            f"                partParent: {quote(collection['partParent'])},\n"
            f"                kinds: [\n{render_kinds(collection['kinds'])}\n"
            "                ],\n"
            "            },")
    return "\n".join(lines)


def render(sections):
    body = []
    for section in sections:
        body.append(
            "    {\n"
            f"        section: {quote(section['section'])},\n"
            f"        entryElement: {quote(section['entryElement'])},\n"
            f"        definitionKey: {quote(section['definitionKey'])},\n"
            f"{render_list('entities', section['entities'], render_entities)}"
            f"{render_list('parameters', section['parameters'], render_parameters)}"
            f"{render_list('blocks', section['blocks'], render_blocks)}"
            f"{render_list('collections', section['collections'], render_collections)}"
            "    },")
    return LICENCE + HEADER + "\n".join(body) + FOOTER


CPP_OUT = os.path.join(
    ROOT, "projects", "ores.http", "refdata", "include", "ores.http", "routes",
    "refdata", "curve_section_wiring.hpp")
CPP_SRC_OUT = os.path.join(
    ROOT, "projects", "ores.http", "refdata", "src", "routes", "refdata",
    "curve_section_wiring.cpp")

CPP_LICENCE = """\
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
 */
"""

CPP_HEADER = """\
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 *
 * Generated by projects/ores.sql/utility/generate_curve_section_parameters.py
 * from the curve configuration projection and the ORE schema. To modify,
 * change the projection or the schema and run the generator.
 */
#ifndef ORES_HTTP_ROUTES_REFDATA_CURVE_SECTION_WIRING_HPP
#define ORES_HTTP_ROUTES_REFDATA_CURVE_SECTION_WIRING_HPP

#include <span>
#include <string_view>

namespace ores::http::routes::refdata::wiring {

/**
 * @brief One column of a curve entry's table, and the ORE leaf that holds the
 * value it carries.
 */
struct curve_parameter final {
    /// The table that holds the column, named as the refdata entity is.
    std::string_view entity;
    /// The column of that table.
    std::string_view column;
    /// The ORE element the column reads, which is the leaf's own name.
    std::string_view ore;
    /// The XPath the ORE parameter contract keys the leaf by.
    std::string_view xpath;
};

/// A table an entry is stored in, and the document vector that carries its rows.
struct curve_entity final {
    std::string_view entity;
    std::string_view vector;
};

/**
 * @brief A child element an entry holds, and how the entry records its presence:
 * a column of the entry's rows, or the rows of an entity the element's
 * construction writes.
 */
struct curve_block final {
    std::string_view element;
    /// The column that records whether the entry holds the element, empty for none.
    std::string_view presence;
    /// The entity whose row states the element, empty for none.
    std::string_view presence_entity;
};

/**
 * @brief One leaf of a kind: the ORE element it is, and the part of the row that
 * holds it.
 *
 * A row's kinds share no settings column, so each leaf is in a part of the row,
 * joined to it by the collection's part key.
 */
struct curve_collection_leaf final {
    std::string_view column;
    std::string_view ore;
    /// The part table that holds the leaf.
    std::string_view entity;
    /// The document vector that carries the part's rows.
    std::string_view vector;
};

/// One kind of a collection row, and the leaves the projection reads for it.
struct curve_collection_kind final {
    /// The element the row is written as, such as Simple.
    std::string_view kind;
    std::span<const curve_collection_leaf> leaves;
};

/// A child element an entry holds rows of, and the columns that place its leaves.
struct curve_collection final {
    /// The element the rows are written as, such as Segments.
    std::string_view element;
    /// The table that holds a row.
    std::string_view entity;
    /// The document vector that carries the rows.
    std::string_view vector;
    /// The column that joins a row back to the entry it belongs to.
    std::string_view parent_key;
    /// The column that names the element a row is written as, empty for none.
    std::string_view kind_column;
    /// The column a quote of one row joins back to, empty for no quotes.
    std::string_view quote_parent;
    /// The column a part of one row joins back to.
    std::string_view part_parent;
    /// The kinds a row may state, and the leaves each kind reads.
    std::span<const curve_collection_kind> kinds;
};

/// One section of the document, with the columns its entries' leaves are read from.
struct curve_section final {
    std::string_view section;
    /// The element one entry of the section is written as.
    std::string_view entry_element;
    /// The column every entry's table is keyed back to its definition by.
    std::string_view definition_key;
    std::span<const curve_entity> entities;
    std::span<const curve_parameter> parameters;
    std::span<const curve_block> blocks;
    std::span<const curve_collection> collections;
};

/**
 * @brief The sections of an ORE curve configuration, in the order the schema
 * declares them.
 */
[[nodiscard]] std::span<const curve_section> sections();

/// One section's wiring, or nothing for a section the table does not name.
[[nodiscard]] const curve_section* section_of(std::string_view code);

}

#endif
"""

CPP_FOOTER = """\
std::span<const curve_section> sections() {
    return curve_section_table;
}

const curve_section* section_of(std::string_view code) {
    for (const auto& section : curve_section_table)
        if (section.section == code)
            return &section;
    return nullptr;
}

}
"""


def cpp_quote(value):
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def cpp_array(kind, name, rows, render_row):
    """One table, and nothing at all for a table with no rows."""
    if not rows:
        return f"constexpr std::array<{kind}, 0> {name}{{}};\n"
    lines = "".join(f"    {render_row(row)},\n" for row in rows)
    return f"constexpr std::array<{kind}, {len(rows)}> {name}{{{{\n{lines}}}}};\n"


def cpp_parameter(parameter):
    return "{" + ", ".join(cpp_quote(parameter[key])
                           for key in ("entity", "column", "ore", "xpath")) + "}"


def cpp_entity(entity):
    return "{" + ", ".join(cpp_quote(entity[key])
                           for key in ("entity", "vector")) + "}"


def cpp_block(block):
    return "{" + ", ".join(cpp_quote(block[key])
                           for key in ("element", "presence", "presenceEntity")) + "}"


def cpp_section_tables(index, section):
    """The arrays one section's wiring reads, and the section itself after them."""
    body = []
    body.append(cpp_array("curve_entity", f"curve_entities_{index}",
                          section["entities"], cpp_entity))
    body.append(cpp_array("curve_parameter", f"curve_parameters_{index}",
                          section["parameters"], cpp_parameter))
    body.append(cpp_array("curve_block", f"curve_blocks_{index}",
                          section["blocks"], cpp_block))
    kinds = []
    collections = []
    for position, collection in enumerate(section["collections"]):
        for kind_index, kind in enumerate(collection["kinds"]):
            kinds.append(cpp_array(
                "curve_collection_leaf",
                f"curve_leaves_{index}_{position}_{kind_index}", kind["leaves"],
                lambda leaf: "{" + ", ".join(
                    cpp_quote(leaf[key]) for key in ("column", "ore", "entity", "vector")) + "}"))
        kind_rows = "".join(
            "    {" + cpp_quote(kind["kind"]) + f", curve_leaves_{index}_{position}_{kind_index}}},\n"
            for kind_index, kind in enumerate(collection["kinds"]))
        kinds.append(
            f"constexpr std::array<curve_collection_kind, {len(collection['kinds'])}>"
            f" curve_kinds_{index}_{position}{{{{\n{kind_rows}}}}};\n")
        collections.append(
            "    {" + ", ".join(
                [cpp_quote(collection[key])
                 for key in ("element", "entity", "vector", "parentKey",
                             "kindColumn", "quoteParent", "partParent")])
            + f", curve_kinds_{index}_{position}}}")
    body.extend(kinds)
    if collections:
        rows = "".join(f"{row},\n" for row in collections)
        body.append(
            f"constexpr std::array<curve_collection, {len(collections)}>"
            f" curve_collections_{index}{{{{\n{rows}}}}};\n")
    else:
        body.append(
            f"constexpr std::array<curve_collection, 0> curve_collections_{index}{{}};\n")
    body.append(
        "    {" + ", ".join(
            [cpp_quote(section["section"]), cpp_quote(section["entryElement"]),
             cpp_quote(section["definitionKey"]),
             f"curve_entities_{index}", f"curve_parameters_{index}", f"curve_blocks_{index}",
             f"curve_collections_{index}"]) + "}")
    return body


def render_cpp(sections):
    tables = []
    rows = []
    for index, section in enumerate(sections):
        body = cpp_section_tables(index, section)
        tables.extend(body[:-1])
        rows.append(body[-1])
    table_rows = "".join(f"{row},\n" for row in rows)
    return (CPP_LICENCE + "#include \"ores.http/routes/refdata/curve_section_wiring.hpp\"\n"
            "#include <array>\n"
            "#include <span>\n"
            "#include <string_view>\n\n"
            "namespace ores::http::routes::refdata::wiring {\n\n"
            "namespace {\n\n"
            + "\n".join(tables) + "\n"
            f"constexpr std::array<curve_section, {len(sections)}> curve_section_table{{{{\n"
            + table_rows + "}};\n\n}\n\n" + CPP_FOOTER)


FOOTER = """
];

/** One section's wiring, by the section's element name, or undefined for none. */
export function curveSectionOf(section: string): CurveSectionWiring | undefined {
    return CURVE_SECTIONS.find((candidate) => candidate.section === section);
}
"""


SQL_LICENCE = """\
/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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
 */
"""


def part_kinds(kinds):
    """The kinds that have each part, by the part's entity, in first-seen order."""
    by_part = {}
    for kind in kinds:
        for part in kind["parts"]:
            seen = by_part.setdefault(part, [])
            if kind["kind"] not in seen:
                seen.append(kind["kind"])
    return by_part


def render_sql(kinds, default_types):
    """The functions that refuse a part its owner's construction does not have."""
    lines = [SQL_LICENCE, """
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 *
 * Generated by projects/ores.sql/utility/generate_curve_section_parameters.py
 * from the curve configuration projection. To modify, change the projection's
 * import_segments or parts_of_default_curve_type and run the generator.
 *
 * A curve segment's settings are parts of the segment, and each segment
 * element has only some of them; a default curve configuration's settings are
 * parts of the configuration, and each type has only some of them. The
 * projection's import_segments and parts_of_default_curve_type are the one
 * statement of which construction fills which part, so these functions are
 * written from them. Each part's insert trigger calls its function on the row
 * the part names.
 */

create or replace function ores_refdata_validate_curve_segment_part_fn(
    p_tenant_id uuid,
    p_segment_id uuid,
    p_part text,
    p_kinds text[]
) returns uuid as $$
declare
    v_kind text;
begin
    select t.segment_kind into v_kind
    from ores_refdata_curve_segments_tbl s
    join ores_refdata_curve_segment_types_tbl t
      on t.tenant_id = s.tenant_id
     and t.code = s.segment_type
     and t.valid_to = ores_utility_infinity_timestamp_fn()
    where s.tenant_id = p_tenant_id
      and s.id = p_segment_id
      and s.valid_to = ores_utility_infinity_timestamp_fn();

    -- The trigger's key check refuses a segment that is not there.
    if v_kind is not null and not (v_kind = any(p_kinds)) then
        raise exception 'Invalid curve_segment_id: %. A % segment has no % part.',
            p_segment_id, v_kind, p_part
            using errcode = '23514';
    end if;
    return p_segment_id;
end;
$$ language plpgsql stable;
"""]
    lines.append("""
create or replace function ores_refdata_validate_default_curve_part_fn(
    p_tenant_id uuid,
    p_configuration_id uuid,
    p_part text,
    p_types text[]
) returns uuid as $$
declare
    v_type text;
begin
    select c.default_curve_type into v_type
    from ores_refdata_default_curve_configurations_tbl c
    where c.tenant_id = p_tenant_id
      and c.id = p_configuration_id
      and c.valid_to = ores_utility_infinity_timestamp_fn();

    -- The trigger's key check refuses a configuration that is not there.
    if v_type is not null and not (v_type = any(p_types)) then
        raise exception 'Invalid default_curve_configuration_id: %. A % default curve has no % part.',
            p_configuration_id, v_type, p_part
            using errcode = '23514';
    end if;
    return p_configuration_id;
end;
$$ language plpgsql stable;
""")
    for part, types in default_types.items():
        types_sql = textwrap.fill(", ".join(f"'{k}'" for k in types), width=80,
                                  initial_indent=" " * 12, subsequent_indent=" " * 12,
                                  break_on_hyphens=False)
        lines.append(f"""
create or replace function ores_refdata_validate_{part}_fn(
    p_tenant_id uuid,
    p_value uuid
) returns uuid as $$
    select ores_refdata_validate_default_curve_part_fn(
        p_tenant_id, p_value, '{part}',
        array[
{types_sql}
        ]);
$$ language sql stable;
""")
    for part, part_kinds_list in part_kinds(kinds).items():
        kinds_sql = textwrap.fill(", ".join(f"'{k}'" for k in part_kinds_list), width=80,
                                  initial_indent=" " * 12, subsequent_indent=" " * 12,
                                  break_on_hyphens=False)
        lines.append(f"""
create or replace function ores_refdata_validate_{part}_fn(
    p_tenant_id uuid,
    p_value uuid
) returns uuid as $$
    select ores_refdata_validate_curve_segment_part_fn(
        p_tenant_id, p_value, '{part}',
        array[
{kinds_sql}
        ]);
$$ language sql stable;
""")
    return "".join(lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed module differs from the projection")
    args = parser.parse_args()
    sections, problems = wiring(source(), leaves_of_schema())
    if problems:
        print("\n".join(problems), file=sys.stderr)
        return 1
    content = render(sections)
    cpp_header = CPP_LICENCE + CPP_HEADER
    cpp_source = render_cpp(sections)
    kinds, _ = kind_leaves(source())
    default_types, default_problems = default_curve_types(source())
    if default_problems:
        print("\n".join(default_problems), file=sys.stderr)
        return 1
    sql = render_sql(kinds, default_types)
    if args.check:
        for path, expected in ((OUT, content), (SQL_OUT, sql),
                               (CPP_OUT, cpp_header), (CPP_SRC_OUT, cpp_source)):
            committed = (open(path, encoding="utf-8").read()
                         if os.path.exists(path) else "")
            if committed != expected:
                print(f"{path} differs from the projection; regenerate it.",
                      file=sys.stderr)
                return 1
        print(f"{len(sections)} sections of {sum(len(s['parameters']) for s in sections)}"
              " parameters match the module.")
        return 0
    with open(OUT, "w", encoding="utf-8") as handle:
        handle.write(content)
    with open(SQL_OUT, "w", encoding="utf-8") as handle:
        handle.write(sql)
    # The gateway renders the same projection for the routes the BFF answers
    # today, so the two sides read one table.
    for path, expected in ((CPP_OUT, cpp_header), (CPP_SRC_OUT, cpp_source)):
        with open(path, "w", encoding="utf-8") as handle:
            handle.write(expected)
    total = sum(len(s["parameters"]) for s in sections)
    blocks = sum(len(s["blocks"]) for s in sections)
    print(f"Wrote {OUT} and the gateway's wiring: {len(sections)} sections,"
          f" {total} parameters, {blocks} blocks.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
