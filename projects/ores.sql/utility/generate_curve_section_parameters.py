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
    "refdata_curve_segment_parts_validate_fn_create.sql")
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
    keep = definition_body(text, "keep")
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
    part_parent = re.search(r"\bp\.(\w+)\s*=\s*segment_id;", keep or "")
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
PART_VECTOR = re.compile(
    r"struct segment_part<refdata::domain::(\w+)> \{\s*"
    r"static constexpr auto vector\s*=\s*&\w+::(\w+);")

# A call to one of the readers that fill a segment's parts, and the reader it
# hands on.
PART_CALL = re.compile(r"\b(import_\w+)\(ctx, \w+, (\w+)[,)]")


def part_vectors(text):
    """The document vector of every segment part, by the part's entity."""
    return dict(PART_VECTOR.findall(text))


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
    return {
        "section": section,
        "entryElement": entry,
        "definitionKey": DEFINITION_KEY,
        "entities": [{"entity": entity, "vector": vector}
                     for entity, vector in vectors_of.items()],
        "parameters": parameters,
        "blocks": [{"element": element, "presence": presence.get(element, "")}
                   for element in blocks],
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

/** A child element an entry holds, and the column that records its presence. */
export interface CurveBlock {
    readonly element: string;
    /** The column that records whether the entry holds the element, and empty for none. */
    readonly presence: string;
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


def render_sql(kinds):
    """The functions that refuse a segment part its segment's kind does not have."""
    lines = [SQL_LICENCE, """
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 *
 * Generated by projects/ores.sql/utility/generate_curve_section_parameters.py
 * from the curve configuration projection. To modify, change the projection's
 * import_segments and run the generator.
 *
 * A curve segment's settings are parts of the segment, and each segment
 * element has only some of them. The projection's import_segments is the one
 * statement of which element fills which part, so these functions are written
 * from it. Each part's insert trigger calls its function on the segment the
 * row names.
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
    kinds, _ = kind_leaves(source())
    sql = render_sql(kinds)
    if args.check:
        for path, expected in ((OUT, content), (SQL_OUT, sql)):
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
    total = sum(len(s["parameters"]) for s in sections)
    blocks = sum(len(s["blocks"]) for s in sections)
    print(f"Wrote {OUT}: {len(sections)} sections, {total} parameters, {blocks} blocks.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
