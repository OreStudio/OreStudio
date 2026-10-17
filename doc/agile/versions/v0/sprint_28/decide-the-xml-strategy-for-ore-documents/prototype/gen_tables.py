#!/usr/bin/env python3
"""Emit the codec descriptor tables for one XSD complex type.

The generator is the lever. It reads the authoritative schema, so the table
cannot disagree with ORE about which elements exist, their order, or their
type. The only thing the schema does not say is the C++ member name, which is
snake-cased from the element name.
"""

import argparse
import re
import sys
import xml.etree.ElementTree as ET

XSD_NS = "{http://www.w3.org/2001/XMLSchema}"

BASE_KINDS = {
    "xs:string": "text",
    "xs:integer": "integer",
    "xs:int": "integer",
    "xs:long": "integer",
    "xs:decimal": "decimal_text",
    "xs:double": "decimal_text",
    "xs:float": "decimal_text",
    "xs:date": "text",
    "xs:time": "text",
    "xs:dateTime": "text",
    "xs:boolean": "text",
}


def local(tag):
    return tag.split("}")[-1] if "}" in tag else tag


def snake(name):
    # Split on lower-to-upper and at the end of a capital run, so ISOCode
    # becomes iso_code rather than isocode.
    step = re.sub(r"(?<=[A-Z])([A-Z])(?=[a-z])", r"_\1", name)
    step = re.sub(r"(?<=[a-z0-9])([A-Z])", r"_\1", step)
    return step.lower()


def parse_schemas(paths):
    return [ET.parse(path).getroot() for path in paths]


def complex_types(roots):
    found = {}
    for root in roots:
        for element in root.iter():
            if local(element.tag) == "complexType" and element.get("name"):
                found[element.get("name")] = element
    return found


def enumerations(roots):
    found = {}
    for root in roots:
        for element in root.iter():
            if local(element.tag) != "simpleType" or not element.get("name"):
                continue
            values = [
                enumeration.get("value")
                for restriction in element
                if local(restriction.tag) == "restriction"
                for enumeration in restriction
                if local(enumeration.tag) == "enumeration"
            ]
            if values:
                found[element.get("name")] = values
    return found


def elements_of(definition):
    for container in definition:
        if local(container.tag) not in ("sequence", "all", "choice"):
            continue
        for element in container:
            if local(element.tag) == "element":
                yield element


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--schema", action="append", required=True)
    parser.add_argument("--type", required=True, help="complexType of one record")
    parser.add_argument("--root", required=True, help="document element name")
    parser.add_argument("--item", required=True, help="element name of one record")
    parser.add_argument("--record", required=True, help="C++ record base name")
    parser.add_argument("--header", required=True)
    args = parser.parse_args()

    roots = parse_schemas(args.schema)
    definition = complex_types(roots)[args.type]
    enums = enumerations(roots)
    fields = list(elements_of(definition))

    out = ["// Generated from the ORE schema. Do not edit by hand.\n"]
    out.append(f'#include "{args.header}"\n')
    out.append("\nnamespace ore::xml {\nnamespace {\n\n")

    declared_types = {field.get("type") for field in fields}
    for enum_type in sorted(set(declared_types) & set(enums)):
        spellings = ", ".join(f'"{value}"' for value in enums[enum_type])
        out.append(f"const char* {snake(enum_type)}_spellings[] = {{{spellings}, nullptr}};\n")
    if set(declared_types) & set(enums):
        out.append("\n")

    for layout in ("row", "view"):
        out.append(f"const field {args.record}_{layout}_fields[] = {{\n")
        for index, element in enumerate(fields):
            name = element.get("name")
            declared = element.get("type")
            if declared in enums:
                kind, tokens = "enumeration", f"{snake(declared)}_spellings"
            elif declared in BASE_KINDS:
                kind, tokens = BASE_KINDS[declared], "nullptr"
            else:
                raise SystemExit(f"unsupported element type {declared} for {name}")
            out.append(
                f'    {{"{name}", detail::hash_name("{name}"), value_kind::{kind}, '
                f"offsetof({args.record}_{layout}, {snake(name)}), {index}, {tokens}}},\n"
            )
        out.append("};\n\n")

    for layout in ("row", "view"):
        out.append(
            f'const record_type {layout}_record = {{"{args.type}", '
            f"{args.record}_{layout}_fields, {len(fields)}}};\n"
        )
        out.append(
            f'const document_type {layout}_document = {{"{args.root}", '
            f'"{args.item}", &{layout}_record}};\n'
        )
    out.append("\n} // namespace\n\n")
    out.append(
        "auto currency_document() -> const document_type& {\n    return row_document;\n}\n\n"
    )
    out.append(
        "auto currency_view_document() -> const document_type& {\n"
        "    return view_document;\n}\n\n"
    )
    out.append(
        "auto write_currencies(const std::vector<currency_row>& rows, std::string& out) -> void {\n"
        "    detail::write_items<detail::materialized>(rows, row_document, out);\n}\n\n"
    )
    out.append("} // namespace ore::xml\n")
    sys.stdout.write("".join(out))


if __name__ == "__main__":
    main()
