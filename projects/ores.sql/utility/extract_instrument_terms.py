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
"""Extract the fields of every trade's element from the vendored ORE schemas and guide.

The schema is the structure. The trade type's data elements are the members of the
oreTradeData group of input.xsd, and each names the complex type that holds its fields.
A field is a leaf, whose value has a simple type, or a nested record, whose value is
another complex type; both state whether the document must carry them and how often.
The schemas are the ones vendored under external/ore/xsd, which are the release the
store records in ore_file_version, so the structure is the release's own.

The guide is the prose. Each trade data and trade component section lists the elements
of a node as "Name [Optional]: text", and that text is the description of the field of
that name in the types the node is declared with.

The element each trade type is read under is the type's name followed by Data, except
where instrument_elements.json, beside this file, states otherwise with its evidence.

The result, instrument_terms.json beside this file, is derived: run this script again
when ORE changes, and read the counts it prints.

    extract_instrument_terms.py [--ore /path/to/ORE/Engine]

The ORE checkout, for the guide, comes from --ore, else from the environment variable
ORES_ORE_ENGINE, else from the default the other extractors use.
"""

import argparse
import glob
import json
import os
import re
import xml.etree.ElementTree as ET

from extract_convention_terms import ITEM, clean

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
XSD = os.path.join(ROOT, "external", "ore", "xsd")
CATALOGUE = os.path.join(ROOT, "projects", "ores.trading", "modeling",
                         "ores.trading.trade_type_catalogue.org")
ELEMENTS = os.path.join(HERE, "instrument_elements.json")
OUT = os.path.join(HERE, "instrument_terms.json")
DEFAULT_ORE = os.environ.get("ORES_ORE_ENGINE", "/home/marco/Vault/Development/ORE/Engine")
GUIDE_DIRS = ("Docs/UserGuide/tradedata", "Docs/UserGuide/tradecomponents")

XS = "{http://www.w3.org/2001/XMLSchema}"
NODE = re.compile(r"\\lstinline!(\w+)!\s+(?:node|element|sub-node)")

# The value kind of a simple type, by the XSD type it restricts.
KINDS = {
    "xs:string": "string", "xs:token": "string", "xs:normalizedString": "string",
    "xs:boolean": "boolean", "xs:integer": "integer", "xs:int": "integer",
    "xs:nonNegativeInteger": "integer", "xs:positiveInteger": "integer",
    "xs:long": "integer", "xs:unsignedInt": "integer",
    "xs:decimal": "double", "xs:double": "double", "xs:float": "double",
    "xs:date": "date", "xs:dateTime": "date",
}


class Schema:
    """The named types, groups and element declarations of the vendored schemas."""

    def __init__(self, directory):
        self.types, self.simple, self.groups, self.files = {}, {}, {}, {}
        self.elements, self.substitutes = {}, {}
        for path in sorted(glob.glob(os.path.join(directory, "*.xsd"))):
            name = os.path.basename(path)
            root = ET.parse(path).getroot()
            for c in root.findall(XS + "complexType"):
                self.types[c.get("name")] = c
                self.files[c.get("name")] = name
            for s in root.findall(XS + "simpleType"):
                self.simple[s.get("name")] = s
            for g in root.findall(XS + "group"):
                self.groups[g.get("name")] = g
            for e in root.findall(XS + "element"):
                self.elements[e.get("name")] = e
                self.files.setdefault(e.get("name"), name)
                if e.get("substitutionGroup"):
                    self.substitutes.setdefault(e.get("substitutionGroup"), []).append(e)

    def members(self, node):
        """The elements and attributes of a type, groups and bases resolved, in order."""
        return [member for member, _, _ in self.particles(node).values()]

    def particles(self, node):
        """Each member of a type by name, with whether it must be present and repeats.

        A member of a choice must be present only when every alternative requires it,
        and a member of an optional or repeated sequence is optional or repeated too.
        """
        out = {}
        for child in node:
            tag = child.tag
            if tag == XS + "element" and child.get("ref") in self.elements:
                found = self.bounded(child, self.referenced(child.get("ref")))
            elif tag in (XS + "element", XS + "attribute"):
                name = child.get("name") or child.get("ref")
                if tag == XS + "attribute":
                    found = {name: (child, child.get("use") == "required", False)}
                else:
                    found = {name: (child, child.get("minOccurs", "1") != "0",
                                    child.get("maxOccurs", "1") not in ("0", "1"))}
            elif tag == XS + "group" and child.get("ref"):
                found = self.bounded(child, self.particles(self.groups[child.get("ref")]))
            elif tag in (XS + "extension", XS + "restriction"):
                base = child.get("base")
                found = dict(self.particles(self.types[base])) if base in self.types else {}
                found.update(self.particles(child))
            elif tag == XS + "choice":
                found = self.bounded(child, self.alternatives(child))
            elif tag in (XS + "sequence", XS + "all", XS + "group"):
                found = self.bounded(child, self.particles(child))
            elif tag in (XS + "complexContent", XS + "simpleContent", XS + "complexType"):
                found = self.particles(child)
            else:
                continue
            for name, entry in found.items():
                if name not in out:
                    out[name] = entry
        return out

    def referenced(self, name):
        """A global element a type refers to, or the elements that substitute for it.

        An abstract element stands for its substitution group, as legDataType stands
        for FixedLegData, FloatingLegData and the other legs, so a reference to it is
        a choice of them.
        """
        element = self.elements[name]
        if element.get("abstract") != "true":
            return {name: (element, True, False)}
        return {s.get("name"): (s, False, False) for s in self.substitutes.get(name, [])}

    def alternatives(self, choice):
        branches = []
        for child in choice:
            if child.tag in (XS + "element", XS + "sequence", XS + "group", XS + "choice", XS + "all"):
                wrapper = ET.Element(XS + "sequence")
                wrapper.append(child)
                branches.append(self.particles(wrapper))
        out = {}
        for branch in branches:
            for name, (member, required, repeated) in branch.items():
                everywhere = all(name in b and b[name][1] for b in branches)
                previous = out.get(name)
                out[name] = (previous[0] if previous else member, everywhere and required,
                             repeated or (previous[2] if previous else False))
        return out

    @staticmethod
    def bounded(particle, found):
        optional = particle.get("minOccurs", "1") == "0"
        repeats = particle.get("maxOccurs", "1") not in ("0", "1")
        return {name: (member, required and not optional, repeated or repeats)
                for name, (member, required, repeated) in found.items()}

    def simple_kind(self, type_name, inline=None):
        """The value kind of a simple type, and the values an enumeration allows."""
        node = inline if inline is not None else self.simple.get(type_name)
        if node is None:
            return KINDS.get(type_name, "string"), []
        restriction = node.find(XS + "restriction")
        if restriction is None:
            return "string", []
        values = [e.get("value") for e in restriction.findall(XS + "enumeration")]
        base = restriction.get("base")
        kind = KINDS.get(base) or (self.simple_kind(base)[0] if base in self.simple else "string")
        return kind, values


def type_fields(schema, node, owner):
    """The fields of one complex type, and the anonymous types nested in it, by name."""
    fields, nested = [], {}
    for name, (member, required, repeated) in schema.particles(node).items():
        is_attribute = member.tag == XS + "attribute"
        type_name = member.get("type")
        inline_complex = member.find(XS + "complexType")
        inline_simple = member.find(XS + "simpleType")
        field = {"name": name, "attribute": is_attribute, "required": required,
                 "repeated": repeated, "default": member.get("default")}
        if type_name in schema.types:
            field["record"] = type_name
        elif inline_complex is not None and any(True for _ in schema.members(inline_complex)):
            anonymous = name if schema.elements.get(name) is member else f"{owner}.{name}"
            if anonymous in schema.types:
                raise ValueError(f"the inline type of {anonymous} has the name of a named type")
            field["record"] = anonymous
            nested[anonymous] = inline_complex
        else:
            kind, values = schema.simple_kind(type_name, inline_simple)
            field["kind"] = kind
            if values:
                field["values"] = values
        fields.append(field)
    return fields, nested


def catalogue_codes():
    text = open(CATALOGUE, encoding="utf-8").read()
    section = text.split("\n* Trade types\n", 1)[1].split("\n* ", 1)[0]
    header = next(line for line in section.splitlines() if line.startswith("| code"))
    columns = [c.strip() for c in header.strip().strip("|").split("|")]
    if columns[:4] != ["code", "name", "family", "construction"]:
        raise ValueError(f"the trade types table of {CATALOGUE} has the columns {columns}")
    codes = []
    for line in section.splitlines():
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if line.startswith("| ") and cells[0] not in ("code", "") and not set(cells[0]) <= {"-", "+"}:
            codes.append((cells[0], cells[3]))
    return codes


def guide_texts(ore):
    """The guide's field texts, by the node whose elements they describe."""
    out = {}
    for directory in GUIDE_DIRS:
        for path in sorted(glob.glob(os.path.join(ore, directory, "*.tex"))):
            lines = open(path, encoding="utf-8").read().splitlines()
            relative = os.path.relpath(path, ore)
            node, in_list = None, False
            for number, line in enumerate(lines, 1):
                named = NODE.search(line)
                if named and not in_list:
                    node = named.group(1)
                if "\\begin{itemize}" in line:
                    in_list = True
                if "\\end{itemize}" in line:
                    in_list = False
                item = ITEM.match(line)
                if in_list and item and node:
                    term = next(g for g in item.groups()[:4] if g)
                    text = item.group(6).strip()
                    k = number
                    while k < len(lines) and not re.match(r"\s*\\(item|end)", lines[k]):
                        text += " " + lines[k].strip()
                        k += 1
                    out.setdefault(node, {}).setdefault(term, {
                        "optional": bool(item.group(5)),
                        "text": clean(text),
                        "guide_ref": f"{relative}:{number}",
                    })
    return out


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--ore", default=DEFAULT_ORE, help="an ORE checkout, for the guide")
    args = parser.parse_args()

    schema = Schema(XSD)
    data_elements = {m.get("name"): m for m in schema.members(schema.groups["oreTradeData"])}
    curated = json.load(open(ELEMENTS, encoding="utf-8"))["elements"]

    products, unresolved, without_element = {}, [], {}
    for code, construction in catalogue_codes():
        entry = curated.get(code)
        element = entry["element"] if entry else code + "Data"
        if entry and element is None:
            without_element[code] = entry["evidence"]
            continue
        declared = entry.get("type_of", element) if entry else element
        if declared not in data_elements:
            unresolved.append(code)
            continue
        member = data_elements[declared]
        root = member.get("type") if member.get("type") in schema.types else declared
        products[code] = {"element": element, "root": root, "construction": construction,
                          **({"declared": declared} if declared != element else {}),
                          "evidence": entry["evidence"] if entry else "the type's name and Data"}

    types, pending, declared_by = {}, [], {}
    for code, product in products.items():
        member = data_elements[product.get("declared", product["element"])]
        if product["root"] in schema.types:
            pending.append((product["root"], schema.types[product["root"]]))
        else:
            pending.append((product["root"], member.find(XS + "complexType")))
        declared_by.setdefault(product["root"], set()).add(product["element"])
    while pending:
        name, node = pending.pop()
        if name in types or node is None:
            continue
        fields, nested = type_fields(schema, node, name)
        types[name] = {"schema": schema.files.get(name.split(".")[0], "input.xsd"),
                       "fields": fields}
        for field in fields:
            record = field.get("record")
            if record:
                declared_by.setdefault(record, set()).add(field["name"])
                if record in schema.types:
                    pending.append((record, schema.types[record]))
        for anonymous, inline in nested.items():
            pending.append((anonymous, inline))

    texts = {node.lower(): items for node, items in guide_texts(args.ore).items()}
    described, total, disputed = 0, 0, []
    for name, entry in types.items():
        own = name.split(".")[-1].lower()
        nodes = sorted(declared_by.get(name, set()), key=lambda n: (n.lower() != own, n))
        for field in entry["fields"]:
            total += 1
            found = [(node, texts[node.lower()][field["name"]]) for node in nodes
                     if field["name"] in texts.get(node.lower(), {})]
            if not found:
                continue
            node, text = found[0]
            field["text"] = text["text"]
            field["guide_ref"] = text["guide_ref"]
            field["guide_node"] = node
            described += 1
            if len({t["text"] for _, t in found}) > 1:
                disputed.append(f"{name}/{field['name']}")

    result = {"xsd_manifest": json.load(open(os.path.join(XSD, "manifest.json")))["engine_version"],
              "products": products, "types": types, "unresolved": unresolved,
              "without_element": without_element}
    with open(OUT, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, sort_keys=True)
        f.write("\n")
    print(f"{len(products)} products, {len(without_element)} with no element in the schema, "
          f"{len(unresolved)} unresolved ({', '.join(unresolved[:8])}); {len(types)} types, "
          f"{total} fields, {described} described by the guide; {len(disputed)} described "
          f"differently by two nodes, the type's own node or else the first winning "
          f"({', '.join(disputed[:5])}).")
    return 1 if unresolved else 0


if __name__ == "__main__":
    raise SystemExit(main())
