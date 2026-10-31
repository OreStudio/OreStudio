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
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
"""The type the ORE schema declares for a leaf, and the kind of value it states.

The ORE parser is the first authority for a leaf's rule, because it says what the
engine accepts. It is not the only one. Where the parser reads a leaf as a plain
string, the schema still declares a type, and a leaf declared =xs:integer= is not
free text: a document that writes =two= there is one the engine cannot parse,
even though the parser hands the string on.

This module walks the schema from the document's root element to the leaf and
states the type it finds, when that type is narrower than =xs:string=. A named
simple type the schema restricts to a kind this module knows, such as =bool= or
=period=, counts as narrower; any other named type states no rule here, so the
caller keeps its own answer.

    rule("curveconfig.xsd", "/CurveConfiguration/.../SpotLag")  -> ("integer", "xs:integer")
    rule("curveconfig.xsd", "/CurveConfiguration/.../CurveId")  -> None

The walk follows a named complex type and an inline one, because ORE declares both
and a leaf below an inline block is as typed as any other. The schemas are read
from =external/ore/xsd=, the ORE release the tree vendors.
"""
import os
import xml.etree.ElementTree as ET

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
XSD = os.path.join(ROOT, "external", "ore", "xsd")

XS = "{http://www.w3.org/2001/XMLSchema}"

# The types narrower than a string, and the kind of value each states.
KIND_OF = {
    "xs:integer": "integer", "xs:int": "integer", "xs:long": "integer",
    "xs:short": "integer", "xs:unsignedLong": "integer",
    "xs:nonNegativeInteger": "integer", "xs:positiveInteger": "integer",
    "xs:double": "number", "xs:float": "number", "xs:decimal": "number",
    "positiveDecimal": "number",
    "xs:boolean": "boolean", "bool": "boolean",
    "period": "period",
}

_CACHE = {}
_ENUMS = {}


def _elements(node):
    """The element declarations a container holds, one level down."""
    found = {}
    for tag in ("sequence", "choice", "all"):
        for holder in node.findall(XS + tag):
            for child in holder:
                if child.tag == XS + "element" and child.get("name"):
                    found.setdefault(child.get("name"), child)
                elif child.tag in (XS + "sequence", XS + "choice", XS + "all"):
                    found.update(_elements(child))
    return found


def _children(element, complex_types):
    """The element declarations a child of this element may be."""
    inline = element.find(XS + "complexType")
    if inline is not None:
        return _elements(inline)
    node = complex_types.get(element.get("type"))
    return _elements(node) if node is not None else {}


def _load(name):
    """The root elements and the named complex types one schema declares."""
    if name in _CACHE:
        return _CACHE[name]
    root = ET.parse(os.path.join(XSD, name)).getroot()
    complex_types = {node.get("name"): node
                     for node in root.findall(XS + "complexType")}
    tops = {node.get("name"): node for node in root.findall(XS + "element")}
    _CACHE[name] = (tops, complex_types)
    return _CACHE[name]


def declared(name, xpath):
    """The type the schema declares for one leaf, spelled as the schema spells it.

    None when the walk cannot reach the leaf: a path the schema states through a
    construct this reader does not follow. The caller states text for such a leaf
    and the count is read from the map rather than assumed absent.
    """
    tops, complex_types = _load(name)
    parts = [part for part in xpath.strip("/").split("/") if part]
    if not parts or parts[0] not in tops:
        return None
    element = tops[parts[0]]
    for part in parts[1:]:
        found = _children(element, complex_types)
        if part not in found:
            return None
        element = found[part]
    return element.get("type")


def rule(name, xpath):
    """The kind one leaf's value has, and the schema type that states it, if any.

    An enumerated type states a kind of its own: the schema lists the values, so
    the rule is a set of them rather than a shape one value must have.
    """
    found = declared(name, xpath)
    kind = KIND_OF.get(found)
    if kind:
        return (kind, found)
    if found in _load_enums():
        return ("enumerated", found)
    return None


def _load_enums():
    """The simple types the schemas enumerate, with the values and where they sit."""
    if _ENUMS:
        return _ENUMS
    for name in sorted(os.listdir(XSD)):
        if not name.endswith(".xsd"):
            continue
        path = os.path.join(XSD, name)
        root = ET.parse(path).getroot()
        with open(path, encoding="utf-8") as handle:
            text = handle.read()
        for node in root.findall(XS + "simpleType"):
            values = [item.get("value") for item in node.iter(XS + "enumeration")
                      if item.get("value") is not None]
            if values:
                _ENUMS[node.get("name")] = {
                    "values": values,
                    "file": name,
                    "line": text[:text.find(f'name="{node.get("name")}"')].count("\n") + 1,
                }
    return _ENUMS


def enumeration(type_name):
    """The values the schema enumerates for one simple type, and where it states them."""
    found = _load_enums().get(type_name)
    return (found["values"], found["file"], found["line"]) if found else None
