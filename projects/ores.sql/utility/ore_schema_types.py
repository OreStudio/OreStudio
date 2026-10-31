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
import re
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
_RULES = {}


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

    A type the schema enumerates states a set of values; a type it restricts to a
    pattern or a range states a shape one value must have; a built-in type states
    its kind. Any other named type states nothing narrower than a string, and the
    caller keeps its own answer.
    """
    found = declared(name, xpath)
    kind = KIND_OF.get(found)
    if kind:
        return (kind, found)
    stated = restriction(found)
    return (stated["kind"], found) if stated else None


def _load_rules():
    """What each schema simple type restricts, and where it is declared."""
    if _RULES:
        return _RULES
    for name in sorted(os.listdir(XSD)):
        if not name.endswith(".xsd"):
            continue
        path = os.path.join(XSD, name)
        root = ET.parse(path).getroot()
        with open(path, encoding="utf-8") as handle:
            text = handle.read()
        for node in root.findall(XS + "simpleType"):
            declared_at = text.find(f'name="{node.get("name")}"')
            entry = {"file": name,
                     "line": text[:declared_at].count("\n") + 1 if declared_at >= 0 else 0}
            values = [item.get("value") for item in node.iter(XS + "enumeration")
                      if item.get("value") is not None]
            if values:
                entry["values"] = values
            node_restriction = node.find(XS + "restriction")
            if node_restriction is not None:
                entry["base"] = node_restriction.get("base")
                pattern = node_restriction.find(XS + "pattern")
                if pattern is not None:
                    entry["pattern"] = pattern.get("value")
                low = node_restriction.find(XS + "minInclusive")
                high = node_restriction.find(XS + "maxInclusive")
                if low is not None:
                    entry["minimum"] = low.get("value")
                if high is not None:
                    entry["maximum"] = high.get("value")
            _RULES.setdefault(node.get("name"), entry)
    return _RULES


def restriction(type_name):
    """What one schema simple type states, when it states more than a string."""
    found = _load_rules().get(type_name)
    if not found:
        return None
    if found.get("values"):
        return dict(found, kind="enumerated")
    base = KIND_OF.get(found.get("base"))
    if found.get("pattern") is not None:
        return dict(found, kind=base or "regex")
    if found.get("minimum") is not None or found.get("maximum") is not None:
        return dict(found, kind=base or "range")
    return dict(found, kind=base) if base else None


def words(name):
    """A schema type's name as a person reads it, for example Dimension type."""
    parts = re.findall(r"[A-Z]+(?![a-z])|[A-Z]?[a-z]+|[0-9]+", name) or [name]
    text = " ".join(part if (len(part) > 1 and part.isupper()) else part.lower()
                    for part in parts)
    return text[0].upper() + text[1:]


def stated_set(kind, type_name, prefix):
    """The set row the schema states for one simple type, and its code.

    The row states what the schema states and nothing more: the values it
    enumerates, the pattern a value must match, or the bounds it allows. The
    pattern is anchored, because a set states what the whole value must be and
    Postgres matches a pattern anywhere in a string otherwise.
    """
    found = restriction(type_name)
    code = prefix + re.sub(r"(?<!^)(?=[A-Z])", "_", type_name).lower()
    where = f"external/ore/xsd/{found['file']}:{found['line']}"
    entry = {"name": words(type_name), "source_kind": kind, "is_ordered": False}
    if kind == "enumerated":
        values = [value for value in found["values"] if value != ""]
        entry["description"] = (f"The values the ORE schema states for {type_name}, "
                                f"from {where}.")
        entry["values"] = [{"value": value, "label": value,
                            "description": f"{value} is one of the values the ORE "
                                           f"schema states for {type_name}.",
                            "order": order}
                           for order, value in enumerate(values)]
        return code, entry
    if found.get("pattern") is not None:
        entry["description"] = (f"The pattern a {type_name} must match, as the ORE "
                                f"schema states it at {where}.")
        entry["pattern"] = "^(" + found["pattern"] + ")$"
    else:
        entry["description"] = (f"The values the ORE schema allows for {type_name}: "
                                f"from {found.get('minimum', 'minus infinity')} to "
                                f"{found.get('maximum', 'infinity')}, at {where}.")
        if found.get("minimum") is not None:
            entry["minimum"] = float(found["minimum"])
        if found.get("maximum") is not None:
            entry["maximum"] = float(found["maximum"])
    return code, entry


def enumeration(type_name):
    """The values the schema enumerates for one simple type, and where it states them."""
    found = _load_rules().get(type_name)
    if not found or not found.get("values"):
        return None
    return (found["values"], found["file"], found["line"])
