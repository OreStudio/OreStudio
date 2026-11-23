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
"""The fields of the native products, one row for each field of each schema type.

The fields come from the vendored ORE schemas, through instrument_terms.json, which
extract_instrument_terms.py writes; the words come from the trade data and trade
component sections of the ORE guide, through the same file.

A row describes a field of a type, not a leaf of a document: a type such as legData
is shared by many products and is described once. Its XPath is relative to its type,
/legData/Payer, and its subtype is the type. A field whose value is another type
names that type through its value domain, record:legData, whose storage kind is
record and whose referenced entity is the type; a field that repeats has a list
domain, list:legData or list:double. The data element of each product is a field
of the trade of its type, /Trade[TradeType='Swap']/SwapData, so a product's fields
are read from its trade type down, and the element each type is read under is
stated where ORE's schema states it.

Every field is written, because a form needs every field. A field the guide does
not describe gets a text that says so, and undocumented() lists it, so the
coverage can be read.
"""

import json
import os

from curve_parameter_rows import first_sentence, humanise
from extract_instrument_terms import catalogue_codes

HERE = os.path.dirname(os.path.abspath(__file__))
TERMS = os.path.join(HERE, "instrument_terms.json")

SCOPE = "instrument"
TRADE = "trade"

LEAF_DOMAINS = ("string", "boolean", "integer", "double", "date")

FORMAT_OF = {
    "boolean": "true or false.",
    "integer": "A whole number.",
    "double": "A number.",
    "date": "A date.",
    "string": "Text.",
}


def terms():
    with open(TERMS) as f:
        return json.load(f)


def release_floor(extracted):
    return f"{extracted['xsd_manifest']} or earlier"


def domain_of(field):
    target = field.get("record") or field["kind"]
    if field["repeated"]:
        return f"list:{target}"
    return f"record:{target}" if field.get("record") else target


def format_of(field):
    if field.get("record"):
        one = f"A {humanise(field['record'].split('.')[-1]).lower()} record"
    else:
        one = FORMAT_OF[field["kind"]].rstrip(".")
        if field.get("values"):
            one = "One of " + ", ".join(field["values"])
    return f"{one}, repeated." if field["repeated"] else f"{one}."


def default_of(field):
    if field.get("default"):
        return f"{field['default']}."
    if field["required"]:
        return "None: the trade must give it."
    return "Not given: ORE applies its own rule."


def xpath_of(owner, field):
    if owner == TRADE:
        return f"/Trade[TradeType='{field['trade_type']}']/{field['name']}"
    leaf = f"@{field['name']}" if field["attribute"] else field["name"]
    return "/" + owner.replace(".", "/") + "/" + leaf


def unstated_text(owner, field):
    kind = "attribute" if field["attribute"] else "element"
    return (f"The {field['name']} {kind} of {owner.split('.')[-1]}, "
            f"which the ORE user guide does not describe.")


def fields():
    """Each field with its owner: the trade's product elements, then every type's."""
    extracted = terms()
    for position, (code, product) in enumerate(sorted(extracted["products"].items())):
        yield TRADE, position, {"name": product["element"], "trade_type": code,
                                "attribute": False, "required": True, "repeated": False,
                                "default": None, "record": product["root"]}
    for owner, entry in sorted(extracted["types"].items()):
        for position, field in enumerate(entry["fields"]):
            yield owner, position, field


def value_domains():
    """The domains the fields name beyond the leaf kinds: (code, name, kind, entity)."""
    out = {}
    for _, _, field in fields():
        code = domain_of(field)
        if code in LEAF_DOMAINS or code in out:
            continue
        target = field.get("record") or field["kind"]
        if field["repeated"]:
            out[code] = (code, f"List of {target}", "list", target)
        else:
            out[code] = (code, f"Record of {target}", "record", target)
    return sorted(out.values())


def unfielded(extracted):
    """The native products of the catalogue whose fields the extraction does not give."""
    out = []
    for code, construction in catalogue_codes():
        if construction != "native" or code in extracted["without_element"]:
            continue
        product = extracted["products"].get(code)
        if product is None or not extracted["types"].get(product["root"], {}).get("fields"):
            out.append(code)
    return out


def rows():
    extracted = terms()
    gaps = unfielded(extracted)
    if gaps:
        raise ValueError(f"native products with no fields: {', '.join(gaps)}; run "
                         f"extract_instrument_terms.py, or state their element in "
                         f"instrument_elements.json")
    floor = release_floor(extracted)
    out = []
    for owner, position, field in fields():
        text = field.get("text") or (
            f"The data of a {field['trade_type']} trade, read under {field['name']}."
            if owner == TRADE else unstated_text(owner, field))
        out.append({
            "schema_name": extracted["types"].get(owner, {}).get("schema", "input.xsd"),
            "scope": SCOPE,
            "subtype": owner,
            "name": field["name"],
            "position": position,
            "domain": domain_of(field),
            "required": field["required"],
            "label": humanise(field["name"]),
            "help": first_sentence(text) if field.get("text") or owner == TRADE else format_of(field),
            "description": text,
            "default_text": default_of(field),
            "format": format_of(field),
            "introduced_in": floor,
            "removed_in": None,
            "xpath": xpath_of(owner, field),
        })
    return out


def missing(all_rows):
    keys = ("label", "help", "description", "default_text", "format", "introduced_in")
    return [r["xpath"] for r in all_rows if any(not r.get(k) for k in keys)]


def undocumented():
    """The fields the guide gives no text for, as XPaths."""
    return [xpath_of(owner, field) for owner, _, field in fields()
            if owner != TRADE and not field.get("text")]
