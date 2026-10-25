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
"""The convention terms, one row for each leaf of the conventions document.

The terms, whether each is required and the type of its value come from the ORE
source, through convention_terms.json, which extract_convention_terms.py writes. The
words come from the conventions chapter of the ORE guide, through the same file, and
from convention_term_curation.json, where a person states a text the guide does not
give, with its reason, source and curator.

A term with no text is not written. It is a gap of the contract, which the screen shows
as an undocumented leaf, and which undocumented() lists so the coverage can be read.
"""

import json
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
TERMS = os.path.join(HERE, "convention_terms.json")
CURATION = os.path.join(HERE, "convention_term_curation.json")

SCHEMA_NAME = "conventions.xsd"

# The release ORE's conventions chapter and source were measured at. A term whose
# release is not measured takes the floor the pricing parameters use.
RELEASE_FLOOR = "1.8.0.7 or earlier"

CURATION_KEYS = ("reason", "source", "source_ref", "curator", "date")
SOURCES = ("ore_guide", "ore_code", "authored", "seed")

DOMAIN_OF = {"boolean": "boolean", "integer": "integer", "double": "double"}

FORMAT_OF = {
    "boolean": "true or false.",
    "integer": "A whole number.",
    "double": "A number.",
    "calendar": "A calendar, such as TARGET or US-NYSE.",
    "day_counter": "A day counter, such as A360 or ACT/365.",
    "business_day_convention": "A business day convention, such as ModifiedFollowing or F.",
    "frequency": "A frequency, such as Annual or Quarterly.",
    "period": "A period, such as 2D or 3M.",
    "currency": "An ISO currency code, such as EUR.",
    "index": "An index name, such as EUR-EURIBOR-6M.",
    "compounding": "A compounding rule, such as Simple or Compounded.",
    "date_generation_rule": "A date generation rule, such as Backward.",
    "weekday": "A weekday, such as Mon.",
    "month": "A month, such as Jan.",
    "enumerated": "One of the values the description names.",
    "text": "Text.",
}


def curation():
    with open(CURATION) as f:
        entries = json.load(f)
    for key, entry in entries.items():
        missing = [k for k in CURATION_KEYS if not entry.get(k)]
        if missing:
            raise ValueError(f"curation entry {key} has no {', '.join(missing)}")
        if entry["source"] not in SOURCES:
            raise ValueError(f"curation entry {key} has an unknown source {entry['source']}")
    return entries


def humanise(name):
    words = re.findall(r"[A-Z]+(?![a-z])|[A-Z]?[a-z]+|[0-9]+", name) or [name]
    words = [w if (len(w) > 1 and w.isupper()) else w.lower() for w in words]
    text = " ".join(words)
    return text[0].upper() + text[1:]


def first_sentence(text):
    match = re.match(r"(.+?[.!?])(\s|$)", text)
    return match.group(1) if match else text


def default_of(term, text):
    stated = re.search(r"\b[Dd]efaults? (?:to|is) ([^.;,]+)", text) or \
        re.search(r"\(default:? ([^)]+)\)", text)
    if stated:
        return stated.group(1).strip().strip('"') + "."
    if term["mandatory"] and not term["conditional"]:
        return "None: the convention must give it."
    if term["mandatory"]:
        return "None where the convention needs it; the description says when."
    return "Not given: ORE applies its own rule."


def xpath_of(term):
    leaf = term["name"]
    if term.get("list_item"):
        leaf = f"{term['name']}/{term['list_item']}"
    return "/Conventions/" + "/".join(term["path"] + [leaf])


def terms():
    with open(TERMS) as f:
        extracted = json.load(f)
    for element, convention in sorted(extracted["conventions"].items()):
        for position, term in enumerate(convention["terms"]):
            yield element, position, term


def curated_for(entries, element, term):
    """The curation of a term: its own entry, which overrides the guide, or else a
    wildcard entry, which only fills a term the guide leaves without text."""
    own = entries.get(f"{element}/{term['name']}")
    if own:
        return own
    if not term["guide_text"]:
        return entries.get(f"*/{term['name']}")
    return None


def rows():
    entries = curation()
    out = []
    for element, position, term in terms():
        curated = curated_for(entries, element, term) or {}
        text = curated.get("description") or term["guide_text"]
        if not text:
            continue
        out.append({
            "schema_name": SCHEMA_NAME,
            "scope": element,
            "subtype": "/".join(term["path"][1:]),
            "name": term["name"],
            "position": position,
            "domain": DOMAIN_OF.get(term["type"], "string"),
            "required": term["mandatory"] and not term["conditional"],
            "label": curated.get("label") or humanise(term["name"]),
            "help": first_sentence(text),
            "description": text,
            "default_text": default_of(term, text),
            "format": FORMAT_OF.get(term["type"], "Text."),
            "introduced_in": RELEASE_FLOOR,
            "removed_in": None,
            "xpath": xpath_of(term),
        })
    return out


def undocumented():
    """The terms the contract holds no text for, as XPaths."""
    entries = curation()
    return [xpath_of(term) for element, _, term in terms()
            if not (curated_for(entries, element, term) or {}).get("description")
            and not term["guide_text"]]


def missing(all_rows):
    keys = ("label", "help", "description", "default_text", "format", "introduced_in")
    return [r["xpath"] for r in all_rows if any(not r.get(k) for k in keys)]
