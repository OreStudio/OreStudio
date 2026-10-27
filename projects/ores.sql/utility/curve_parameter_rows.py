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
"""The curve configuration terms, one row for each leaf of curveconfig.xsd.

The leaves, whether each is required and the type of its value come from the
ORE source, through curve_terms.json, which extract_curve_terms.py writes. The
words come from the curveconfig chapter of the ORE guide, through the same
file, and from curve_term_curation.json, where a person states a text the guide
does not give, with its reason, source and curator. That file is created when the
first text is curated; until then the guide is the only source and every leaf it
does not describe is a gap.

A term with no text is not written. It is a gap of the contract, which the
screen shows as an undocumented leaf, and which undocumented() lists so the
coverage can be read.
"""

import json
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
TERMS = os.path.join(HERE, "curve_terms.json")
CURATION = os.path.join(HERE, "curve_term_curation.json")

SCHEMA_NAME = "curveconfig.xsd"

# The release ORE's curve configuration chapter and source were measured at.
RELEASE_FLOOR = "1.8.0.7 or earlier"

CURATION_KEYS = ("reason", "source", "source_ref", "curator", "date")
SOURCES = ("ore_guide", "ore_code", "authored", "seed")

DOMAIN_OF = {"boolean": "boolean", "integer": "integer", "double": "double"}

FORMAT_OF = {
    "boolean": "true or false.",
    "integer": "A whole number.",
    "double": "A number.",
    "text": "Text.",
}

ABBREVIATIONS = ("i.e.", "e.g.", "etc.", "vs.")


def humanise(name):
    words = re.findall(r"[A-Z]+(?![a-z])|[A-Z]?[a-z]+|[0-9]+", name) or [name]
    words = [w if (len(w) > 1 and w.isupper()) else w.lower() for w in words]
    text = " ".join(words)
    return text[0].upper() + text[1:] if text else name


def first_sentence(text):
    match = re.match(r"(.+?[.!?])(\s|$)", text)
    return match.group(1) if match else text


def sentence_from(text, start):
    """The text from start to the end of its sentence, read past an abbreviation."""
    i = start
    while i < len(text):
        if text[i] == "." and (i + 1 == len(text) or text[i + 1] == " "):
            if not any(text[:i + 1].endswith(a) for a in ABBREVIATIONS):
                return text[start:i]
        i += 1
    return text[start:]


def default_of(term, text):
    """The default the source or the guide states, or a sentence that says none."""
    if term.get("default"):
        return f"{term['default']}."
    stated = re.search(r"\b[Dd]efaults? (?:to|is) ", text)
    if stated:
        value = sentence_from(text, stated.end()).split(" (")[0].strip()
        if value:
            return value + "."
    if term["mandatory"] and not term.get("conditional"):
        return "None: the curve must give it."
    if term["mandatory"]:
        return "None where the curve needs it; the description says when."
    return "Not given: ORE applies its own rule."


def curation():
    if not os.path.exists(CURATION):
        return {}
    with open(CURATION) as f:
        entries = json.load(f)
    for key, entry in entries.items():
        missing = [k for k in CURATION_KEYS if not entry.get(k)]
        if missing:
            raise ValueError(f"curation entry {key} has no {', '.join(missing)}")
        if entry["source"] not in SOURCES:
            raise ValueError(f"curation entry {key} has an unknown source {entry['source']}")
    return entries


def xpath_of(term):
    return "/CurveConfiguration/" + "/".join(term["path"])


def terms():
    with open(TERMS) as f:
        extracted = json.load(f)
    for section, body in sorted(extracted["sections"].items()):
        for position, term in enumerate(body["terms"]):
            yield section, position, term


def curated_for(entries, section, term):
    """The curation of a term: its own entry, which overrides the guide, or else a
    wildcard entry, which only fills a term the guide leaves without text."""
    own = entries.get(f"{section}/{term['name']}")
    if own:
        return own
    if not term.get("guide_text"):
        return entries.get(f"*/{term['name']}")
    return None


def rows():
    entries = curation()
    out = []
    for section, position, term in terms():
        curated = curated_for(entries, section, term) or {}
        text = curated.get("description") or term.get("guide_text")
        if not text:
            continue
        out.append({
            "schema_name": SCHEMA_NAME,
            "scope": section,
            "subtype": "/".join(term["path"][1:-1]),
            "name": term["name"],
            "position": position,
            "domain": DOMAIN_OF.get(term["type"], "string"),
            "required": term["mandatory"] and not term.get("conditional"),
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
    return [xpath_of(term) for section, _, term in terms()
            if not (curated_for(entries, section, term) or {}).get("description")
            and not term.get("guide_text")]


def missing(all_rows):
    keys = ("label", "help", "description", "default_text", "format", "introduced_in")
    return [r["xpath"] for r in all_rows if any(not r.get(k) for k in keys)]
