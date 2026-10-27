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
"""The value sets of the curve configuration leaves, and the link from each leaf.

The conventions derive does not carry across. A curve config declares no `build`
method, and ORE parses its strings in the curve construction code, so the parser
a term's read wraps is present for only a handful of leaves. The set therefore
comes from three places, in this order:

- a leaf whose name is one of ORE's vocabularies takes that vocabulary's
  catalogue: a currency, a day counter, a business day convention;
- a leaf the curation links to a parser takes that parser's list, which
  curve_terms.json records from the ORE source, so the values are derived even
  where the link is authored;
- a leaf whose type the extractor read takes its type's set;
- every other leaf is text. ORE keeps it as text in the configuration, so the
  screen gives it a text field and the write path checks nothing about it. That
  is a stated outcome, not a silent fallback: text_of() lists the leaves that
  fall here, so the coverage can be read.

A calendar is text, as it is for the conventions: ORE accepts a joint calendar
such as US,MWK, and the refdata calendar catalogue holds too few of them for a
catalogue to be safe.

Only a leaf the contract documents is linked, because the link names a row of it.
"""

import json
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
TERMS = os.path.join(HERE, "curve_terms.json")
CURATION = os.path.join(HERE, "curve_value_set_curation.json")

CURATION_KEYS = ("link", "reason", "source", "source_ref", "curator", "date")
SOURCES = ("ore_guide", "ore_code", "authored", "seed")

CATALOGUES = {
    "refdata.business_day_convention": {
        "name": "Business day conventions",
        "relation": "ores_refdata_business_day_convention_catalogue_v",
        "description": "The business day conventions reference data holds.",
    },
    "refdata.day_counter": {
        "name": "Day counters",
        "relation": "ores_refdata_day_counter_catalogue_v",
        "description": "The day counters reference data holds.",
    },
    "refdata.currency": {
        "name": "Currencies",
        "relation": "ores_refdata_currency_catalogue_v",
        "description": "The currencies reference data holds.",
    },
}

# The leaves whose name is one of ORE's vocabularies, and the catalogue each takes.
CATALOGUE_OF = {
    "Currency": "refdata.currency",
    "DayCounter": "refdata.day_counter",
    "YieldCurveDayCounter": "refdata.day_counter",
    "BusinessDayConvention": "refdata.business_day_convention",
}

TYPE_SETS = {
    "boolean": "curve.flag",
    "double": "curve.number",
    "integer": "curve.integer",
    "period": "curve.period",
}

OWN_SETS = {
    "curve.flag": {
        "name": "Flag",
        "description": "True or false, as ORE writes a boolean.",
        "source_kind": "boolean",
        "is_ordered": False,
    },
    "curve.number": {
        "name": "Number",
        "description": "A number the ORE code parses as a real.",
        "source_kind": "number",
        "is_ordered": False,
    },
    "curve.integer": {
        "name": "Whole number",
        "description": "A whole number the ORE code parses as an integer.",
        "source_kind": "integer",
        "is_ordered": False,
    },
    "curve.period": {
        "name": "Period",
        "description": "A period such as 2D, 3M or 1Y6M, which the ORE code parses as a period.",
        "source_kind": "regex",
        "pattern": "^([0-9]+[DdWwMmYy])+$",
        "is_ordered": False,
    },
    "curve.text": {
        "name": "Text",
        "description": "Text ORE reads as it stands. The curve configuration holds it as "
                       "text, so no list narrows it.",
        "source_kind": "text",
        "is_ordered": False,
    },
}


def words(name):
    parts = re.findall(r"[A-Z]+(?![a-z])|[A-Z]?[a-z]+|[0-9]+", name) or [name]
    text = " ".join(p if (len(p) > 1 and p.isupper()) else p.lower() for p in parts)
    return text[0].upper() + text[1:]


def parser_set_code(parser):
    return "curve." + re.sub(r"(?<!^)(?=[A-Z])", "_", parser[len("parse"):]).lower()


def parser_set(parser, entry):
    """The set of one parser's map: every string it accepts, labelled with what it means."""
    if len(entry["values"]) < 2:
        raise ValueError(f"{parser} lists {len(entry['values'])} value at {entry['source_ref']}; "
                         "the extractor likely misread it")
    meaning_of, values = {}, []
    for item in entry["values"]:
        if item["value"] in meaning_of:
            if meaning_of[item["value"]] != item["means"]:
                raise ValueError(f"{parser} maps {item['value']} to both "
                                 f"{meaning_of[item['value']]} and {item['means']}")
            continue
        meaning_of[item["value"]] = item["means"]
        meaning = words(item["means"])
        values.append({
            "value": item["value"],
            "label": meaning if meaning.lower() != item["value"].lower() else item["value"],
            "description": f"{item['value']} is one of the values the ORE code accepts, "
                           f"meaning {meaning}.",
            "order": len(values),
        })
    label = words(parser[len("parse"):])
    return {
        "name": label,
        "description": f"The values the ORE parser {parser} accepts, from its map at "
                       f"{entry['source_ref']}.",
        "source_kind": "enumerated",
        "is_ordered": False,
        "values": values,
    }


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
        if not entry["link"].startswith(("parse", "refdata.")):
            raise ValueError(f"curation entry {key} links to {entry['link']}, "
                             "which is neither a parser nor a catalogue")
    return entries


def terms():
    with open(TERMS) as f:
        extracted = json.load(f)
    for section, body in sorted(extracted["sections"].items()):
        for term in body["terms"]:
            yield section, term


def curated_for(curated, section, name):
    """The curation of a leaf: its own section's entry, or a wildcard entry.

    A link names one parser, and a name may mean the same thing in every section
    that carries it, so a wildcard is allowed. It is never assumed: the entry
    says which sections it covers by the key it uses.
    """
    return curated.get(f"{section}|{name}") or curated.get(f"*|{name}")


def set_of(term, curated, parsers, section):
    """The set code of one leaf, and the parser whose list it is, if any."""
    name = term["name"]
    entry = curated_for(curated, section, name)
    if entry:
        link = entry["link"]
        if link.startswith("refdata."):
            return "curve." + link.split(".", 1)[1], None
        if link not in parsers:
            raise ValueError(f"{name}: the curation links to {link}, which no ORE source "
                             "states a list for; correct the entry or drop it")
        return parser_set_code(link), link
    if name in CATALOGUE_OF:
        return "curve." + CATALOGUE_OF[name].split(".", 1)[1], None
    if term["type"] in TYPE_SETS:
        return TYPE_SETS[term["type"]], None
    return "curve.text", None


def data():
    """The catalogues, the sets and the links the curve leaves add."""
    with open(TERMS) as f:
        extracted = json.load(f)
    parsers = extracted["parsers"]
    curated = curation()
    import curve_parameter_rows
    documented = {(r["scope"], r["name"]) for r in curve_parameter_rows.rows()}
    sets = dict(OWN_SETS)
    for catalogue in CATALOGUE_OF.values():
        sets["curve." + catalogue.split(".", 1)[1]] = {
            "name": CATALOGUES[catalogue]["name"],
            "description": f"One of the {CATALOGUES[catalogue]['name'].lower()} "
                           "that reference data holds.",
            "source_kind": "catalogue",
            "catalogue_code": catalogue,
            "is_ordered": False,
        }
    links = {}
    for section, term in terms():
        if (section, term["name"]) not in documented:
            continue
        code, parser = set_of(term, curated, parsers, section)
        if parser and code not in sets:
            sets[code] = parser_set(parser, parsers[parser])
        key = f"{section}|{term['name']}"
        if links.get(key, code) != code:
            raise ValueError(f"{key} would take two sets, {links[key]} and {code}")
        links[key] = code
    for key in curated:
        section, _, name = key.partition("|")
        if not any((section in ("", "*") or section == sec) and nm == name
                   for sec, nm in documented):
            raise ValueError(f"curation entry {key} matches no documented curve leaf; "
                             "correct the key or drop the entry")
    named = set(links.values())
    sets = {code: entry for code, entry in sets.items() if code in named}
    catalogues = {code: entry for code, entry in CATALOGUES.items()
                  if any(s.get("catalogue_code") == code for s in sets.values())}
    return {"catalogues": catalogues, "sets": sets, "links": links}


def catalogue_scopes():
    """The sections each catalogue name is matched in, so a name that comes to
    mean something else in a new family is seen rather than assumed."""
    with open(TERMS) as f:
        extracted = json.load(f)
    scopes = {}
    for section, body in extracted["sections"].items():
        for term in body["terms"]:
            if term["name"] in CATALOGUE_OF:
                scopes.setdefault(term["name"], set()).add(section)
    return {name: sorted(found) for name, found in sorted(scopes.items())}


def text_of():
    """The documented leaves that fall to text, so the coverage can be read."""
    with open(TERMS) as f:
        extracted = json.load(f)
    parsers = extracted["parsers"]
    curated = curation()
    import curve_parameter_rows
    documented = {(r["scope"], r["name"]) for r in curve_parameter_rows.rows()}
    return sorted({f"{section}|{term['name']}" for section, term in terms()
                   if (section, term["name"]) in documented
                   and set_of(term, curated, parsers, section)[0] == "curve.text"})
