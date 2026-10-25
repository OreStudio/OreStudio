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
"""The value sets of the convention terms, and the link from each term to its set.

A term's set follows from the type the ORE parser gives its value, as
convention_terms.json records it:

- a business day convention, a day counter or a currency draws on the refdata
  catalogue of that kind, which holds every value the seeded conventions use except
  two currency fund codes, MXV and CLF;
- a frequency, a compounding rule, a date generation rule, a month, a weekday or
  another value a parser maps is the list that parser's own map accepts, each string
  with the word it means;
- a flag, a whole number, a number and a period take their type's set;
- a calendar is text. ORE reads a joint calendar such as US,MWK and a currency code
  as a calendar, and the refdata calendar catalogue holds 28 of the 254 calendar
  values the seeded conventions use, so a catalogue would refuse good conventions;
- an index and any other term is text.

Only a term the contract documents is linked, because the link names a row of it.

A term whose parser states no list falls back to text only when its type says why: a
calendar, an index, a number and the like. Any other parser with no list stops the
generator, because a parser that gained a list, or a list the extractor failed to
read, would otherwise leave the term free text with no one told.
"""

import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import convention_parameter_rows  # noqa: E402

TERMS = os.path.join(os.path.dirname(os.path.abspath(__file__)), "convention_terms.json")

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

CATALOGUE_OF = {
    "business_day_convention": "refdata.business_day_convention",
    "day_counter": "refdata.day_counter",
    "currency": "refdata.currency",
}

# The kinds whose parser states no list, and the set each takes instead.
WITHOUT_LIST = {"calendar": "text", "index": "text", "text": "text"}

# A kind whose value one parser lists, whichever parser the term names.
LIST_OF = {"frequency": "parseFrequency"}

TYPE_SETS = {
    "boolean": "flag",
    "double": "number",
    "integer": "convention.integer",
    "period": "convention.period",
}

OWN_SETS = {
    "convention.integer": {
        "name": "Whole number",
        "description": "A whole number the ORE code parses as an integer.",
        "source_kind": "integer",
        "is_ordered": False,
    },
    "convention.period": {
        "name": "Period",
        "description": "A period such as 2D, 3M or 1Y6M, which the ORE code parses as a period.",
        "source_kind": "regex",
        "pattern": "^([0-9]+[DdWwMmYy])+$",
        "is_ordered": False,
    },
}


def words(name):
    parts = re.findall(r"[A-Z]+(?![a-z])|[A-Z]?[a-z]+|[0-9]+", name) or [name]
    text = " ".join(p if (len(p) > 1 and p.isupper()) else p.lower() for p in parts)
    return text[0].upper() + text[1:]


def parser_set_code(parser):
    return "convention." + re.sub(r"(?<!^)(?=[A-Z])", "_", parser[len("parse"):]).lower()


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


def set_of(term, parsers):
    """The set code of one term, and the parser whose list it is, if any."""
    kind = term["type"]
    if kind in CATALOGUE_OF:
        return "convention." + kind, None
    if kind in TYPE_SETS:
        return TYPE_SETS[kind], None
    parser = LIST_OF.get(kind, term["parser"])
    if parser and parser in parsers:
        return parser_set_code(parser), parser
    if kind in WITHOUT_LIST:
        return WITHOUT_LIST[kind], None
    raise ValueError(f"{term['source_ref']}: {term['name']} is read by {term['parser']}, "
                     f"which states no list, and its kind {kind} names no set")


def data():
    """The catalogues, the sets and the links the convention terms add."""
    with open(TERMS) as f:
        extracted = json.load(f)
    parsers = extracted["parsers"]
    documented = {(r["scope"], r["name"]) for r in convention_parameter_rows.rows()}
    sets = dict(OWN_SETS)
    for kind, catalogue in CATALOGUE_OF.items():
        sets["convention." + kind] = {
            "name": CATALOGUES[catalogue]["name"],
            "description": f"One of the {CATALOGUES[catalogue]['name'].lower()} "
                           "that reference data holds.",
            "source_kind": "catalogue",
            "catalogue_code": catalogue,
            "is_ordered": False,
        }
    links = {}
    for element, convention in sorted(extracted["conventions"].items()):
        for term in convention["terms"]:
            if (element, term["name"]) not in documented:
                continue
            code, parser = set_of(term, parsers)
            if parser and code not in sets:
                sets[code] = parser_set(parser, parsers[parser])
            key = f"{element}|{term['name']}"
            if links.get(key, code) != code:
                raise ValueError(f"{key} would take two sets, {links[key]} and {code}")
            links[key] = code
    named = set(links.values())
    sets = {code: entry for code, entry in sets.items() if code in named}
    catalogues = {code: entry for code, entry in CATALOGUES.items()
                  if any(s.get("catalogue_code") == code for s in sets.values())}
    return {"catalogues": catalogues, "sets": sets, "links": links}
