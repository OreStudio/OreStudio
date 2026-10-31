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
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import ore_schema_types as schema_types  # noqa: E402

TERMS = os.path.join(HERE, "curve_terms.json")
CURATION = os.path.join(HERE, "curve_value_set_curation.json")

CURATION_KEYS = ("link", "reason", "source", "source_ref", "curator", "date")
SOURCES = ("ore_guide", "ore_code", "ore_xsd", "authored", "seed")

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

# The vocabularies the schema declares, and the value set each element name takes.
# They are declared as data, one row per element name, so a new vocabulary is a
# row in the declaration and a seeded table row, not a change to this generator.
VOCABULARIES = os.path.join(HERE, "ore_parameter_vocabularies.json")
SCHEMA = "curveconfig.xsd"


def declared_vocabularies():
    """The declared vocabularies, by code, as the declaration file states them."""
    with open(VOCABULARIES) as f:
        return json.load(f)


def vocabularies(schema=SCHEMA):
    """The element names one schema declares, and the value set each takes.

    A declaration for a schema this generator does not apply is a loud failure
    rather than a silently dropped row: the rows are the declaration, and a row
    nothing applies is a rule the repository believes and the tree does not.
    """
    declared = declared_vocabularies()
    other = {code for code, entry in declared.items() if entry["schema"] != schema}
    if other:
        raise ValueError(
            f"ore_parameter_vocabularies.json declares {', '.join(sorted(other))} "
            f"for a schema no generator applies; add the rule that applies it or "
            f"drop the row")
    return {entry["element"]: entry["value_set"] for entry in declared.values()}


def catalogue_of(value_set):
    """The catalogue a declared value set draws on, or None for a text set.

    A declaration names a catalogue only when reference data holds the spellings
    ORE writes. A vocabulary whose registry holds words where ORE writes its own
    code states no catalogue, and its set is text until the registry carries the
    spelling the document does.
    """
    for entry in declared_vocabularies().values():
        if entry["value_set"] != value_set:
            continue
        catalogue = entry.get("catalogue")
        if catalogue is None:
            return None
        if catalogue not in CATALOGUES:
            raise ValueError(f"{value_set}: the declaration names {catalogue}, "
                             "which is not a catalogue this generator knows")
        return catalogue
    raise ValueError(f"{value_set}: the declaration names a set no row backs")


# The element names the schema declares, and the value set each takes. The name
# says what the map holds: a value set, not a catalogue.
VALUE_SET_OF = vocabularies()

# A list leaf whose items ORE reads as strings, and the kind one item has. The
# extractor sees the read and not what parses an item, so a person states it here
# and names the place the ORE code proves it.
LIST_ITEM = {
    "Strikes": {
        "kind": "number_list",
        "source": "ore_code",
        "source_ref": "OREData/ored/configuration/reportconfig.cpp:68",
        "reason": "The report grid beside the curve parses its strike list as a "
                  "real, and both leaves name the same set of strikes, so one item "
                  "is a number.",
    },
    "CapStrikes": {
        "kind": "number_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/parameterisation/curveconfig.tex:50",
        "reason": "The guide states the strikes for which cap prices are quoted, so "
                  "one item is a number.",
    },
    "FloorStrikes": {
        "kind": "number_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/parameterisation/curveconfig.tex:52",
        "reason": "The guide states the strikes for which floor prices are quoted, "
                  "so one item is a number.",
    },
    "DetachmentPoints": {
        "kind": "number_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/parameterisation/curveconfig.tex:38",
        "reason": "The guide states any positive number less than one, so one item "
                  "is a number.",
    },
    "SmileSpreads": {
        "kind": "number_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/parameterisation/curveconfig.tex:111",
        "reason": "The guide states strikes expressed as strike spreads relative to "
                  "the ATM strike, so one item is a number.",
    },
    "Tenors": {
        "kind": "period_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/curve_configurations/capfloorvolatility.tex:94",
        "reason": "The guide states a comma separated list of valid tenor strings, "
                  "and a tenor is a period.",
    },
    "AtmTenors": {
        "kind": "period_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/curve_configurations/capfloorvolatility.tex:106",
        "reason": "The guide states a comma separated list of valid tenor strings, "
                  "and a tenor is a period.",
    },
    "Terms": {
        "kind": "period_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/parameterisation/curveconfig.tex:37",
        "reason": "The guide states a comma separated list of tenors, and a tenor "
                  "is a period.",
    },
    "SmileOptionTenors": {
        "kind": "period_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/parameterisation/curveconfig.tex:109",
        "reason": "The guide states option expiries in period form, so one item is "
                  "a period.",
    },
    "OptionTenors": {
        "kind": "period_list",
        "source": "ore_code",
        "source_ref": "OREData/ored/configuration/correlationcurveconfig.cpp:148",
        "reason": "The option tenors are the expiries of the volatility surface, "
                  "and every family reads an option tenor as a period, so one item "
                  "is a period.",
    },
    "SwapTenors": {
        "kind": "period_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/parameterisation/curveconfig.tex:135",
        "reason": "The guide states the underlying Swap term in period form, so one "
                  "item is a period.",
    },
    "SmileSwapTenors": {
        "kind": "period_list",
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/parameterisation/curveconfig.tex:142",
        "reason": "The guide states the underlying Swap term in period form for the "
                  "smile section, so one item is a period.",
    },
    "BondTenors": {
        "kind": "period_list",
        "source": "ore_code",
        "source_ref": "OREData/ored/configuration/yieldvolcurveconfig.hpp:42",
        "reason": "The yield volatility class builds this leaf from the same read "
                  "the swaption class builds SwapTenors from, with Bond as the "
                  "underlying label, and a bond tenor is a period.",
    },
    "SmileBondTenors": {
        "kind": "period_list",
        "source": "ore_code",
        "source_ref": "OREData/ored/configuration/yieldvolcurveconfig.hpp:42",
        "reason": "The yield volatility class builds this leaf from the same read "
                  "the swaption class builds SmileSwapTenors from, and a bond tenor "
                  "is a period.",
    },
    "Deltas": {
        "kind": None,
        "source": "ore_code",
        "source_ref": "OREData/ored/configuration/fxvolcurveconfig.cpp:167",
        "reason": "ORE writes a delta as 25D, which is neither a number nor a "
                  "period, and this tree states no rule for one yet.",
    },
    "Expiries": {
        "kind": None,
        "source": "ore_guide",
        "source_ref": "Docs/UserGuide/parameterisation/curveconfig.tex:128",
        "reason": "The guide states expiry tenors and or expiry dates, and a single "
                  "wildcard *, so no one rule holds of an item yet.",
    },
    "PillarDates": {
        "kind": None,
        "source": "ore_code",
        "source_ref": "OREData/ored/configuration/reportconfig.cpp:82",
        "reason": "A pillar date is a date, and this tree states no rule for one "
                  "yet.",
    },
}

TYPE_SETS = {
    "boolean": "curve.flag",
    "double": "curve.number",
    "integer": "curve.integer",
    "period": "curve.period",
    "number_list": "curve.number_list",
    "period_list": "curve.period_list",
    "list": "curve.list",
}

# The kind the ORE schema declares, as this family's TYPE_SETS names it.
SCHEMA_TYPE_SETS = {"integer": "integer", "number": "double",
                    "boolean": "boolean", "period": "period"}

PERIOD_PATTERN = "^[+-]?([0-9]+[DdWwMmYy])+$"

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
        "pattern": PERIOD_PATTERN,
        "is_ordered": False,
    },
    "curve.number_list": {
        "name": "Numbers",
        "description": "A comma separated list of numbers, which the ORE code reads "
                       "into one vector.",
        "source_kind": "number",
        "cardinality": "many",
        "is_ordered": True,
    },
    "curve.period_list": {
        "name": "Periods",
        "description": "A comma separated list of periods such as 1Y,2Y,5Y, which "
                       "the ORE code reads into one vector.",
        "source_kind": "regex",
        "pattern": PERIOD_PATTERN,
        "cardinality": "many",
        "is_ordered": True,
    },
    "curve.list": {
        "name": "List of text",
        "description": "A comma separated list whose items no value set states yet. "
                       "ORE reads it into one vector, so the leaf holds many values "
                       "and the tree cannot yet say what one may be.",
        "source_kind": "text",
        "cardinality": "many",
        "is_ordered": True,
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


_DRAWN = None


def is_drawn(term):
    """Whether the projection carries this leaf, so a set generated for it is used.

    The schema enumerates a type for many leaves the projection does not carry, and
    a set nothing names and no row carries is dead data. A set the schema states is
    therefore generated only for a leaf a column actually holds.
    """
    global _DRAWN
    if _DRAWN is None:
        import generate_curve_column_guards as guards
        _DRAWN = {row["xpath"] for row in guards.rows()}
    return "/CurveConfiguration/" + "/".join(term["path"]) in _DRAWN


def set_of(term, curated, parsers, section):
    """The set one leaf takes, and the set's row when this module generates it.

    A set the parser or the schema states is built here, because its values come
    from a source rather than from a person: the parser's map, or the values a
    schema simple type enumerates. The row travels with the code so the caller
    seeds it without deriving the same answer a second time.
    """
    name = term["name"]
    entry = curated_for(curated, section, name)
    if entry:
        link = entry["link"]
        if link.startswith("refdata."):
            return "curve." + link.split(".", 1)[1], None
        if link not in parsers:
            raise ValueError(f"{name}: the curation links to {link}, which no ORE source "
                             "states a list for; correct the entry or drop it")
        return parser_set_code(link), parser_set(link, parsers[link])
    if name in VALUE_SET_OF:
        value_set = VALUE_SET_OF[name]
        return ("text" if catalogue_of(value_set) is None else value_set), None
    if term["type"] in TYPE_SETS:
        if term["type"] == "list":
            stated = item_set_of(term)
            if stated:
                return stated, None
        return TYPE_SETS[term["type"]], None
    stated = schema_rule(term)
    if stated == "enumerated":
        return schema_enum_set(term) if is_drawn(term) else ("text", None)
    if stated:
        return TYPE_SETS[SCHEMA_TYPE_SETS[stated]], None
    return "text", None


def schema_enum_set(term):
    """The enumerated set the ORE schema states for one leaf's type.

    The schema lists the values a simple type accepts, so the rule is a set of
    them rather than a shape one value must have, and a screen draws a combo from
    it without a person writing the list twice.
    """
    _, type_name = schema_types.rule(SCHEMA, "/CurveConfiguration/" + "/".join(term["path"]))
    values, file, line = schema_types.enumeration(type_name)
    # The schema lists the empty string where a leaf may be left unset. The guard
    # treats a value that is not written as absent rather than as a choice, so the
    # set states the choices and the unset case stays an empty field.
    values = [value for value in values if value != ""]
    code = "curve." + re.sub(r"(?<!^)(?=[A-Z])", "_", type_name).lower()
    return code, {
        "name": words(type_name),
        "description": f"The values the ORE schema states for {type_name}, from "
                       f"external/ore/xsd/{file}:{line}.",
        "source_kind": "enumerated",
        "is_ordered": False,
        "values": [{"value": value, "label": value,
                    "description": f"{value} is one of the values the ORE schema "
                                   f"states for {type_name}.",
                    "order": order}
                   for order, value in enumerate(values)],
    }


def schema_rule(term):
    """The kind the ORE schema declares for a leaf the parser read as a string.

    The parser says what the engine accepts and is the first authority. Where it
    reads a leaf as a plain string without saying more, the schema still declares
    a type, and a leaf declared xs:integer is not free text.
    """
    found = schema_types.rule(SCHEMA, "/CurveConfiguration/" + "/".join(term["path"]))
    return found[0] if found else None


def item_set_of(term):
    """The set of one item of a list leaf ORE reads as strings, if a person stated it.

    One leaf of a name may be read into a vector of strings while another leaf of
    the same name is read with a template that states its item, and the setting
    link cannot hold both: its key is the section and the name. Where the two
    disagree the proven item wins for the name, because they name the same set of
    values and the unproven read is the extractor's gap rather than a second rule.
    """
    stated = LIST_ITEM.get(term["name"])
    return TYPE_SETS[stated["kind"]] if stated and stated["kind"] else None


def data():
    """The catalogues, the sets and the links the curve leaves add.

    Every leaf the ORE source reads takes a set, whether or not the parameter
    contract documents it. The rule follows from the parser the ORE code reads
    the leaf with, and it holds of the leaf whether or not a person has written
    a description of it.

    A link is a different statement: it says which set governs a *setting*, keyed
    by section and name. Two leaves of one section that share a name can be read
    differently, as the equity volatility curve's Expiries are read as strings and
    the report grid's beside them as periods. The link follows the leaf the
    projection draws, because that is the leaf a screen states; when two drawn
    leaves disagree the key cannot hold both, so it is returned in =conflicted=
    instead of guessed, and the map states each leaf's own rule.
    """
    with open(TERMS) as f:
        extracted = json.load(f)
    parsers = extracted["parsers"]
    curated = curation()
    sets = dict(OWN_SETS)
    for value_set in set(VALUE_SET_OF.values()):
        catalogue = catalogue_of(value_set)
        if catalogue is None:
            continue
        sets[value_set] = {
            "name": CATALOGUES[catalogue]["name"],
            "description": f"One of the {CATALOGUES[catalogue]['name'].lower()} "
                           "that reference data holds.",
            "source_kind": "catalogue",
            "catalogue_code": catalogue,
            "is_ordered": False,
        }
    import generate_curve_column_guards as curves

    projected = {row["xpath"] for row in curves.rows()}
    codes, built = {}, set()
    for section, term in terms():
        code, generated = set_of(term, curated, parsers, section)
        if generated:
            built.add(code)
            if code not in sets:
                sets[code] = generated
        key = f"{section}|{term['name']}"
        address = "/CurveConfiguration/" + "/".join(term["path"])
        found = codes.setdefault(key, {})
        found.setdefault(code, set()).add(address in projected)
    links, conflicted = {}, {}
    for key, found in codes.items():
        drawn = {code for code, shown in found.items() if True in shown}
        if len(drawn) == 1:
            links[key] = next(iter(drawn))
        elif not drawn and len(found) == 1:
            links[key] = next(iter(found))
        elif len(found) > 1:
            conflicted[key] = sorted(found)
    for key in curated:
        section, _, name = key.partition("|")
        if not any((section in ("", "*") or section == sec) and nm == name
                   for sec, nm in ((s, t["name"]) for s, t in terms())):
            raise ValueError(f"curation entry {key} matches no curve leaf the ORE "
                             "source reads; correct the key or drop the entry")
    named = {code for found in codes.values() for code in found}
    sets = {code: entry for code, entry in sets.items() if code in named}
    catalogues = {code: entry for code, entry in CATALOGUES.items()
                  if any(s.get("catalogue_code") == code for s in sets.values())}
    return {"catalogues": catalogues, "sets": sets, "links": links,
            "conflicted": conflicted, "generated": built}


def set_of_leaf(term, section=None):
    """The set code of one curve leaf, resolved by the leaf and not by its name.

    A caller that holds a term keys the rule by the leaf it is about. The link
    table cannot answer that question, because its key is the section and the name
    and two leaves of one name can differ.
    """
    with open(TERMS) as f:
        parsers = json.load(f)["parsers"]
    return set_of(term, curation(), parsers, section or term["path"][0])[0]


def catalogue_scopes():
    """The sections each catalogue name is matched in, so a name that comes to
    mean something else in a new family is seen rather than assumed."""
    with open(TERMS) as f:
        extracted = json.load(f)
    scopes = {}
    for section, body in extracted["sections"].items():
        for term in body["terms"]:
            if term["name"] in VALUE_SET_OF:
                scopes.setdefault(term["name"], set()).add(section)
    return {name: sorted(found) for name, found in sorted(scopes.items())}


def text_of():
    """The leaves that fall to text, so the coverage can be read."""
    with open(TERMS) as f:
        extracted = json.load(f)
    parsers = extracted["parsers"]
    curated = curation()
    return sorted({f"{section}|{term['name']}" for section, term in terms()
                   if set_of(term, curated, parsers, section)[0] == "text"})
