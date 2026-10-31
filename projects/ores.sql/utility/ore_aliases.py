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
"""The renderings ORE accepts for one value, read from ORE's own reader.

ORE reads several spellings for one business day convention, day counter,
frequency, compounding rule and date rule, and converts each to the form it
holds. The tables that do the conversion live in the ORE reader,
projects/ores.ore/core/src/domain/conventions_fields.cpp, and this module reads
them so a value set can state the same knowledge as data.

One table, day_counter_spellings, runs the other way: it names the spelling an
export writes for a canonical day counter. A reader knows nothing about it, and
it is not a rendering a reader accepts in place of another.

The canonical form a set states is the form the store holds, which is not always
a form a document may write: ORE writes 'NEAREST' where the compiled table holds
'Nearest', so the table that converts a value on the way out still states a fact
the value set does not. That direction is left where it is until the export is
stated as data too.
"""

import os
import re

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
SOURCE = os.path.join(ROOT, "projects", "ores.ore", "core", "src", "domain",
                      "conventions_fields.cpp")

CODE_TABLE = re.compile(r"const code_table (\w+) = \{(.*?)\n\};", re.S)
SPELLING_TABLE = re.compile(
    r"const std::map<std::string, std::string, std::less<>> (\w+) = \{(.*?)\n\};", re.S)
ENTRY = re.compile(r'\{"((?:[^"\\]|\\.)*)", "((?:[^"\\]|\\.)*)"\}')

_cached = None


def _unescape(text):
    return text.replace('\\"', '"').replace("\\\\", "\\")


def tables():
    """Every conversion table, and the renderings each maps onto a canonical form."""
    global _cached
    if _cached is not None:
        return _cached
    with open(SOURCE, encoding="utf-8") as handle:
        source = handle.read()
    found = {}
    for name, body in CODE_TABLE.findall(source):
        found[name] = [(_unescape(spelling), _unescape(canonical))
                       for spelling, canonical in ENTRY.findall(body)]
    for name, body in SPELLING_TABLE.findall(source):
        found[name] = [(_unescape(spelling), _unescape(canonical))
                       for spelling, canonical in ENTRY.findall(body)]
    if len(found) < 5:
        raise ValueError(f"{SOURCE} holds {len(found)} conversion tables, and ORE's "
                         "reader holds at least five; the table names likely changed")
    _cached = found
    return found


def canonical_of(table, rendering):
    """The canonical form of one rendering, or None when the table does not know it."""
    for spelling, canonical in tables().get(table, []):
        if spelling == rendering:
            return canonical
    return None


def canonical_forms(table):
    """The canonical forms a table names, in the order it first names each."""
    forms = []
    for _, canonical in tables().get(table, []):
        form = canonical
        if form not in forms:
            forms.append(form)
    return forms


def renderings(table):
    """Every rendering a table knows, the canonical forms included, in source order."""
    return [spelling for spelling, _ in tables().get(table, [])]


def canonical_map(table, parser_values=None):
    """The canonical form of every rendering, from the table that converts them.

    The table is the authority for which renderings mean one fact, because it is
    the table the store's readers use. The parser that accepts the renderings is
    the authority for which ones a document may write. Neither is inferred from
    the other: a rendering one names and the other does not is a document ORE
    reads and this store refuses, or a rule the reader cannot reach, and both stop
    the run.

    The form a configuration holds is one of the renderings, so a document may
    write it. A form no document writes would need a second table to leave the
    store, and the fact would be stated twice.
    """
    known = {}
    for spelling, canonical in tables().get(table, []):
        known[spelling] = canonical
    if parser_values is None:
        return known
    read = {item["value"] for item in parser_values}
    unstated = sorted(read - set(known))
    if unstated:
        raise ValueError(
            f"ORE reads {', '.join(repr(value) for value in unstated)} and the "
            f"{table} table states no canonical form for "
            f"{'them' if len(unstated) > 1 else 'it'}; the store would refuse a "
            "document the engine accepts")
    unreachable = sorted(set(known) - read)
    if unreachable:
        raise ValueError(
            f"the {table} table maps {', '.join(repr(value) for value in unreachable)} "
            "and ORE's reader accepts no such rendering; the row states a rule "
            "nothing can apply")
    unwritable = sorted({form for form in known.values() if form not in read})
    if unwritable:
        raise ValueError(
            f"the {table} table makes {', '.join(repr(form) for form in unwritable)} "
            "the form a configuration holds, and no document may write it; the "
            "writer would need a second table, so choose a form the schema states")
    return known


# The parser whose list names one conversion table's renderings. ORE's reader
# states each rendering once, in the table that converts it, and the parser that
# accepts it states the word it means. The two must agree, and a rendering only
# one of them names is a gap this module reports rather than guesses at.
PARSER_TABLE = {
    "parseBusinessDayConvention": "business_day_conventions",
    "parseDayCounter": "day_counters",
    "parseFrequency": "frequencies",
    "parseCompounding": "compoundings",
    "parseDateGenerationRule": "date_rules",
}

WORDS_LABEL = "words"
VALUE_LABEL = "value"

# What one family is called, and how a row of its set reads.
SETS = {
    "business_day_conventions": {
        "name": "Business day conventions",
        "description": "The business day conventions, as the form a configuration holds "
                       "and every other rendering ORE's reader accepts.",
        "label": WORDS_LABEL,
    },
    "day_counters": {
        "name": "Day counters",
        "description": "The day counters, as the form a configuration holds and every "
                       "other rendering ORE's reader accepts.",
        "label": VALUE_LABEL,
    },
    "frequencies": {
        "name": "Frequencies",
        "description": "The frequencies, as the form a configuration holds and every "
                       "other rendering ORE's reader accepts.",
        "label": WORDS_LABEL,
    },
    "compoundings": {
        "name": "Compoundings",
        "description": "The compounding rules, as the form a configuration holds and "
                       "every other rendering ORE's reader accepts.",
        "label": WORDS_LABEL,
    },
    "date_rules": {
        "name": "Date generation rules",
        "description": "The date generation rules, as the form a configuration holds and "
                       "every other rendering ORE's reader accepts.",
        "label": WORDS_LABEL,
    },
}


def _word_of(name):
    parts = re.findall(r"[A-Z]+(?![a-z])|[A-Z]?[a-z]+|[0-9]+", name) or [name]
    text = " ".join(p if (len(p) > 1 and p.isupper()) else p.lower() for p in parts)
    return text[0].upper() + text[1:]


def rendering_rows(table, parser_values, label_of):
    """The value rows a set states for one conversion table, canonical forms first.

    Every rendering the table or the parser names is a row. A row whose value is
    its own canonical form states no canonical value: it *is* the form a
    configuration holds. Every other row names that form, so a reader tells a
    choice from a rendering of one without a second table.
    """
    known = canonical_map(table, parser_values)
    order, groups = [], {}
    for rendering, canonical in known.items():
        if canonical not in groups:
            order.append(canonical)
            groups[canonical] = [canonical]
        if rendering != canonical and rendering not in groups[canonical]:
            groups[canonical].append(rendering)
    rows = []
    for canonical in order:
        members = groups[canonical]
        label = _word_of(canonical) if label_of == WORDS_LABEL else canonical
        rows.append({
            "value": canonical,
            "label": label,
            "description": f"{label} is the form a configuration holds.",
            "order": len(rows),
        })
        for rendering in members[1:]:
            rows.append({
                "value": rendering,
                "label": rendering,
                "description": f"{rendering} is another rendering of {label}, "
                               "which is the form a configuration holds.",
                "order": len(rows),
                "canonical_value": canonical,
            })
    return rows


def parser_for(table):
    """The ORE parser whose map names one family's renderings."""
    for parser, name in PARSER_TABLE.items():
        if name == table:
            return parser
    raise ValueError(f"{table}: no ORE parser names the renderings of this family")


def rendered_set(table, parsers):
    """The value set one family states, from ORE's table and the parser's map."""
    found = SETS[table]
    return {
        "name": found["name"],
        "description": found["description"],
        "source_kind": "enumerated",
        "is_ordered": False,
        "values": rendering_rows(table,
                                 parsers[parser_for(table)]["values"],
                                 found["label"]),
    }


def unstated_canonical_forms(table, stated):
    """The canonical forms of a table that a set states no value for.

    A table can name a canonical form whose row it never lists, because the
    reader recognises it as a rendering of another. A set that omitted it would
    hold a configuration no set row accepts, so the omission stops the run.
    """
    return [form for form in canonical_forms(table) if form not in stated]


def compare(set_values, table):
    """What a set states against what the table states, as problems to print.

    The check runs while both statements exist. It names a rendering the set
    lost, a rendering the set drew as a value of its own when the table says it
    means another, and a canonical form the set never states.
    """
    problems = []
    stated = {value["value"]: value.get("canonical_value") for value in set_values}
    for form in canonical_forms(table):
        if form not in stated:
            problems.append(f"the set states no value for the canonical form {form!r}")
    for spelling, canonical in tables().get(table, []):
        form = canonical
        if spelling not in stated:
            problems.append(f"the set states no value for the rendering {spelling!r}")
        elif spelling != form and stated[spelling] != form:
            problems.append(f"the set states {spelling!r} as "
                            f"{stated[spelling]!r}, and ORE reads it as {form!r}")
    for value, canonical in stated.items():
        if canonical is not None and canonical not in stated:
            problems.append(f"the set states {value!r} as {canonical!r}, which it "
                            "holds no value for")
    return problems
