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
"""The codes a coded script input accepts, read from the library comment's line.

A script reads some choices as numbers and states their meaning only in prose:
"1 for DownIn, 2 for UpIn", "3=DownOut, 4=UpOut", "+1 for call, -1 for put". A
code is a number followed by "for" or "=", and its label is the words up to the
next code, a comma or a semicolon, with what is in brackets, a leading article, a
trailing "or", "and" or "position", and the words every label of the input ends
with removed: "1 for a long, -1 for a short position" gives long and short, and
"1 for American KO style, -1 for European KO style" gives American and European.
An input whose line states fewer than two distinct codes is not coded. The first
statement of a code wins, so a later "should be 1 for pure KO" does not relabel it;
a later statement that gives the code another label is reported as a conflict.

Where the prose is wrong or reads badly, script_input_code_corrections.json, beside
this file, states the codes of the input with its reason, and the reading is not
used.
"""

import json
import re
from pathlib import Path

CORRECTIONS = Path(__file__).resolve().parent / "script_input_code_corrections.json"

MARKER = re.compile(r"(?<![\w.])([+-]?\d+)\s*(?:=|\bfor\b)\s*")
ARTICLES = ("a", "an", "the")
TRAILING = ("or", "and", "position", "positions")


def corrections():
    entries = json.loads(CORRECTIONS.read_text(encoding="utf-8"))
    for key, entry in entries.items():
        if not entry.get("reason") or not entry.get("codes"):
            raise ValueError(f"script input code correction {key} has no reason or no codes")
    return entries


def phrase(text):
    depth, out = 0, []
    for ch in text:
        if ch in "([":
            depth += 1
        elif ch in ")]":
            depth = max(0, depth - 1)
        elif ch in ",;" and depth == 0:
            break
        elif depth == 0:
            out.append(ch)
    words = "".join(out).split()
    if words and words[0].lower() in ARTICLES:
        words = words[1:]
    while words and words[-1].lower() in TRAILING:
        words = words[:-1]
    return " ".join(words)


def statements(description):
    """Every (code, label) a comment line states, in order, repeats included."""
    found = list(MARKER.finditer(description or ""))
    out = []
    for i, match in enumerate(found):
        end = found[i + 1].start() if i + 1 < len(found) else len(description)
        label = phrase(description[match.end():end])
        if label and label[0].isalpha():
            out.append((int(match.group(1)), label))
    return out


def conflicts(description):
    """The codes a comment line gives two different labels."""
    first, out = {}, []
    for code, label in statements(description):
        if code in first and first[code] != label and code not in out:
            out.append(code)
        first.setdefault(code, label)
    return out


def read(description):
    """The (code, label) pairs a comment line states, or none."""
    pairs, seen = [], set()
    for code, label in statements(description):
        if code in seen:
            continue
        seen.add(code)
        pairs.append([code, label])
    if len(pairs) < 2:
        return []
    split = [p[1].split() for p in pairs]
    while all(len(words) > 1 for words in split) and len({w[-1] for w in split}) == 1:
        split = [w[:-1] for w in split]
    return [[code, " ".join(words)] for (code, _), words in zip(pairs, split)]


def codes(script, input_name, data_type, description, fixes):
    """The codes of one input: its correction's, else what its comment states."""
    fix = fixes.get(f"{script}/{input_name}")
    if fix:
        return [list(pair) for pair in fix["codes"]]
    if data_type != "Number":
        return []
    return read(description)
