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
"""The value sets of the engine run settings, and the link from each setting.

The engine run settings screen draws the settings in the =Setup= scope of the
ORE document, so a setting's allowed values belong to that scope and to its ORE
name. The sets are curated in the engine settings groups file, which states the
rule once: a flag takes the two switch values, a whole number takes a bounded
integer, and a choice takes the words the run accepts.

The groups and their settings name a set; they state no control of their own.
The group settings table still carried the control when this module was written,
and the groups generator derives those columns from the set until the model
drops them.
"""

import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..",
                                "ores.seeder", "datasets", "ore_pricing_engines"))
import curation  # noqa: E402

SCOPE = "Setup"

# The control a set's source kind gives the screen, in the words the group
# settings table used before the set stated it.
CONTROL_OF = {
    "boolean": "flag",
    "integer": "whole",
    "number": "whole",
    "range": "whole",
    "enumerated": "choice",
}

# The source kind a control needs, so a set that cannot draw the control the
# screen needs stops the generator instead of drawing the wrong field.
KIND_OF_CONTROL = {
    "flag": ("boolean",),
    "whole": ("integer", "number", "range"),
    "choice": ("enumerated",),
}


def groups():
    """The curated groups, with the sets they name."""
    return curation.CURATED.engine_settings_groups


def settings():
    """Every setting of every group, in the order the screen draws them."""
    return [setting for group in groups()["groups"] for setting in group.get("settings", [])]


def control_of(set_code):
    """The control the set states: (kind, options, minimum, maximum)."""
    entry = groups()["value_sets"][set_code]
    kind = CONTROL_OF[entry["source_kind"]]
    options = [value["value"] for value in entry.get("values", [])]
    return kind, options, entry.get("minimum"), entry.get("maximum")


def problems():
    """What the curation states that the sets cannot hold."""
    found = []
    sets = groups()["value_sets"]
    named = set()
    for setting in settings():
        code = setting.get("value_set", "")
        if code not in sets:
            found.append(f"Setup.{setting.get('ore', '')}: names the set {code!r}, "
                         "which the file does not state")
            continue
        named.add(code)
        if setting.get("options") or setting.get("kind") or setting.get("minimum") is not None \
                or setting.get("maximum") is not None:
            found.append(f"Setup.{setting.get('ore', '')}: states a control beside its set; "
                         "the set is the control")
    for code in sets:
        if code not in named:
            found.append(f"{code}: no setting names it")
    return found


def data():
    """The sets and the links, in the shape the value set generator writes."""
    sets = {}
    links = {}
    for code, entry in groups()["value_sets"].items():
        sets[code] = {
            "name": entry["name"],
            "description": entry["description"],
            "source_kind": entry["source_kind"],
            "is_ordered": entry.get("is_ordered", False),
            "values": entry.get("values", []),
        }
        for key in ("catalogue_code", "minimum", "maximum", "pattern"):
            if entry.get(key) is not None:
                sets[code][key] = entry[key]
    for setting in settings():
        links[f"{SCOPE}|{setting['ore']}"] = setting["value_set"]
    return {"catalogues": {}, "sets": sets, "links": links}
