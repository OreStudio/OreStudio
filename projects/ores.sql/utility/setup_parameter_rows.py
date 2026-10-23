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
"""The Setup ore parameters, one row for each engine setting, with the text a screen shows.

The texts are in setup_parameter_descriptions.py.
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ore_parameter_seed import TEXT_KEYS  # noqa: E402
from setup_parameter_descriptions import SETUP_PARAMETERS  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_setup_ore_parameters_populate.sql")


def rows():
    return [{**p, "scope": "Setup", "subtype": "", "position": position, "required": False}
            for position, p in enumerate(SETUP_PARAMETERS, start=1)]


def missing(all_rows):
    """The parameters with a text that is absent or blank."""
    return [f"{r['scope']}.{r['name']}" for r in all_rows
            if any(not str(r.get(k) or "").strip() for k in TEXT_KEYS)]
