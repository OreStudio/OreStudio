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
"""The ore parameters of the analytics, in the order the run document writes them.

The text of each one is in the analytic_parameter_descriptions package, and the release
that introduced it is in analytic_parameter_releases.py. Every parameter needs a complete text.
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from analytic_parameter_catalogue import CATALOGUE  # noqa: E402
from analytic_parameter_descriptions import DESCRIPTIONS  # noqa: E402
from analytic_parameter_releases import INTRODUCED_IN, REMOVED_IN  # noqa: E402
from ore_parameter_seed import TEXT_KEYS  # noqa: E402


def rows():
    known = {(t, n) for t, n, _ in CATALOGUE}
    for analytic, parameters in DESCRIPTIONS.items():
        for name in parameters:
            if (analytic, name) not in known:
                raise ValueError(f"{analytic}.{name} has a text but is not in the catalogue")
    out = []
    for position, (analytic, name, domain) in enumerate(CATALOGUE, start=1):
        text = DESCRIPTIONS.get(analytic, {}).get(name, {})
        out.append({"scope": "analytic", "subtype": analytic, "name": name,
                    "position": position, "domain": domain,
                    "required": text.get("required", False),
                    "introduced_in": INTRODUCED_IN.get(f"{analytic}|{name}"),
                    "removed_in": REMOVED_IN.get(f"{analytic}|{name}"),
                    **{k: text.get(k) for k in TEXT_KEYS if k != "introduced_in"}})
    return out


def missing(all_rows):
    """The parameters with a text that is absent or blank, and the release that is not measured."""
    return [f"{r['subtype']}.{r['name']}" for r in all_rows
            if any(not str(r.get(k) or "").strip() for k in TEXT_KEYS)]
