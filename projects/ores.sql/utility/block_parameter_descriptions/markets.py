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
"""The Markets parameters: which market configuration each step of a run uses."""

from analytic_parameter_descriptions._entry import entry

CONFIGURATION = "The name of a market configuration of the today's market file, for example collateral_eur."

ENTRIES = {
    "pricing": entry(
        "Pricing market",
        "A market configuration name.",
        "Names the market configuration that prices the portfolio: the NPV, the cash flows and the pricing step of the other "
        "analytics. The configuration is defined in the today's market file. For example, collateral_eur discounts with the "
        "curves of euro collateral.",
        "default. ORE uses the configuration named default.", CONFIGURATION),
    "simulation": entry(
        "Simulation market",
        "A market configuration name.",
        "Names the market configuration that the simulation uses to project the market. A cross currency adjusted discount "
        "curve is the usual choice, because it prices FX products correctly. For example, collateral_eur.",
        "default. ORE uses the configuration named default.", CONFIGURATION),
    "lgmcalibration": entry(
        "Interest rate calibration market",
        "A market configuration name.",
        "Names the market configuration that calibrates the interest rate models of the simulation, the LGM models. "
        "Local OIS discounting is the usual choice. For example, collateral_inccy discounts each currency with its own OIS curve.",
        "default. ORE uses the configuration named default.", CONFIGURATION),
    "fxcalibration": entry(
        "FX calibration market",
        "A market configuration name.",
        "Names the market configuration that calibrates the FX models of the simulation. For example, collateral_eur.",
        "default. ORE uses the configuration named default.", CONFIGURATION),
    "eqcalibration": entry(
        "Equity calibration market",
        "A market configuration name.",
        "Names the market configuration that calibrates the equity models of the simulation. For example, collateral_inccy.",
        "default. ORE uses the configuration named default.", CONFIGURATION),
    "infcalibration": entry(
        "Inflation calibration market",
        "A market configuration name.",
        "Names the market configuration that calibrates the inflation models of the simulation. For example, collateral_inccy.",
        "default. ORE uses the configuration named default.", CONFIGURATION),
    "crcalibration": entry(
        "Credit calibration market",
        "A market configuration name.",
        "Names the market configuration that calibrates the credit models of the simulation. For example, collateral_inccy.",
        "default. ORE uses the configuration named default.", CONFIGURATION),
    "sensitivity": entry(
        "Sensitivity market",
        "A market configuration name.",
        "Names the market configuration that the sensitivity analysis uses to build the market it shifts. For example, "
        "collateral_eur.",
        "default. ORE uses the configuration named default.", CONFIGURATION),
    "bondpricing": entry(
        "Bond pricing market",
        "A market configuration name. No release of ORE reads it.",
        "No release of the ORE sources reads this parameter. Some example documents carry it to pick a market configuration "
        "for bonds. ORE ignores it, so bonds use the pricing market. Do not rely on it.",
        "None. ORE ignores the parameter.", CONFIGURATION),
}
