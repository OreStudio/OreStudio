# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
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
set(files
    "domain_currency_mapper_tests.cpp"
    "domain_trade_mapper_tests.cpp"
    "hierarchy_hierarchy_builder_tests.cpp"
    "log_ore_log_parser_tests.cpp"
    "main.cpp"
    "market_fx_quote_convention_checker_tests.cpp"
    "market_market_data_parser_tests.cpp"
    "market_market_data_roundtrip_tests.cpp"
    "market_series_key_registry_tests.cpp"
    "planner_import_planner_tests.cpp"
    "scanner_directory_scanner_tests.cpp"
    "xml_bond_golden_roundtrip_tests.cpp"
    "xml_bond_mapper_roundtrip_tests.cpp"
    "xml_bond_option_golden_roundtrip_tests.cpp"
    "xml_bond_option_mapper_roundtrip_tests.cpp"
    "xml_calendaradjustment_roundtrip_tests.cpp"
    "xml_collateralbalances_roundtrip_tests.cpp"
    "xml_commodity_golden_roundtrip_tests.cpp"
    "xml_commodity_mapper_roundtrip_tests.cpp"
    "xml_composite_scripted_mapper_roundtrip_tests.cpp"
    "xml_conventions_roundtrip_tests.cpp"
    "xml_counterpartyinformation_roundtrip_tests.cpp"
    "xml_credit_golden_roundtrip_tests.cpp"
    "xml_credit_mapper_roundtrip_tests.cpp"
    "xml_creditsimulation_roundtrip_tests.cpp"
    "xml_crossassetmodel_roundtrip_tests.cpp"
    "xml_currency_config_tests.cpp"
    "xml_curveconfig_roundtrip_tests.cpp"
    "xml_equity_golden_roundtrip_tests.cpp"
    "xml_equity_mapper_roundtrip_tests.cpp"
    "xml_exotic_golden_roundtrip_tests.cpp"
    "xml_exporter_tests.cpp"
    "xml_file_io_tests.cpp"
    "xml_fx_convention_mapper_roundtrip_tests.cpp"
    "xml_fx_exotic_mapper_roundtrip_tests.cpp"
    "xml_fx_golden_roundtrip_tests.cpp"
    "xml_fx_mapper_roundtrip_tests.cpp"
    "xml_hybrid_golden_roundtrip_tests.cpp"
    "xml_importer_tests.cpp"
    "xml_inflation_golden_roundtrip_tests.cpp"
    "xml_ir_batch2_golden_roundtrip_tests.cpp"
    "xml_ir_golden_roundtrip_tests.cpp"
    "xml_ir_mapper_roundtrip_tests.cpp"
    "xml_nettingsetdefinitions_roundtrip_tests.cpp"
    "xml_ore_roundtrip_tests.cpp"
    "xml_portfolio_roundtrip_tests.cpp"
    "xml_pricingengines_roundtrip_tests.cpp"
    "xml_remaining_phases_mapper_roundtrip_tests.cpp"
    "xml_roundtrip_tests.cpp"
    "xml_scripted_golden_roundtrip_tests.cpp"
    "xml_sensitivityanalysis_roundtrip_tests.cpp"
    "xml_simulation_roundtrip_tests.cpp"
    "xml_stresstesting_roundtrip_tests.cpp"
    "xml_swaption_mapper_roundtrip_tests.cpp"
    "xml_todaysmarket_roundtrip_tests.cpp"
    "xml_trade_import_tests.cpp"
)
