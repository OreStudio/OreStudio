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
# AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
# Template: cmake_component_files_src.mustache
# To modify, update the template and regenerate.
set(files
    "domain/bond_instrument_projection.cpp"
    "domain/commodity_instrument_projection.cpp"
    "domain/composite_instrument_projection.cpp"
    "domain/conventions_diff.cpp"
    "domain/conventions_mapper.cpp"
    "domain/credit_instrument_projection.cpp"
    "domain/credit_simulation_projection.cpp"
    "domain/curve_configuration_projection.cpp"
    "domain/domain.cpp"
    "domain/equity_instrument_projection.cpp"
    "domain/fx_instrument_projection.cpp"
    "domain/portfolio_projection.cpp"
    "domain/pricing_engine_projection.cpp"
    "domain/rate_instrument_projection.cpp"
    "domain/run_document_projection.cpp"
    "domain/scripted_instrument_projection.cpp"
    "domain/todays_market_mapper.cpp"
    "hierarchy/ore_hierarchy_builder.cpp"
    "log/engine_run_verdict.cpp"
    "log/ore_log_parser.cpp"
    "market/fx_quote_convention_checker.cpp"
    "market/market_data_parser.cpp"
    "market/market_data_serializer.cpp"
    "planner/ore_import_planner.cpp"
    "scanner/ore_directory_scanner.cpp"
    "store/run_store.cpp"
    "xml/binding.cpp"
    "xml/document.cpp"
    "xml/document_kind.cpp"
    "xml/emitter.cpp"
    "xml/exporter.cpp"
    "xml/generated/calendar_adjustments.cpp"
    "xml/generated/conventions.cpp"
    "xml/generated/counterparty_information.cpp"
    "xml/generated/credit_simulation.cpp"
    "xml/generated/currency_config.cpp"
    "xml/generated/curve_configuration.cpp"
    "xml/generated/ibor_fallback_config.cpp"
    "xml/generated/netting_set_definitions.cpp"
    "xml/generated/ore.cpp"
    "xml/generated/portfolio.cpp"
    "xml/generated/pricing_engines.cpp"
    "xml/generated/stress_testing.cpp"
    "xml/generated/todays_market.cpp"
    "xml/importer.cpp"
    "xml/record.cpp"
    "xml/record_diff.cpp"
    "xml/roundtrip.cpp"
    "xml/roundtrip_harness.cpp"
    "xml/scanner.cpp"
    "xml/value_fingerprint.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/conventions_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/credit_simulation_grid.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/credit_simulation_projection.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/curve_configuration_projection.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/domain.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/domain_xsd.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/ore_code_tables.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/payment_frequency_conversion.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/portfolio_instrument_projection.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/portfolio_projection.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/pricing_engine_projection.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/run_document_projection.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/todays_market_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/hierarchy/import_node.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/hierarchy/ore_hierarchy_builder.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/log/engine_run_verdict.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/log/ore_log_parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/fixing.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/fx_quote_convention_checker.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/market_data_parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/market_data_serializer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/market_datum.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/ores.ore.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/import_choices.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/ore_import_plan.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/ore_import_planner.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/ore_import_result.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/ore_instrument_error.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/scanner/ore_directory_scanner.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/scanner/scan_result.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/store/run_store.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/binding.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/document.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/document_kind.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/emitter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/exporter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/calendar_adjustments.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/conventions.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/counterparty_information.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/credit_simulation.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/currency_config.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/curve_configuration.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/ibor_fallback_config.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/netting_set_definitions.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/ore.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/portfolio.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/pricing_engines.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/stress_testing.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/generated/todays_market.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/importer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/lexical.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/projection.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/record.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/record_diff.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/roundtrip.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/roundtrip_harness.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/scanner.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/trade_import_item.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/value_fingerprint.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/xml.hpp"
)
