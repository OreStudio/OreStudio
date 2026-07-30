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
    "domain/bond_instrument_mapper.cpp"
    "domain/calendar_adjustment_mapper.cpp"
    "domain/commodity_instrument_mapper.cpp"
    "domain/composite_instrument_mapper.cpp"
    "domain/conventions_mapper.cpp"
    "domain/credit_instrument_mapper.cpp"
    "domain/currency_mapper.cpp"
    "domain/domain.cpp"
    "domain/equity_instrument_mapper.cpp"
    "domain/fx_instrument_mapper.cpp"
    "domain/scripted_instrument_mapper.cpp"
    "domain/swap_instrument_mapper.cpp"
    "domain/trade_mapper.cpp"
    "hierarchy/ore_hierarchy_builder.cpp"
    "log/ore_log_parser.cpp"
    "market/fx_quote_convention_checker.cpp"
    "market/market_data_parser.cpp"
    "market/market_data_serializer.cpp"
    "market/series_key_registry.cpp"
    "planner/ore_import_planner.cpp"
    "scanner/ore_directory_scanner.cpp"
    "xml/exporter.cpp"
    "xml/importer.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/bond_instrument_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/calendar_adjustment_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/commodity_instrument_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/composite_instrument_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/conventions_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/credit_instrument_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/currency_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/domain.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/domain_xsd.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/equity_instrument_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/fx_instrument_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/payment_frequency_conversion.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/scripted_instrument_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/swap_instrument_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/domain/trade_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/hierarchy/import_node.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/hierarchy/ore_hierarchy_builder.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/log/ore_log_parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/fixing.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/fx_quote_convention_checker.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/market_data_parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/market_data_serializer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/market_datum.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/market/series_key_registry.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/ores.ore.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/import_choices.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/ore_import_plan.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/ore_import_planner.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/ore_import_result.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/planner/ore_instrument_error.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/scanner/ore_directory_scanner.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/scanner/scan_result.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/exporter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/importer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.core/xml/xml.hpp"
)
