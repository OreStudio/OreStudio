# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(name "ores.ore.core")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

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

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_ORE_CORE_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

if(MSVC)
    target_compile_options(${lib_target_name} PUBLIC /bigobj)
endif()

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.database.lib
        ores.refdata.api.lib
        ores.trading.api.lib
    PRIVATE
        ores.dq.api.lib
        ores.logging.lib
        ores.platform.lib
        ores.utility.lib
        Boost::boost)
# Note: reflectcpp comes transitively through ores.platform.lib (PUBLIC)
# Do not add it explicitly to avoid duplicate library warnings on macOS.

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
