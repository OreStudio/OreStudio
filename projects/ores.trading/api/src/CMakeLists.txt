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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(name "ores.trading.api")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "domain/activity_type_json_io.cpp"
    "domain/activity_type_table.cpp"
    "domain/activity_type_table_io.cpp"
    "domain/balance_guaranteed_swap_instrument_json_io.cpp"
    "domain/balance_guaranteed_swap_instrument_table.cpp"
    "domain/balance_guaranteed_swap_instrument_table_io.cpp"
    "domain/bond_instrument_json_io.cpp"
    "domain/bond_instrument_table.cpp"
    "domain/bond_instrument_table_io.cpp"
    "domain/callable_swap_instrument_json_io.cpp"
    "domain/callable_swap_instrument_table.cpp"
    "domain/callable_swap_instrument_table_io.cpp"
    "domain/cap_floor_instrument_json_io.cpp"
    "domain/cap_floor_instrument_table.cpp"
    "domain/cap_floor_instrument_table_io.cpp"
    "domain/commodity_instrument_json_io.cpp"
    "domain/commodity_instrument_table.cpp"
    "domain/commodity_instrument_table_io.cpp"
    "domain/composite_instrument_json_io.cpp"
    "domain/composite_instrument_table.cpp"
    "domain/composite_instrument_table_io.cpp"
    "domain/composite_leg_json_io.cpp"
    "domain/composite_leg_table.cpp"
    "domain/composite_leg_table_io.cpp"
    "domain/credit_instrument_json_io.cpp"
    "domain/credit_instrument_table.cpp"
    "domain/credit_instrument_table_io.cpp"
    "domain/equity_accumulator_instrument_json_io.cpp"
    "domain/equity_asian_option_instrument_json_io.cpp"
    "domain/equity_barrier_option_instrument_json_io.cpp"
    "domain/equity_digital_option_instrument_json_io.cpp"
    "domain/equity_forward_instrument_json_io.cpp"
    "domain/equity_option_instrument_json_io.cpp"
    "domain/equity_position_instrument_json_io.cpp"
    "domain/equity_swap_instrument_json_io.cpp"
    "domain/equity_variance_swap_instrument_json_io.cpp"
    "domain/fpml_event_type_json_io.cpp"
    "domain/fpml_event_type_table.cpp"
    "domain/fpml_event_type_table_io.cpp"
    "domain/fra_instrument_json_io.cpp"
    "domain/fra_instrument_table.cpp"
    "domain/fra_instrument_table_io.cpp"
    "domain/fx_accumulator_instrument_json_io.cpp"
    "domain/fx_asian_forward_instrument_json_io.cpp"
    "domain/fx_barrier_option_instrument_json_io.cpp"
    "domain/fx_digital_option_instrument_json_io.cpp"
    "domain/fx_forward_instrument_json_io.cpp"
    "domain/fx_vanilla_option_instrument_json_io.cpp"
    "domain/fx_variance_swap_instrument_json_io.cpp"
    "domain/inflation_swap_instrument_json_io.cpp"
    "domain/inflation_swap_instrument_table.cpp"
    "domain/inflation_swap_instrument_table_io.cpp"
    "domain/knock_out_swap_instrument_json_io.cpp"
    "domain/knock_out_swap_instrument_table.cpp"
    "domain/knock_out_swap_instrument_table_io.cpp"
    "domain/lifecycle_event_json_io.cpp"
    "domain/lifecycle_event_table.cpp"
    "domain/lifecycle_event_table_io.cpp"
    "domain/party_role_type_json_io.cpp"
    "domain/party_role_type_table.cpp"
    "domain/party_role_type_table_io.cpp"
    "domain/rpa_instrument_json_io.cpp"
    "domain/rpa_instrument_table.cpp"
    "domain/rpa_instrument_table_io.cpp"
    "domain/scripted_instrument_json_io.cpp"
    "domain/scripted_instrument_table.cpp"
    "domain/scripted_instrument_table_io.cpp"
    "domain/swap_leg_json_io.cpp"
    "domain/swap_leg_table.cpp"
    "domain/swap_leg_table_io.cpp"
    "domain/swaption_instrument_json_io.cpp"
    "domain/swaption_instrument_table.cpp"
    "domain/swaption_instrument_table_io.cpp"
    "domain/trade_id_type_json_io.cpp"
    "domain/trade_id_type_table.cpp"
    "domain/trade_id_type_table_io.cpp"
    "domain/trade_identifier_json_io.cpp"
    "domain/trade_identifier_table.cpp"
    "domain/trade_identifier_table_io.cpp"
    "domain/trade_json_io.cpp"
    "domain/trade_party_role_json_io.cpp"
    "domain/trade_party_role_table.cpp"
    "domain/trade_party_role_table_io.cpp"
    "domain/trade_table.cpp"
    "domain/trade_table_io.cpp"
    "domain/trade_type_json_io.cpp"
    "domain/trade_type_table.cpp"
    "domain/trade_type_table_io.cpp"
    "domain/vanilla_swap_instrument_json_io.cpp"
    "domain/vanilla_swap_instrument_table.cpp"
    "domain/vanilla_swap_instrument_table_io.cpp"
    "generator/lifecycle_event_generator.cpp"
    "generator/trade_identifier_generator.cpp"
    "generators/activity_type_generator.cpp"
    "generators/balance_guaranteed_swap_instrument_generator.cpp"
    "generators/callable_swap_instrument_generator.cpp"
    "generators/cap_floor_instrument_generator.cpp"
    "generators/composite_instrument_generator.cpp"
    "generators/composite_leg_generator.cpp"
    "generators/fpml_event_type_generator.cpp"
    "generators/fra_instrument_generator.cpp"
    "generators/inflation_swap_instrument_generator.cpp"
    "generators/knock_out_swap_instrument_generator.cpp"
    "generators/party_role_type_generator.cpp"
    "generators/rpa_instrument_generator.cpp"
    "generators/scripted_instrument_generator.cpp"
    "generators/swap_leg_generator.cpp"
    "generators/swaption_instrument_generator.cpp"
    "generators/trade_generator.cpp"
    "generators/trade_id_type_generator.cpp"
    "generators/trade_party_role_generator.cpp"
    "generators/trade_type_generator.cpp"
    "generators/vanilla_swap_instrument_generator.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_TRADING_API_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.trading/api/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.dq.api.lib
        ores.eventing.api.lib
        ores.platform.lib
    PRIVATE
        ores.utility.lib
        faker-cxx::faker-cxx
        libfort::fort)

# rfl's no_duplicate_field_names check is O(n²) in constexpr recursion depth.
# The trade_instrument variant (8 alternatives, ~150 combined fields with
# rfl::AddTagsToVariants) requires depth >> 512. PUBLIC propagates to all
# consumers automatically so they don't need to set this independently.
target_compile_options(${lib_target_name} PUBLIC
    $<$<AND:$<CXX_COMPILER_ID:MSVC>,$<COMPILE_LANGUAGE:CXX>>:/constexpr:depth100000>
    $<$<AND:$<CXX_COMPILER_ID:MSVC>,$<COMPILE_LANGUAGE:CXX>>:/constexpr:steps10000000>
    $<$<AND:$<CXX_COMPILER_ID:Clang>,$<COMPILE_LANGUAGE:CXX>>:-fbracket-depth=1024>)


target_link_libraries(${lib_target_name} PRIVATE ores.rfl.pch)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
