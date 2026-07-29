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
set(name "ores.trading.core")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "messaging/registrar.cpp"
    "messaging/registrar_bond.cpp"
    "messaging/registrar_commodity.cpp"
    "messaging/registrar_composite.cpp"
    "messaging/registrar_credit.cpp"
    "messaging/registrar_equity.cpp"
    "messaging/registrar_fx.cpp"
    "messaging/registrar_rates.cpp"
    "messaging/registrar_scripted.cpp"
    "messaging/registrar_trades.cpp"
    "repository/activity_type_entity.cpp"
    "repository/activity_type_mapper.cpp"
    "repository/activity_type_repository.cpp"
    "repository/balance_guaranteed_swap_instrument_entity.cpp"
    "repository/balance_guaranteed_swap_instrument_mapper.cpp"
    "repository/balance_guaranteed_swap_instrument_repository.cpp"
    "repository/bond_instrument_entity.cpp"
    "repository/bond_instrument_mapper.cpp"
    "repository/bond_instrument_repository.cpp"
    "repository/callable_swap_instrument_entity.cpp"
    "repository/callable_swap_instrument_mapper.cpp"
    "repository/callable_swap_instrument_repository.cpp"
    "repository/cap_floor_instrument_entity.cpp"
    "repository/cap_floor_instrument_mapper.cpp"
    "repository/cap_floor_instrument_repository.cpp"
    "repository/commodity_instrument_entity.cpp"
    "repository/commodity_instrument_mapper.cpp"
    "repository/commodity_instrument_repository.cpp"
    "repository/composite_instrument_entity.cpp"
    "repository/composite_instrument_mapper.cpp"
    "repository/composite_instrument_repository.cpp"
    "repository/composite_leg_entity.cpp"
    "repository/composite_leg_mapper.cpp"
    "repository/composite_leg_repository.cpp"
    "repository/credit_instrument_entity.cpp"
    "repository/credit_instrument_mapper.cpp"
    "repository/credit_instrument_repository.cpp"
    "repository/equity_accumulator_instrument_entity.cpp"
    "repository/equity_accumulator_instrument_mapper.cpp"
    "repository/equity_accumulator_instrument_repository.cpp"
    "repository/equity_asian_option_instrument_entity.cpp"
    "repository/equity_asian_option_instrument_mapper.cpp"
    "repository/equity_asian_option_instrument_repository.cpp"
    "repository/equity_barrier_option_instrument_entity.cpp"
    "repository/equity_barrier_option_instrument_mapper.cpp"
    "repository/equity_barrier_option_instrument_repository.cpp"
    "repository/equity_digital_option_instrument_entity.cpp"
    "repository/equity_digital_option_instrument_mapper.cpp"
    "repository/equity_digital_option_instrument_repository.cpp"
    "repository/equity_forward_instrument_entity.cpp"
    "repository/equity_forward_instrument_mapper.cpp"
    "repository/equity_forward_instrument_repository.cpp"
    "repository/equity_option_instrument_entity.cpp"
    "repository/equity_option_instrument_mapper.cpp"
    "repository/equity_option_instrument_repository.cpp"
    "repository/equity_position_instrument_entity.cpp"
    "repository/equity_position_instrument_mapper.cpp"
    "repository/equity_position_instrument_repository.cpp"
    "repository/equity_swap_instrument_entity.cpp"
    "repository/equity_swap_instrument_mapper.cpp"
    "repository/equity_swap_instrument_repository.cpp"
    "repository/equity_variance_swap_instrument_entity.cpp"
    "repository/equity_variance_swap_instrument_mapper.cpp"
    "repository/equity_variance_swap_instrument_repository.cpp"
    "repository/fpml_event_type_entity.cpp"
    "repository/fpml_event_type_mapper.cpp"
    "repository/fpml_event_type_repository.cpp"
    "repository/fra_instrument_entity.cpp"
    "repository/fra_instrument_mapper.cpp"
    "repository/fra_instrument_repository.cpp"
    "repository/fx_accumulator_instrument_entity.cpp"
    "repository/fx_accumulator_instrument_mapper.cpp"
    "repository/fx_accumulator_instrument_repository.cpp"
    "repository/fx_asian_forward_instrument_entity.cpp"
    "repository/fx_asian_forward_instrument_mapper.cpp"
    "repository/fx_asian_forward_instrument_repository.cpp"
    "repository/fx_barrier_option_instrument_entity.cpp"
    "repository/fx_barrier_option_instrument_mapper.cpp"
    "repository/fx_barrier_option_instrument_repository.cpp"
    "repository/fx_digital_option_instrument_entity.cpp"
    "repository/fx_digital_option_instrument_mapper.cpp"
    "repository/fx_digital_option_instrument_repository.cpp"
    "repository/fx_forward_instrument_entity.cpp"
    "repository/fx_forward_instrument_mapper.cpp"
    "repository/fx_forward_instrument_repository.cpp"
    "repository/fx_vanilla_option_instrument_entity.cpp"
    "repository/fx_vanilla_option_instrument_mapper.cpp"
    "repository/fx_vanilla_option_instrument_repository.cpp"
    "repository/fx_variance_swap_instrument_entity.cpp"
    "repository/fx_variance_swap_instrument_mapper.cpp"
    "repository/fx_variance_swap_instrument_repository.cpp"
    "repository/inflation_swap_instrument_entity.cpp"
    "repository/inflation_swap_instrument_mapper.cpp"
    "repository/inflation_swap_instrument_repository.cpp"
    "repository/knock_out_swap_instrument_entity.cpp"
    "repository/knock_out_swap_instrument_mapper.cpp"
    "repository/knock_out_swap_instrument_repository.cpp"
    "repository/lifecycle_event_entity.cpp"
    "repository/lifecycle_event_mapper.cpp"
    "repository/lifecycle_event_repository.cpp"
    "repository/party_role_type_entity.cpp"
    "repository/party_role_type_mapper.cpp"
    "repository/party_role_type_repository.cpp"
    "repository/rpa_instrument_entity.cpp"
    "repository/rpa_instrument_mapper.cpp"
    "repository/rpa_instrument_repository.cpp"
    "repository/scripted_instrument_entity.cpp"
    "repository/scripted_instrument_mapper.cpp"
    "repository/scripted_instrument_repository.cpp"
    "repository/swap_leg_entity.cpp"
    "repository/swap_leg_mapper.cpp"
    "repository/swap_leg_repository.cpp"
    "repository/swaption_instrument_entity.cpp"
    "repository/swaption_instrument_mapper.cpp"
    "repository/swaption_instrument_repository.cpp"
    "repository/trade_entity.cpp"
    "repository/trade_id_type_entity.cpp"
    "repository/trade_id_type_mapper.cpp"
    "repository/trade_id_type_repository.cpp"
    "repository/trade_identifier_entity.cpp"
    "repository/trade_identifier_mapper.cpp"
    "repository/trade_identifier_repository.cpp"
    "repository/trade_mapper.cpp"
    "repository/trade_party_role_entity.cpp"
    "repository/trade_party_role_mapper.cpp"
    "repository/trade_party_role_repository.cpp"
    "repository/trade_repository.cpp"
    "repository/trade_type_entity.cpp"
    "repository/trade_type_mapper.cpp"
    "repository/trade_type_repository.cpp"
    "repository/vanilla_swap_instrument_entity.cpp"
    "repository/vanilla_swap_instrument_mapper.cpp"
    "repository/vanilla_swap_instrument_repository.cpp"
    "service/activity_type_service.cpp"
    "service/balance_guaranteed_swap_instrument_service.cpp"
    "service/bond_instrument_service.cpp"
    "service/callable_swap_instrument_service.cpp"
    "service/cap_floor_instrument_service.cpp"
    "service/commodity_instrument_service.cpp"
    "service/composite_instrument_service.cpp"
    "service/credit_instrument_service.cpp"
    "service/equity_accumulator_instrument_service.cpp"
    "service/equity_asian_option_instrument_service.cpp"
    "service/equity_barrier_option_instrument_service.cpp"
    "service/equity_digital_option_instrument_service.cpp"
    "service/equity_forward_instrument_service.cpp"
    "service/equity_option_instrument_service.cpp"
    "service/equity_position_instrument_service.cpp"
    "service/equity_swap_instrument_service.cpp"
    "service/equity_variance_swap_instrument_service.cpp"
    "service/fpml_event_type_service.cpp"
    "service/fra_instrument_service.cpp"
    "service/fx_accumulator_instrument_service.cpp"
    "service/fx_asian_forward_instrument_service.cpp"
    "service/fx_barrier_option_instrument_service.cpp"
    "service/fx_digital_option_instrument_service.cpp"
    "service/fx_forward_instrument_service.cpp"
    "service/fx_vanilla_option_instrument_service.cpp"
    "service/fx_variance_swap_instrument_service.cpp"
    "service/inflation_swap_instrument_service.cpp"
    "service/knock_out_swap_instrument_service.cpp"
    "service/lifecycle_event_service.cpp"
    "service/party_role_type_service.cpp"
    "service/rpa_instrument_service.cpp"
    "service/scripted_instrument_service.cpp"
    "service/swaption_instrument_service.cpp"
    "service/trade_id_type_service.cpp"
    "service/trade_identifier_service.cpp"
    "service/trade_party_role_service.cpp"
    "service/trade_service.cpp"
    "service/trade_status_service.cpp"
    "service/trade_type_service.cpp"
    "service/vanilla_swap_instrument_service.cpp"
)

set(lib_files ${files})
# On Windows the DLL export table is capped at 65535 symbols. This library
# instantiates enough rfl/Boost templates to exceed that limit, and it is
# only consumed by executables, so no DLL boundary needs to be crossed.
# Build it as a static library on Windows to avoid the limit.
if(WIN32)
    add_library(${lib_target_name} STATIC ${lib_files})
else()
    add_library(${lib_target_name} ${lib_files})
endif()
target_compile_definitions(${lib_target_name} PRIVATE ORES_TRADING_CORE_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
if(NOT WIN32)
    set_target_properties(${lib_target_name}
        PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})
endif()

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.trading.api.lib
        ores.nats.lib
    PRIVATE
        ores.service.lib
        ores.storage.lib
        ores.database.lib
        ores.dq.api.lib
        ores.utility.lib
        ores.security.lib
        sqlgen::sqlgen
        faker-cxx::faker-cxx
        libfort::fort
        Boost::boost)
# Note: reflectcpp comes transitively through ores.utility.lib -> ores.platform.lib (PUBLIC)

# On Windows ores.trading.core is a static archive (see above) whose Debug
# build exceeds 2 GB — the WiX v3 single-file cabinet offset limit. The
# archive is only consumed by our own executables at link time, so nothing
# downstream needs to find it in the installer. Skip install on Windows.
if(NOT WIN32)
    install(TARGETS ${lib_target_name}
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
endif()
