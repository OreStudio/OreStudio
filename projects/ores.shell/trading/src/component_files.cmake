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
    "app/commands/trading/equity_accumulator_instrument_commands.cpp"
    "app/commands/trading/equity_asian_option_instrument_commands.cpp"
    "app/commands/trading/equity_barrier_option_instrument_commands.cpp"
    "app/commands/trading/equity_digital_option_instrument_commands.cpp"
    "app/commands/trading/equity_forward_instrument_commands.cpp"
    "app/commands/trading/equity_option_instrument_commands.cpp"
    "app/commands/trading/equity_position_instrument_commands.cpp"
    "app/commands/trading/equity_swap_instrument_commands.cpp"
    "app/commands/trading/equity_variance_swap_instrument_commands.cpp"
    "app/commands/trading/fx_accumulator_instrument_commands.cpp"
    "app/commands/trading/fx_asian_forward_instrument_commands.cpp"
    "app/commands/trading/fx_barrier_option_instrument_commands.cpp"
    "app/commands/trading/fx_digital_option_instrument_commands.cpp"
    "app/commands/trading/fx_forward_instrument_commands.cpp"
    "app/commands/trading/fx_vanilla_option_instrument_commands.cpp"
    "app/commands/trading/fx_variance_swap_instrument_commands.cpp"
    "app/commands/trading/lifecycle_event_commands.cpp"
    "app/commands/trading/party_role_type_commands.cpp"
    "app/commands/trading/trade_id_type_commands.cpp"
    "app/commands/trading/trade_identifier_commands.cpp"
    "app/commands/trading/trade_party_role_commands.cpp"
    "app/commands/trading/trade_type_commands.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/equity_accumulator_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/equity_asian_option_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/equity_barrier_option_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/equity_digital_option_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/equity_forward_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/equity_option_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/equity_position_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/equity_swap_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/equity_variance_swap_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/fx_accumulator_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/fx_asian_forward_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/fx_barrier_option_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/fx_digital_option_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/fx_forward_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/fx_vanilla_option_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/fx_variance_swap_instrument_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/lifecycle_event_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/party_role_type_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/trade_id_type_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/trade_identifier_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/trade_party_role_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/trading/trade_type_commands.hpp"
)
