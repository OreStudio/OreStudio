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
    "app/application.cpp"
    "app/host.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "main.cpp"
    "messaging/equity_accumulator_instrument_event_registrar.cpp"
    "messaging/equity_asian_option_instrument_event_registrar.cpp"
    "messaging/equity_barrier_option_instrument_event_registrar.cpp"
    "messaging/equity_digital_option_instrument_event_registrar.cpp"
    "messaging/equity_forward_instrument_event_registrar.cpp"
    "messaging/equity_option_instrument_event_registrar.cpp"
    "messaging/equity_position_instrument_event_registrar.cpp"
    "messaging/equity_swap_instrument_event_registrar.cpp"
    "messaging/equity_variance_swap_instrument_event_registrar.cpp"
    "messaging/fx_accumulator_instrument_event_registrar.cpp"
    "messaging/fx_asian_forward_instrument_event_registrar.cpp"
    "messaging/fx_barrier_option_instrument_event_registrar.cpp"
    "messaging/fx_digital_option_instrument_event_registrar.cpp"
    "messaging/fx_forward_instrument_event_registrar.cpp"
    "messaging/fx_vanilla_option_instrument_event_registrar.cpp"
    "messaging/fx_variance_swap_instrument_event_registrar.cpp"
    "messaging/party_role_type_event_registrar.cpp"
    "messaging/trade_type_event_registrar.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/app/application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/app/application_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/app/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/equity_accumulator_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/equity_asian_option_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/equity_barrier_option_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/equity_digital_option_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/equity_forward_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/equity_option_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/equity_position_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/equity_swap_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/equity_variance_swap_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/fx_accumulator_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/fx_asian_forward_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/fx_barrier_option_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/fx_digital_option_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/fx_forward_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/fx_vanilla_option_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/fx_variance_swap_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/party_role_type_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/messaging/trade_type_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.trading.service/ores.trading.service.hpp"
)
