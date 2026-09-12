/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.shell/app/commands/trading/trading_commands.hpp"
#include "ores.shell/app/commands/trading/balance_guaranteed_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/bond_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/callable_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/cap_floor_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/commodity_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/composite_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/credit_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_accumulator_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_asian_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_barrier_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_digital_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_forward_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_position_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_variance_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fra_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_accumulator_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_asian_forward_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_barrier_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_digital_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_forward_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_vanilla_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_variance_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/inflation_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/knock_out_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/lifecycle_event_commands.hpp"
#include "ores.shell/app/commands/trading/ore_commands.hpp"
#include "ores.shell/app/commands/trading/party_role_type_commands.hpp"
#include "ores.shell/app/commands/trading/rpa_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/scripted_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/swaption_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/trade_commands.hpp"
#include "ores.shell/app/commands/trading/trade_id_type_commands.hpp"
#include "ores.shell/app/commands/trading/trade_identifier_commands.hpp"
#include "ores.shell/app/commands/trading/trade_party_role_commands.hpp"
#include "ores.shell/app/commands/trading/trade_type_commands.hpp"
#include "ores.shell/app/commands/trading/vanilla_swap_instrument_commands.hpp"

namespace ores::shell::app::commands {

using namespace logging;

void trading_commands::register_commands(cli::Menu& root_menu,
                                         ores::nats::service::nats_client& session,
                                         pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Registering trading command surface.";
    balance_guaranteed_swap_instrument_commands::register_commands(root_menu, session, pagination);
    bond_instrument_commands::register_commands(root_menu, session, pagination);
    callable_swap_instrument_commands::register_commands(root_menu, session, pagination);
    cap_floor_instrument_commands::register_commands(root_menu, session, pagination);
    commodity_instrument_commands::register_commands(root_menu, session, pagination);
    composite_instrument_commands::register_commands(root_menu, session, pagination);
    credit_instrument_commands::register_commands(root_menu, session, pagination);
    equity_accumulator_instrument_commands::register_commands(root_menu, session, pagination);
    equity_asian_option_instrument_commands::register_commands(root_menu, session, pagination);
    equity_barrier_option_instrument_commands::register_commands(root_menu, session, pagination);
    equity_digital_option_instrument_commands::register_commands(root_menu, session, pagination);
    equity_forward_instrument_commands::register_commands(root_menu, session, pagination);
    equity_option_instrument_commands::register_commands(root_menu, session, pagination);
    equity_position_instrument_commands::register_commands(root_menu, session, pagination);
    equity_swap_instrument_commands::register_commands(root_menu, session, pagination);
    equity_variance_swap_instrument_commands::register_commands(root_menu, session, pagination);
    fx_accumulator_instrument_commands::register_commands(root_menu, session, pagination);
    fx_asian_forward_instrument_commands::register_commands(root_menu, session, pagination);
    fx_barrier_option_instrument_commands::register_commands(root_menu, session, pagination);
    fx_digital_option_instrument_commands::register_commands(root_menu, session, pagination);
    fx_forward_instrument_commands::register_commands(root_menu, session, pagination);
    fx_vanilla_option_instrument_commands::register_commands(root_menu, session, pagination);
    fx_variance_swap_instrument_commands::register_commands(root_menu, session, pagination);
    fra_instrument_commands::register_commands(root_menu, session, pagination);
    inflation_swap_instrument_commands::register_commands(root_menu, session, pagination);
    knock_out_swap_instrument_commands::register_commands(root_menu, session, pagination);
    lifecycle_event_commands::register_commands(root_menu, session, pagination);
    ore_commands::register_commands(root_menu, session);
    party_role_type_commands::register_commands(root_menu, session, pagination);
    rpa_instrument_commands::register_commands(root_menu, session, pagination);
    scripted_instrument_commands::register_commands(root_menu, session, pagination);
    swaption_instrument_commands::register_commands(root_menu, session, pagination);
    trade_commands::register_commands(root_menu, session, pagination);
    trade_id_type_commands::register_commands(root_menu, session, pagination);
    trade_identifier_commands::register_commands(root_menu, session, pagination);
    trade_party_role_commands::register_commands(root_menu, session, pagination);
    trade_type_commands::register_commands(root_menu, session, pagination);
    vanilla_swap_instrument_commands::register_commands(root_menu, session, pagination);
}

}
