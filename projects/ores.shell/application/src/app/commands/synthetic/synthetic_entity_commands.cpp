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
#include "ores.shell/app/commands/synthetic/synthetic_entity_commands.hpp"
#include "ores.shell/app/commands/synthetic/folder_commands.hpp"
#include "ores.shell/app/commands/synthetic/fx_spot_generation_config_commands.hpp"
#include "ores.shell/app/commands/synthetic/gmm_component_commands.hpp"
#include "ores.shell/app/commands/synthetic/ir_curve_generation_config_commands.hpp"
#include "ores.shell/app/commands/synthetic/ir_curve_generation_config_process_parameter_value_commands.hpp"
#include "ores.shell/app/commands/synthetic/ir_curve_template_entry_commands.hpp"
#include "ores.shell/app/commands/synthetic/market_data_generation_config_commands.hpp"
#include "ores.shell/app/commands/synthetic/yield_curve_process_parameter_definition_commands.hpp"
#include "ores.shell/app/commands/synthetic/yield_curve_process_type_commands.hpp"

namespace ores::shell::app::commands {

using namespace logging;

void synthetic_entity_commands::register_commands(cli::Menu& root_menu,
                                                  ores::nats::service::nats_client& session,
                                                  pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Registering synthetic entity command surface.";
    folder_commands::register_commands(root_menu, session, pagination);
    fx_spot_generation_config_commands::register_commands(root_menu, session, pagination);
    gmm_component_commands::register_commands(root_menu, session, pagination);
    ir_curve_generation_config_commands::register_commands(root_menu, session, pagination);
    ir_curve_template_entry_commands::register_commands(root_menu, session, pagination);
    ir_curve_generation_config_process_parameter_value_commands::register_commands(
        root_menu, session, pagination);
    market_data_generation_config_commands::register_commands(root_menu, session, pagination);
    yield_curve_process_type_commands::register_commands(root_menu, session, pagination);
    yield_curve_process_parameter_definition_commands::register_commands(
        root_menu, session, pagination);
}

}
