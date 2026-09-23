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
#include "ores.shell/app/commands/analytics/analytics_commands.hpp"
#include "ores.shell/app/commands/analytics/pricing_engine_type_commands.hpp"
#include "ores.shell/app/commands/analytics/pricing_model_config_commands.hpp"
#include "ores.shell/app/commands/analytics/pricing_model_product_commands.hpp"
#include "ores.shell/app/commands/analytics/pricing_model_product_parameter_commands.hpp"

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

void analytics_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    BOOST_LOG_SEV(lg(), debug) << "Registering analytics command surface.";

    pricing_engine_type_commands::register_commands(root_menu, session);
    pricing_model_config_commands::register_commands(root_menu, session);
    pricing_model_product_commands::register_commands(root_menu, session);
    pricing_model_product_parameter_commands::register_commands(root_menu, session);
}

}
