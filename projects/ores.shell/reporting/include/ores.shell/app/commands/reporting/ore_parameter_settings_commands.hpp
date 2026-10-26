/*
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
 */
#ifndef ORES_SHELL_APP_COMMANDS_REPORTING_ORE_PARAMETER_SETTINGS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_REPORTING_ORE_PARAMETER_SETTINGS_COMMANDS_HPP

#include "ores.nats/service/nats_client.hpp"
#include <cli/cli.h>

namespace ores::shell::app::commands {

/**
 * @brief The parameters of the contract with the value set each one states.
 *
 * The command reads the contract and the four value set tables over the
 * reporting subjects and joins them, so a person sees, for every parameter,
 * the set it names and the choices that set returns. `check` is the
 * completeness gate: it fails when a parameter has no set or a set has no
 * values.
 */
class ore_parameter_settings_commands {
public:
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);
};

}

#endif
