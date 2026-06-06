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
#ifndef ORES_SHELL_APP_COMMANDS_SYNTHETIC_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_SYNTHETIC_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.synthetic.api/messaging/generate_organisation_protocol.hpp"
#include <optional>
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Commands for synthetic data generation.
 *
 * Wraps the synthetic organisation generator the tenant provisioning
 * wizard uses for its synthetic data source. Every generation knob is
 * an optional flag carrying the wizard's default; the response prints
 * the actual seed used so a random run can be reproduced.
 */
class synthetic_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.synthetic_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register synthetic-data commands.
     *
     * Creates the synthetic submenu with the generate operation.
     */
    static void register_commands(cli::Menu& root_menu,
                                  ores::nats::service::nats_client& session);

    /**
     * @brief Generate a synthetic organisation:
     * synthetic generate [--country GB|US] [--party-count N]
     * [--party-max-depth N] [--counterparty-count N]
     * [--counterparty-max-depth N] [--portfolio-leaf-count N]
     * [--portfolio-max-depth N] [--books-per-portfolio N]
     * [--business-unit-count N] [--business-unit-max-depth N]
     * [--contacts-per-party N] [--contacts-per-counterparty N]
     * [--no-addresses] [--no-identifiers] [--seed N].
     */
    static void process_generate(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::vector<std::string>& args);

    /**
     * @brief The generation knob flag specs, for commands that embed
     * synthetic generation (provision tenant).
     */
    static std::vector<flag_spec> generate_flag_specs();

    /**
     * @brief Build a generation request from parsed flags, reporting
     * the offending flag on validation failure.
     */
    static std::optional<ores::synthetic::messaging::generate_organisation_request>
    build_generate_request(std::ostream& out, const parsed_args& parsed);

    /**
     * @brief Execute a generation request, printing the entity counts
     * and the actual seed used. Marks command failure on error.
     *
     * @return true on success.
     */
    static bool generate(std::ostream& out,
                         ores::nats::service::nats_client& session,
                         const ores::synthetic::messaging::generate_organisation_request& req);
};

}

#endif
