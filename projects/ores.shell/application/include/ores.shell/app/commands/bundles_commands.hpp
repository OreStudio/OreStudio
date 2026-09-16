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
#ifndef ORES_SHELL_APP_COMMANDS_BUNDLES_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_BUNDLES_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Commands for dataset bundles.
 *
 * Lists the bundles available for publication and publishes one,
 * mirroring what the provisioning wizards do behind their bundle
 * pages. Publication dispatches a workflow; --wait blocks on it via
 * workflow_commands::wait_for_instance.
 */
class bundles_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.bundles_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register bundle-related commands.
     *
     * Creates the bundles submenu with list and publish operations.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief List the dataset bundles available on the server.
     */
    static void process_list(std::ostream& out, ores::nats::service::nats_client& session);

    /**
     * @brief Publish a bundle: bundles publish <code> [--wait]
     * [--root-lei <lei>] [--party-id <id>] [--dataset <code>]
     * [--timeout <seconds>].
     *
     * Publishes atomically in upsert mode as the logged-in user, as
     * the wizards do. --root-lei, --party-id and --dataset populate
     * the publication parameters; --wait blocks until the dispatched
     * workflow reaches a terminal state.
     */
    static void process_publish(std::ostream& out,
                                ores::nats::service::nats_client& session,
                                const std::vector<std::string>& args);
};

}

#endif
