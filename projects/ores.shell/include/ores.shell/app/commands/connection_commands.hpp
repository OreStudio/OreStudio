/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025-2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_SHELL_APP_COMMANDS_CONNECTION_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_CONNECTION_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages commands related to connections.
 */
class connection_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.connection";

    auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register connection management commands.
     *
     * Adds connect and disconnect commands to the root menu.
     *
     * @param root The root menu to add commands to
     * @param session Client session for connectivity.
     */
    static void register_commands(cli::Menu& root,
                                  ores::nats::service::nats_client& session,
                                  ores::nats::config::nats_options connection_template = {});

    /**
     * @brief Process a connection request.
     *
     * Starts from @p connection_template (the connection the binary was
     * launched with, carrying TLS and the subject prefix) and applies the
     * positional host/port — or a single nats:// URL — and any
     * --tls-ca/--tls-cert/--tls-key/--subject-prefix overrides parsed from
     * @p args, then connects @p session.
     *
     * @param out Output stream for user feedback.
     * @param session Client session for connectivity.
     * @param connection_template Startup connection reused for TLS/prefix.
     * @param args connect command arguments (positionals + flags).
     */
    static void process_connect(std::ostream& out,
                                ores::nats::service::nats_client& session,
                                const ores::nats::config::nats_options& connection_template,
                                const std::vector<std::string>& args);

    /**
     * @brief Process a disconnect request.
     *
     * Cleanly disconnects from the server if connected.
     *
     * @param out Output stream for user feedback.
     * @param session Client session for connectivity.
     */
    static void process_disconnect(std::ostream& out, ores::nats::service::nats_client& session);
};

}

#endif
