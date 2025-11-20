/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

#include <memory>
#include <iosfwd>
#include <string>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.comms/net/client.hpp"

namespace cli {
class Menu;
}

namespace ores::shell::app::commands::connection {

/**
 * @brief Register connection management commands.
 *
 * Adds connect and disconnect commands to the root menu.
 *
 * @param root_menu The root menu to add commands to
 * @param io_ctx I/O context for async operations
 * @param client The comms client instance
 * @param config Configuration reference that can be updated
 */
void register_commands(
    ::cli::Menu& root_menu,
    boost::asio::io_context& io_ctx,
    std::shared_ptr<comms::net::client>& client,
    comms::net::client_options& config);

/**
 * @brief Process a connection request.
 *
 * Handles the async connection workflow including configuration updates
 * and existing connection cleanup.
 *
 * @param out Output stream for user feedback
 * @param client The client instance
 * @param config Configuration to update
 * @param host New host (empty to keep current)
 * @param port New port (empty to keep current)
 * @param identifier New client identifier (empty to keep current)
 */
boost::asio::awaitable<void> process_connect(
    std::ostream& out,
    std::shared_ptr<comms::net::client>& client,
    comms::net::client_options& config,
    std::string host,
    std::string port,
    std::string identifier);

/**
 * @brief Process a disconnect request.
 *
 * Cleanly disconnects from the server if connected.
 *
 * @param client The client instance
 */
void process_disconnect(std::shared_ptr<comms::net::client>& client);

}

#endif
