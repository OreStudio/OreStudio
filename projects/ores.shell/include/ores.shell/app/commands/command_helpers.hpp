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
#ifndef ORES_SHELL_APP_COMMANDS_COMMAND_HELPERS_HPP
#define ORES_SHELL_APP_COMMANDS_COMMAND_HELPERS_HPP

#include <memory>
#include <iosfwd>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.comms/net/client.hpp"

namespace ores::client::app::commands {

/**
 * @brief Shared utility functions for command handlers.
 */
class command_helpers final {
public:
    /**
     * @brief Check if client is connected, display error if not.
     *
     * @param client The client instance to check
     * @param out Output stream for error messages
     * @return true if connected, false otherwise
     */
    static bool require_connection(
        const std::shared_ptr<comms::net::client>& client,
        std::ostream& out);

    /**
     * @brief Spawn a command coroutine with error handling.
     *
     * @param io_ctx The I/O context to spawn on
     * @param awaitable The coroutine to execute
     */
    template<typename Awaitable>
    static void spawn_command(boost::asio::io_context& io_ctx, Awaitable&& awaitable);
};

}

#endif
