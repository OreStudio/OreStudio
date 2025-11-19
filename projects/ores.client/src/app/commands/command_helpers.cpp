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
#include "ores.client/app/commands/command_helpers.hpp"

#include <ostream>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>

namespace ores::client::app::commands {

bool command_helpers::require_connection(
    const std::shared_ptr<comms::client>& client,
    std::ostream& out) {

    if (!client || !client->is_connected()) {
        out << "✗ Not connected to server. Use 'connect' command first" << std::endl;
        return false;
    }
    return true;
}

template<typename Awaitable>
void command_helpers::spawn_command(boost::asio::io_context& io_ctx, Awaitable&& awaitable) {
    auto executor = io_ctx.get_executor();
    boost::asio::co_spawn(executor, std::forward<Awaitable>(awaitable), boost::asio::detached);
}

// Explicit instantiation for common types
template void command_helpers::spawn_command(
    boost::asio::io_context&,
    boost::asio::awaitable<void>&&);

}
