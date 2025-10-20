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
#ifndef ORES_COMMS_SESSION_HPP
#define ORES_COMMS_SESSION_HPP

#include <memory>
#include <string>
#include "ores.comms/connection.hpp"
#include "ores.comms/protocol/message_dispatcher.hpp"

namespace ores::comms {

/**
 * @brief Represents a client session on the server side.
 *
 * Manages the lifecycle of a single client connection, including
 * handshake, message processing, and cleanup.
 */
class session final {
public:
    /**
     * @brief Construct a session from a connection.
     */
    explicit session(std::unique_ptr<connection> conn, std::string server_id,
        std::shared_ptr<protocol::message_dispatcher> dispatcher);

    /**
     * @brief Run the session.
     *
     * Performs handshake and processes messages until connection closes.
     */
    boost::asio::awaitable<void> run();

private:
    /**
     * @brief Perform handshake with client.
     *
     * Returns true if handshake succeeds, false otherwise.
     */
    boost::asio::awaitable<bool> perform_handshake();

    /**
     * @brief Process messages from client after handshake.
     *
     * Placeholder for future message handling.
     */
    boost::asio::awaitable<void> process_messages();

    std::unique_ptr<connection> conn_;
    std::string server_id_;
    std::shared_ptr<protocol::message_dispatcher> dispatcher_;
    std::uint32_t sequence_number_;
    bool handshake_complete_;
};

}

#endif
