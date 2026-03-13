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
#ifndef ORES_COMMS_MESSAGING_MESSAGE_SERVER_HPP
#define ORES_COMMS_MESSAGING_MESSAGE_SERVER_HPP

#include <memory>
#include <string>
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"

namespace ores::comms::messaging {

/**
 * @brief Abstract interface for a message server.
 *
 * Implemented by both the legacy ASIO SSL server (comms::net::server)
 * and the NATS server (nats::service::nats_server). All domain registrars
 * take this interface rather than the concrete transport type so that the
 * transport can be swapped without touching handler code.
 */
class message_server {
public:
    virtual ~message_server() = default;

    /**
     * @brief Register a message handler for a range of message types.
     */
    virtual void register_handler(message_type_range range,
        std::shared_ptr<message_handler> handler) = 0;

    /**
     * @brief Get the shared auth session service.
     */
    [[nodiscard]] virtual std::shared_ptr<service::auth_session_service>
    sessions() const = 0;

    /**
     * @brief Broadcast database status to all connected clients.
     *
     * @param available Whether the database is available.
     * @param error_message Error message if unavailable, empty otherwise.
     */
    virtual void broadcast_database_status(bool available,
        const std::string& error_message) = 0;
};

}

#endif
