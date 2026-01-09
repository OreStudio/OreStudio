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
#ifndef ORES_COMMS_SERVICE_HEARTBEAT_SERVICE_HPP
#define ORES_COMMS_SERVICE_HEARTBEAT_SERVICE_HPP

#include <cstdint>
#include <boost/asio/awaitable.hpp>
#include "ores.logging/make_logger.hpp"

namespace ores::comms::net { class connection; }

namespace ores::comms::service {

/**
 * @brief Service for handling heartbeat (ping/pong) protocol on the server.
 *
 * Provides server-side handling of ping messages by sending pong responses.
 * Client-side heartbeat is handled by the client's message loop using
 * correlation IDs for response matching.
 */
class heartbeat_service final {
private:
    inline static std::string_view logger_name =
        "ores.comms.service.heartbeat_service";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Handle incoming ping from client and send pong response.
     *
     * @param conn Connection to send pong response on
     * @param sequence Sequence number for pong frame
     * @param correlation_id Correlation ID to echo back in pong
     */
    static boost::asio::awaitable<void> handle_ping(
        net::connection& conn,
        std::uint32_t sequence,
        std::uint32_t correlation_id);
};

}

#endif
