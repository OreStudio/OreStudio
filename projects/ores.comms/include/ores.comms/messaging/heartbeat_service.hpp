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
#ifndef ORES_COMMS_MESSAGING_HEARTBEAT_SERVICE_HPP
#define ORES_COMMS_MESSAGING_HEARTBEAT_SERVICE_HPP

#include <cstdint>
#include <boost/asio/awaitable.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::comms::net { class connection; }

namespace ores::comms::messaging {

/**
 * @brief Service for handling heartbeat (ping/pong) protocol.
 *
 * Provides lightweight connection liveness checking through ping/pong
 * messages. Follows the same pattern as handshake_service for consistency.
 */
class heartbeat_service final {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.comms.messaging.heartbeat_service");
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

    /**
     * @brief Send ping and wait for pong response (client-side).
     *
     * @param conn Connection to perform ping/pong on
     * @param sequence Sequence number for ping frame
     * @return true if pong received, false on timeout/error
     */
    static boost::asio::awaitable<bool> send_ping(
        net::connection& conn,
        std::uint32_t sequence);
};

}

#endif
