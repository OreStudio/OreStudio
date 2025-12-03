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
#include "ores.comms/messaging/heartbeat_service.hpp"

#include "ores.comms/net/connection.hpp"
#include "ores.comms/messaging/heartbeat_protocol.hpp"

namespace ores::comms::messaging {

using namespace ores::utility::log;

boost::asio::awaitable<void> heartbeat_service::handle_ping(
    net::connection& conn,
    std::uint32_t sequence) {

    BOOST_LOG_SEV(lg(), trace) << "Received ping, sending pong response";

    auto pong_frame = create_pong_frame(sequence);
    co_await conn.write_frame(pong_frame);

    BOOST_LOG_SEV(lg(), trace) << "Sent pong response";
}

boost::asio::awaitable<bool> heartbeat_service::send_ping(
    net::connection& conn,
    std::uint32_t sequence) {

    BOOST_LOG_SEV(lg(), trace) << "Sending ping";

    auto ping_frame = create_ping_frame(sequence);
    co_await conn.write_frame(ping_frame);

    BOOST_LOG_SEV(lg(), trace) << "Waiting for pong response";
    auto response_result = co_await conn.read_frame();

    if (!response_result) {
        BOOST_LOG_SEV(lg(), debug) << "Failed to read pong response";
        co_return false;
    }

    const auto& response_frame = *response_result;
    if (response_frame.header().type != message_type::pong) {
        BOOST_LOG_SEV(lg(), warn) << "Expected pong, got message type "
                                  << response_frame.header().type;
        co_return false;
    }

    BOOST_LOG_SEV(lg(), trace) << "Received pong response";
    co_return true;
}

}
