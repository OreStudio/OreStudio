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
#include "ores.comms/service/heartbeat_service.hpp"

#include "ores.comms/net/connection.hpp"
#include "ores.comms/messaging/heartbeat_protocol.hpp"

namespace ores::comms::service {

using messaging::create_pong_frame;

using namespace ores::telemetry::log;

boost::asio::awaitable<void> heartbeat_service::handle_ping(
    net::connection& conn,
    std::uint32_t sequence,
    std::uint32_t correlation_id) {

    BOOST_LOG_SEV(lg(), trace) << "Received ping, sending pong response"
                               << " correlation_id=" << correlation_id;

    auto pong_frame = create_pong_frame(sequence, correlation_id);
    co_await conn.write_frame(pong_frame);

    BOOST_LOG_SEV(lg(), trace) << "Sent pong response";
}

}
