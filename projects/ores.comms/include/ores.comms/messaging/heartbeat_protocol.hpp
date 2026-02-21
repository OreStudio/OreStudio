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
#ifndef ORES_COMMS_MESSAGING_HEARTBEAT_PROTOCOL_HPP
#define ORES_COMMS_MESSAGING_HEARTBEAT_PROTOCOL_HPP

#include <vector>
#include <cstdint>
#include <expected>
#include "ores.comms/messaging/frame.hpp"

namespace ores::comms::messaging {

/**
 * @brief Ping message sent by client to check connection liveness.
 *
 * Carries the RTT measured from the previous heartbeat exchange so the
 * server can record it as part of the time-series sample.
 */
struct ping final {
    /**
     * @brief Round-trip time from the previous heartbeat, in milliseconds.
     *
     * Zero on the first ping (no previous measurement).
     */
    std::uint64_t latency_ms = 0;

    /**
     * @brief Serialize to frame payload.
     */
    static std::vector<std::byte> serialize(ping v);

    /**
     * @brief Deserialize from frame payload.
     */
    static std::expected<ping, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

/**
 * @brief Pong message sent by server in response to ping.
 *
 * Lightweight response message confirming connection is alive.
 */
struct pong final {
    /**
     * @brief Serialize to frame payload.
     */
    static std::vector<std::byte> serialize(pong v);

    /**
     * @brief Deserialize from frame payload.
     */
    static std::expected<pong, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

/**
 * @brief Create a ping frame.
 */
frame create_ping_frame(std::uint32_t sequence, std::uint32_t correlation_id = 0);

/**
 * @brief Create a pong frame.
 */
frame create_pong_frame(std::uint32_t sequence, std::uint32_t correlation_id = 0);

}

#endif
