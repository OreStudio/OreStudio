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
#ifndef ORES_COMMS_NET_CLIENT_OPTIONS_HPP
#define ORES_COMMS_NET_CLIENT_OPTIONS_HPP

#include <iosfwd>
#include <string>
#include <chrono>
#include <cstdint>

namespace ores::comms::net {

/**
 * @brief Configuration for client retry and reconnection behavior.
 */
struct retry_options final {
    /**
     * @brief Maximum number of connection attempts before giving up.
     *
     * Set to 0 to disable retries (single attempt only).
     */
    std::uint32_t max_attempts{5};

    /**
     * @brief Base delay for exponential backoff.
     *
     * The actual delay is calculated as: base_delay * 2^attempt
     */
    std::chrono::milliseconds base_delay{100};

    /**
     * @brief Maximum delay between retry attempts.
     *
     * Caps the exponential backoff to prevent excessively long waits.
     */
    std::chrono::milliseconds max_delay{30000};

    /**
     * @brief Jitter factor for randomizing retry delays.
     *
     * A value of 0.2 means ±20% random variation. This helps prevent
     * thundering herd when multiple clients reconnect simultaneously.
     */
    double jitter_factor{0.2};

    /**
     * @brief Whether to automatically reconnect after disconnect.
     *
     * When enabled, the client will attempt to reconnect using exponential
     * backoff when the connection is lost (detected via heartbeat or read error).
     */
    bool auto_reconnect{true};
};

std::ostream& operator<<(std::ostream& s, const retry_options& v);

/**
 * @brief Configuration for the client.
 */
struct client_options final {
    /**
     * @brief Host to connect to.
     */
    std::string host = "localhost";

    /**
     * @brief Port to connect to.
     */
    std::uint16_t port = 55555;

    /**
     * @brief Client identifier to send when performing the handshake.
     */
    std::string client_identifier = "ores-client";

    /**
     * @brief Whether to verify the server's certificate.
     */
    bool verify_certificate = true;

    /**
     * @brief Whether to enable heartbeat (ping/pong) for connection monitoring.
     *
     * When enabled, the client will periodically send ping messages to the
     * server to detect disconnections proactively.
     */
    bool heartbeat_enabled = true;

    /**
     * @brief Interval between heartbeat pings in seconds.
     *
     * Default is 30 seconds. Lower values provide faster disconnect detection
     * but increase network traffic.
     */
    std::uint32_t heartbeat_interval_seconds = 30;

    /**
     * @brief Retry and reconnection options.
     */
    retry_options retry;
};

std::ostream& operator<<(std::ostream& s, const client_options& v);

}

#endif
