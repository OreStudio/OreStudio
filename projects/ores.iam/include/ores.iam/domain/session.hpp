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
#ifndef ORES_IAM_DOMAIN_SESSION_HPP
#define ORES_IAM_DOMAIN_SESSION_HPP

#include <chrono>
#include <string>
#include <cstdint>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include <boost/asio/ip/address.hpp>

namespace ores::iam::domain {

/**
 * @brief Represents a user session in the system.
 *
 * This is the unified session type that serves both as:
 * - In-memory session data for authorization checks
 * - Persistent session record for historical tracking and analytics
 *
 * Sessions are stored in a TimescaleDB hypertable for efficient time-series
 * queries and automatic data lifecycle management.
 */
struct session final {
    /**
     * @brief Unique identifier for this session.
     */
    boost::uuids::uuid id;

    /**
     * @brief Foreign key referencing the associated account.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief Timestamp when the session started (login time).
     */
    std::chrono::system_clock::time_point start_time;

    /**
     * @brief Timestamp when the session ended (logout or disconnect).
     *
     * Empty if session is still active.
     */
    std::optional<std::chrono::system_clock::time_point> end_time;

    /**
     * @brief Client IP address.
     */
    boost::asio::ip::address client_ip;

    /**
     * @brief Client identifier string from handshake.
     *
     * Typically contains the client application name.
     */
    std::string client_identifier;

    /**
     * @brief Client protocol version major number.
     */
    std::uint16_t client_version_major = 0;

    /**
     * @brief Client protocol version minor number.
     */
    std::uint16_t client_version_minor = 0;

    /**
     * @brief Total bytes sent to the client during this session.
     */
    std::uint64_t bytes_sent = 0;

    /**
     * @brief Total bytes received from the client during this session.
     */
    std::uint64_t bytes_received = 0;

    /**
     * @brief ISO 3166-1 alpha-2 country code from geolocation.
     *
     * Empty if geolocation is not available or IP is private/localhost.
     */
    std::string country_code;

    /**
     * @brief Calculates the session duration.
     *
     * @return Duration if session has ended, nullopt if still active.
     */
    [[nodiscard]] std::optional<std::chrono::seconds> duration() const {
        if (!end_time) {
            return std::nullopt;
        }
        return std::chrono::duration_cast<std::chrono::seconds>(
            *end_time - start_time);
    }

    /**
     * @brief Checks if the session is still active.
     */
    [[nodiscard]] bool is_active() const {
        return !end_time.has_value();
    }
};

/**
 * @brief Aggregated session statistics for a time period.
 */
struct session_statistics final {
    /**
     * @brief Start of the time period.
     */
    std::chrono::system_clock::time_point period_start;

    /**
     * @brief End of the time period.
     */
    std::chrono::system_clock::time_point period_end;

    /**
     * @brief Account ID if statistics are per-account, nil for aggregate.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief Total number of sessions in this period.
     */
    std::uint64_t session_count = 0;

    /**
     * @brief Average session duration in seconds.
     */
    double avg_duration_seconds = 0.0;

    /**
     * @brief Total bytes sent across all sessions.
     */
    std::uint64_t total_bytes_sent = 0;

    /**
     * @brief Total bytes received across all sessions.
     */
    std::uint64_t total_bytes_received = 0;

    /**
     * @brief Average bytes sent per session.
     */
    double avg_bytes_sent = 0.0;

    /**
     * @brief Average bytes received per session.
     */
    double avg_bytes_received = 0.0;

    /**
     * @brief Number of unique countries from which sessions originated.
     */
    std::uint32_t unique_countries = 0;
};

}

#endif
