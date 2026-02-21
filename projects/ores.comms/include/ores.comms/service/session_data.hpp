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
#ifndef ORES_COMMS_SERVICE_SESSION_DATA_HPP
#define ORES_COMMS_SERVICE_SESSION_DATA_HPP

#include <chrono>
#include <string>
#include <vector>
#include <cstdint>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include <boost/asio/ip/address.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::comms::service {

/**
 * @brief A single bytes-sent/received sample captured at heartbeat time.
 *
 * Samples are collected server-side on each heartbeat ping and stored
 * in-memory until the session ends, then bulk-inserted to the database.
 */
struct session_sample final {
    /**
     * @brief When this sample was taken.
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief Cumulative bytes sent on the connection at sample time.
     */
    std::uint64_t bytes_sent = 0;

    /**
     * @brief Cumulative bytes received on the connection at sample time.
     */
    std::uint64_t bytes_received = 0;

    /**
     * @brief Round-trip time reported by the client in this ping, in milliseconds.
     *
     * Zero if not reported (first ping or older client).
     */
    std::uint64_t latency_ms = 0;
};

/**
 * @brief Protocol used for the session connection.
 */
enum class session_protocol : std::uint8_t {
    /**
     * @brief ORE Studio binary protocol over TCP.
     */
    binary = 0,

    /**
     * @brief HTTP/REST API with JWT authentication.
     */
    http = 1
};

/**
 * @brief Session data owned by ores.comms for protocol-level session tracking.
 *
 * This struct exists to break the circular dependency between ores.comms and
 * ores.iam. Previously, ores.comms depended on ores.iam for the session type,
 * which created a cycle:
 *
 *   ores.comms -> ores.iam -> ores.variability -> ores.comms
 *
 * By defining session data here in ores.comms, the dependency is inverted:
 * ores.iam now depends on ores.comms (which it does anyway via variability),
 * and provides conversion functions between this struct and its domain::session.
 *
 * This is a plain data struct with no behavior beyond trivial helpers.
 * The persistence layer (repository, mappers) remains in ores.iam.
 */
struct session_data final {
    /**
     * @brief Unique identifier for this session.
     */
    boost::uuids::uuid id;

    /**
     * @brief Foreign key referencing the associated account.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief Tenant ID for this session.
     *
     * Resolved at login time from the account's tenant and immutable for the
     * session lifetime. Used to establish the correct database context for
     * all operations performed during this session.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Party ID for party-level isolation.
     *
     * Resolved at login time from the account's party assignments.
     * Nil UUID if the account has no party assignment.
     */
    boost::uuids::uuid party_id = {};

    /**
     * @brief Visible party IDs for party-level RLS.
     *
     * Computed at login time from the party hierarchy. Contains the
     * party_id and all its descendant parties. Empty if no party context.
     */
    std::vector<boost::uuids::uuid> visible_party_ids;

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
     * @brief Cumulative bytes sent at the time of the last sample, used to
     *        compute per-interval deltas for the next sample.
     */
    std::uint64_t last_sample_bytes_sent = 0;

    /**
     * @brief Cumulative bytes received at the time of the last sample.
     */
    std::uint64_t last_sample_bytes_received = 0;

    /**
     * @brief Time-series samples accumulating since the last flush.
     *
     * Each entry captures the bytes transferred IN that heartbeat interval
     * (delta, not cumulative). When the flush threshold is reached, samples
     * are moved to flush_pending and this vector is cleared. Any remaining
     * samples are flushed at logout.
     */
    std::vector<session_sample> samples;

    /**
     * @brief Samples ready to be persisted to the database.
     *
     * Populated by record_sample() when the flush threshold is reached.
     * Drained by accounts_message_handler on the next non-ping request.
     */
    std::vector<session_sample> flush_pending;

    /**
     * @brief ISO 3166-1 alpha-2 country code from geolocation.
     *
     * Empty if geolocation is not available or IP is private/localhost.
     */
    std::string country_code;

    /**
     * @brief Protocol used for this session (binary or HTTP).
     */
    session_protocol protocol = session_protocol::binary;

    /**
     * @brief Username of the account that owns this session.
     *
     * Cached here for efficient access without needing to look up the account.
     * Set during login and immutable for the session lifetime.
     */
    std::string username;

    /**
     * @brief Checks if the session is still active.
     */
    [[nodiscard]] bool is_active() const {
        return !end_time.has_value();
    }

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
};

}

#endif
