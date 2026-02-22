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
#ifndef ORES_IAM_REPOSITORY_SESSION_ENTITY_HPP
#define ORES_IAM_REPOSITORY_SESSION_ENTITY_HPP

#include <string>
#include <cstdint>
#include <optional>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::iam::repository {

/**
 * @brief Represents a session record in the database.
 *
 * This entity maps to a TimescaleDB hypertable partitioned by start_time
 * for efficient time-series queries.
 */
struct session_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_sessions_tbl";

    /**
     * @brief Session UUID - part of composite primary key.
     */
    sqlgen::PrimaryKey<std::string> id;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Foreign key to accounts table.
     */
    std::string account_id;

    /**
     * @brief Session start timestamp - part of composite primary key.
     *
     * Required in primary key for TimescaleDB hypertable partitioning.
     */
    sqlgen::PrimaryKey<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> start_time;

    /**
     * @brief Session end timestamp. Empty string if session is active.
     *
     * Uses std::string instead of std::optional because sqlgen doesn't
     * support optional types in UPDATE SET operations. Empty string
     * represents NULL/active session in the database.
     */
    std::string end_time;

    /**
     * @brief Client IP address (IPv4 or IPv6).
     */
    std::string client_ip;

    /**
     * @brief Client application identifier from handshake.
     */
    std::string client_identifier;

    /**
     * @brief Client protocol version major number.
     */
    int client_version_major = 0;

    /**
     * @brief Client protocol version minor number.
     */
    int client_version_minor = 0;

    /**
     * @brief Total bytes sent to client.
     */
    std::int64_t bytes_sent = 0;

    /**
     * @brief Total bytes received from client.
     */
    std::int64_t bytes_received = 0;

    /**
     * @brief ISO 3166-1 alpha-2 country code.
     */
    std::string country_code;

    /**
     * @brief Protocol used for this session (binary or http).
     */
    std::string protocol = "binary";
};

std::ostream& operator<<(std::ostream& s, const session_entity& v);

/**
 * @brief Entity for session statistics from continuous aggregates.
 */
struct session_statistics_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_session_stats_tbl";

    std::string day;
    std::string account_id;
    std::int64_t session_count = 0;
    double avg_duration_seconds = 0.0;
    std::int64_t total_bytes_sent = 0;
    std::int64_t total_bytes_received = 0;
};

std::ostream& operator<<(std::ostream& s, const session_statistics_entity& v);

/**
 * @brief Entity for a single time-series sample in the session samples hypertable.
 */
struct session_sample_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_session_samples_tbl";

    /**
     * @brief Session UUID — part of composite primary key.
     */
    sqlgen::PrimaryKey<std::string> session_id;

    /**
     * @brief Tenant identifier for multi-tenancy isolation (RLS).
     */
    std::string tenant_id;

    /**
     * @brief Sample timestamp — part of composite primary key and partition column.
     */
    sqlgen::PrimaryKey<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> sample_time;

    /**
     * @brief Cumulative bytes sent at sample time.
     */
    std::int64_t bytes_sent = 0;

    /**
     * @brief Cumulative bytes received at sample time.
     */
    std::int64_t bytes_received = 0;

    /**
     * @brief Round-trip time reported by the client in this ping, in milliseconds.
     */
    std::int64_t latency_ms = 0;
};

std::ostream& operator<<(std::ostream& s, const session_sample_entity& v);

}

#endif
