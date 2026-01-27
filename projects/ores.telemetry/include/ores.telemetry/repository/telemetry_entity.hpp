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
#ifndef ORES_TELEMETRY_REPOSITORY_TELEMETRY_ENTITY_HPP
#define ORES_TELEMETRY_REPOSITORY_TELEMETRY_ENTITY_HPP

#include <string>
#include <cstdint>
#include <iosfwd>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::telemetry::repository {

/**
 * @brief Represents a telemetry log record in the database.
 *
 * This entity maps to the TimescaleDB telemetry_logs hypertable
 * partitioned by timestamp.
 */
struct telemetry_entity {
    constexpr static const char* schema = "production";
    constexpr static const char* tablename = "telemetry_logs_tbl";

    /**
     * @brief Log entry UUID - part of composite primary key.
     */
    sqlgen::PrimaryKey<std::string> id;

    /**
     * @brief Log timestamp - part of composite primary key.
     *
     * Required in primary key for TimescaleDB hypertable partitioning.
     */
    sqlgen::PrimaryKey<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> timestamp;

    /**
     * @brief Source type ('client' or 'server').
     */
    std::string source;

    /**
     * @brief Source application name.
     */
    std::string source_name;

    /**
     * @brief Session ID (empty string for no session).
     *
     * Uses std::string instead of std::optional because sqlgen doesn't
     * support optional types in operations.
     */
    std::string session_id;

    /**
     * @brief Account ID (empty string for no account).
     */
    std::string account_id;

    /**
     * @brief Log severity level.
     */
    std::string level;

    /**
     * @brief Logger/component name.
     */
    std::string component;

    /**
     * @brief Log message body.
     */
    std::string message;

    /**
     * @brief Optional tag for filtering.
     */
    std::string tag;

    /**
     * @brief Server receipt timestamp.
     */
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> recorded_at;
};

std::ostream& operator<<(std::ostream& s, const telemetry_entity& v);

/**
 * @brief Entity for hourly telemetry statistics from continuous aggregates.
 */
struct telemetry_stats_hourly_entity {
    constexpr static const char* schema = "production";
    constexpr static const char* tablename = "telemetry_stats_hourly_vw";

    std::string hour;
    std::string source;
    std::string source_name;
    std::string level;
    std::int64_t log_count = 0;
    std::int64_t unique_sessions = 0;
    std::int64_t unique_accounts = 0;
};

std::ostream& operator<<(std::ostream& s, const telemetry_stats_hourly_entity& v);

/**
 * @brief Entity for daily telemetry statistics from continuous aggregates.
 */
struct telemetry_stats_daily_entity {
    constexpr static const char* schema = "production";
    constexpr static const char* tablename = "telemetry_stats_daily_vw";

    std::string day;
    std::string source;
    std::string source_name;
    std::string component;
    std::string level;
    std::int64_t log_count = 0;
    std::int64_t unique_sessions = 0;
    std::int64_t unique_accounts = 0;
};

std::ostream& operator<<(std::ostream& s, const telemetry_stats_daily_entity& v);

}

#endif
