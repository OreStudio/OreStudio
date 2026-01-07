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
#ifndef ORES_TELEMETRY_DOMAIN_TELEMETRY_QUERY_HPP
#define ORES_TELEMETRY_DOMAIN_TELEMETRY_QUERY_HPP

#include <chrono>
#include <string>
#include <cstdint>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.telemetry/domain/telemetry_source.hpp"

namespace ores::telemetry::domain {

/**
 * @brief Query parameters for retrieving telemetry logs.
 *
 * All filter fields are optional. When not set, that filter is not applied.
 * Multiple filters are combined with AND logic.
 */
struct telemetry_query final {
    /**
     * @brief Start of the time range (inclusive).
     *
     * Required for efficient hypertable queries.
     */
    std::chrono::system_clock::time_point start_time;

    /**
     * @brief End of the time range (exclusive).
     *
     * Required for efficient hypertable queries.
     */
    std::chrono::system_clock::time_point end_time;

    /**
     * @brief Filter by source type (client or server).
     */
    std::optional<telemetry_source> source;

    /**
     * @brief Filter by source application name.
     */
    std::optional<std::string> source_name;

    /**
     * @brief Filter by session ID.
     *
     * Useful for debugging specific client sessions.
     */
    std::optional<boost::uuids::uuid> session_id;

    /**
     * @brief Filter by account ID.
     *
     * Useful for viewing logs from a specific user.
     */
    std::optional<boost::uuids::uuid> account_id;

    /**
     * @brief Filter by log level.
     *
     * Examples: "error", "warn", "info"
     */
    std::optional<std::string> level;

    /**
     * @brief Filter by minimum log level.
     *
     * When set, returns logs at this level or higher severity.
     * Severity order: trace < debug < info < warn < error
     */
    std::optional<std::string> min_level;

    /**
     * @brief Filter by component name.
     *
     * Supports prefix matching (e.g., "ores.comms" matches all comms logs).
     */
    std::optional<std::string> component;

    /**
     * @brief Filter by tag.
     */
    std::optional<std::string> tag;

    /**
     * @brief Search text in message body.
     *
     * Simple substring search, case-insensitive.
     */
    std::optional<std::string> message_contains;

    /**
     * @brief Maximum number of results to return.
     */
    std::uint32_t limit = 1000;

    /**
     * @brief Number of results to skip (for pagination).
     */
    std::uint32_t offset = 0;
};

/**
 * @brief Granularity for statistics queries.
 */
enum class stats_granularity {
    /**
     * @brief Hourly aggregation.
     */
    hourly = 0,

    /**
     * @brief Daily aggregation.
     */
    daily = 1
};

/**
 * @brief Query parameters for retrieving telemetry statistics.
 */
struct telemetry_stats_query final {
    /**
     * @brief Start of the time range (inclusive).
     */
    std::chrono::system_clock::time_point start_time;

    /**
     * @brief End of the time range (exclusive).
     */
    std::chrono::system_clock::time_point end_time;

    /**
     * @brief Aggregation granularity.
     */
    stats_granularity granularity = stats_granularity::daily;

    /**
     * @brief Filter by source type.
     */
    std::optional<telemetry_source> source;

    /**
     * @brief Filter by source application name.
     */
    std::optional<std::string> source_name;

    /**
     * @brief Filter by log level.
     */
    std::optional<std::string> level;

    /**
     * @brief Filter by component (daily stats only).
     */
    std::optional<std::string> component;
};

}

#endif
