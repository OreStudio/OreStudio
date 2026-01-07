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
#ifndef ORES_TELEMETRY_DOMAIN_TELEMETRY_STATS_HPP
#define ORES_TELEMETRY_DOMAIN_TELEMETRY_STATS_HPP

#include <chrono>
#include <string>
#include <cstdint>
#include "ores.telemetry/domain/telemetry_source.hpp"

namespace ores::telemetry::domain {

/**
 * @brief Aggregated telemetry statistics for a time period.
 *
 * This structure represents pre-computed statistics from the TimescaleDB
 * continuous aggregates, enabling fast dashboard queries.
 */
struct telemetry_stats final {
    /**
     * @brief Start of the aggregation period.
     *
     * For hourly stats, this is the start of the hour.
     * For daily stats, this is the start of the day (00:00:00 UTC).
     */
    std::chrono::system_clock::time_point period_start;

    /**
     * @brief Source type (client or server).
     */
    telemetry_source source = telemetry_source::client;

    /**
     * @brief Source application name.
     *
     * May be empty for aggregate stats across all sources.
     */
    std::string source_name;

    /**
     * @brief Component name.
     *
     * Only populated for daily stats with component breakdown.
     * Empty for hourly stats or aggregate views.
     */
    std::string component;

    /**
     * @brief Log level.
     */
    std::string level;

    /**
     * @brief Total log count for this combination.
     */
    std::uint64_t log_count = 0;

    /**
     * @brief Number of unique sessions that generated logs.
     */
    std::uint32_t unique_sessions = 0;

    /**
     * @brief Number of unique accounts that generated logs.
     */
    std::uint32_t unique_accounts = 0;
};

/**
 * @brief Summary statistics for a telemetry overview.
 *
 * Used for dashboard widgets showing recent activity.
 */
struct telemetry_summary final {
    /**
     * @brief Time range start.
     */
    std::chrono::system_clock::time_point start_time;

    /**
     * @brief Time range end.
     */
    std::chrono::system_clock::time_point end_time;

    /**
     * @brief Total log count.
     */
    std::uint64_t total_logs = 0;

    /**
     * @brief Error log count.
     */
    std::uint64_t error_count = 0;

    /**
     * @brief Warning log count.
     */
    std::uint64_t warn_count = 0;

    /**
     * @brief Info log count.
     */
    std::uint64_t info_count = 0;

    /**
     * @brief Debug log count.
     */
    std::uint64_t debug_count = 0;

    /**
     * @brief Trace log count.
     */
    std::uint64_t trace_count = 0;

    /**
     * @brief Number of unique source applications.
     */
    std::uint32_t unique_sources = 0;

    /**
     * @brief Number of unique sessions.
     */
    std::uint32_t unique_sessions = 0;

    /**
     * @brief Number of unique accounts.
     */
    std::uint32_t unique_accounts = 0;
};

}

#endif
