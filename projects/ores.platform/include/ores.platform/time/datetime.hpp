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
#ifndef ORES_PLATFORM_TIME_DATETIME_HPP
#define ORES_PLATFORM_TIME_DATETIME_HPP

#include <chrono>
#include <string>

namespace ores::platform::time {

/**
 * @brief Canonical timestamp format string: "YYYY-MM-DD HH:MM:SS".
 *
 * Shared by all callers that need strftime/format format strings without a
 * timezone suffix (database storage format and local-time display format).
 */
inline constexpr const char* k_timestamp_format = "%Y-%m-%d %H:%M:%S";

/**
 * @brief Utilities for date and time operations.
 *
 * Policy: all timestamps are stored and transmitted in UTC. Local time is used
 * only for display. The canonical wire/storage format is ISO 8601 with a 'Z'
 * suffix: "YYYY-MM-DD HH:MM:SSZ".
 */
class datetime final {
public:
    /**
     * @brief Serialises a time point to ISO 8601 UTC string with 'Z' suffix.
     *
     * Output format: "YYYY-MM-DD HH:MM:SSZ"
     * Use this for all wire protocol and database writes.
     *
     * @param tp Time point to format (treated as UTC per C++20 definition).
     * @return ISO 8601 UTC string, always ending with 'Z'.
     */
    static std::string to_iso8601_utc(
        const std::chrono::system_clock::time_point& tp);

    /**
     * @brief Parses an ISO 8601 UTC string to a time point.
     *
     * Accepted UTC designators: 'Z', '+00', '+00:00'.
     * Throws std::invalid_argument if the designator is absent or the offset
     * is non-zero — callers must not pass ambiguous local-time strings.
     *
     * @param str String to parse. Must include a UTC designator.
     * @return Parsed time point.
     * @throws std::invalid_argument if the string cannot be parsed or has no
     *         UTC designator.
     */
    static std::chrono::system_clock::time_point from_iso8601_utc(
        const std::string& str);

    /**
     * @brief Formats a time point as a UTC database timestamp string.
     *
     * Output format: "YYYY-MM-DD HH:MM:SS" (no timezone suffix).
     * Use this for embedding timestamps directly in raw SQL statements.
     * Produces the same format as PostgreSQL returns for TIMESTAMPTZ columns
     * when the session timezone is UTC.
     *
     * @param tp Time point to format (treated as UTC per C++20 definition).
     * @return UTC timestamp string without timezone designator.
     */
    static std::string to_db_string(
        const std::chrono::system_clock::time_point& tp);

    /**
     * @brief Formats a time point as a human-readable local-time string.
     *
     * Intended for display only (UI, CLI output). Do NOT use for wire
     * protocol or database writes — use to_iso8601_utc() instead.
     *
     * @param tp Time point to format.
     * @param format strftime format string (default: k_timestamp_format).
     * @return Local-time string without timezone designator.
     */
    static std::string to_local_display_string(
        const std::chrono::system_clock::time_point& tp,
        const std::string& format = k_timestamp_format);
};

}

#endif
