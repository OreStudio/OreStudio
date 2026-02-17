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
#ifndef ORES_QT_RELATIVE_TIME_HELPER_HPP
#define ORES_QT_RELATIVE_TIME_HELPER_HPP

#include <QString>
#include <chrono>
#include <string>

namespace ores::qt {

/**
 * @brief Helper class for formatting recorded_at timestamps as relative time.
 *
 * Provides convenience functions to convert ISO date/time strings to
 * human-readable relative time strings like "5 minutes ago" or "yesterday".
 */
class relative_time_helper final {
public:
    /**
     * @brief Formats a recorded_at timestamp string as relative time.
     *
     * Supports multiple input formats:
     * - ISO 8601: "2025-01-15T10:30:00"
     * - Date + time: "2025-01-15 10:30:00"
     * - Date only: "2025-01-15"
     *
     * @param recorded_at The timestamp string to format.
     * @return Relative time string (e.g., "5 minutes ago", "yesterday"),
     *         or the original string if parsing fails.
     */
    static QString format(const std::string& recorded_at);

    /**
     * @brief Formats a recorded_at QString as relative time.
     *
     * @param recorded_at The timestamp string to format.
     * @return Relative time string or the original string if parsing fails.
     */
    static QString format(const QString& recorded_at);

    /**
     * @brief Formats a time_point as relative time.
     *
     * @param recorded_at The time point to format.
     * @return Relative time string (e.g., "5 minutes ago", "yesterday").
     */
    static QString format(const std::chrono::system_clock::time_point& recorded_at);

    /**
     * @brief Always formats as relative time, regardless of global mode.
     */
    static QString format_relative(const QString& recorded_at);

    /**
     * @brief Always formats as relative time, regardless of global mode.
     */
    static QString format_relative(
        const std::chrono::system_clock::time_point& recorded_at);
};

}

#endif
