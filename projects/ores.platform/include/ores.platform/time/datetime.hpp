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
 * @brief Utilities for date and time operations.
 */
class datetime final {
public:
    /**
     * @brief Formats a time point as a string using local time.
     *
     * @param tp Time point to format.
     * @param format Format string (e.g., "%Y-%m-%d %H:%M:%S").
     * @return Formatted string representation of the time point.
     */
    static std::string format_time_point(
        const std::chrono::system_clock::time_point& tp,
        const std::string& format = "%Y-%m-%d %H:%M:%S");

    /**
     * @brief Formats a time point as a string using UTC time.
     *
     * @param tp Time point to format.
     * @param format Format string (e.g., "%Y-%m-%d %H:%M:%S").
     * @return Formatted string representation of the time point in UTC.
     */
    static std::string format_time_point_utc(
        const std::chrono::system_clock::time_point& tp,
        const std::string& format = "%Y-%m-%d %H:%M:%S");

    /**
     * @brief Parses a string into a time point.
     *
     * @param str String to parse.
     * @param format Format string (e.g., "%Y-%m-%d %H:%M:%S").
     * @return Parsed time point.
     */
    static std::chrono::system_clock::time_point parse_time_point(
        const std::string& str,
        const std::string& format = "%Y-%m-%d %H:%M:%S");
};

}

#endif
