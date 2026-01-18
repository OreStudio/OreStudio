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
#ifndef ORES_UTILITY_FAKER_DATETIME_HPP
#define ORES_UTILITY_FAKER_DATETIME_HPP

#include <chrono>
#include <string>

namespace ores::utility::faker {

/**
 * @brief Generates fake datetime values for testing purposes.
 */
class datetime final {
public:
    /**
     * @brief Generates a random past datetime as a time_point.
     *
     * Returns a random datetime between 1970-01-01 and 2038-12-31.
     */
    static std::chrono::system_clock::time_point past_timepoint();

    /**
     * @brief Generates a random past datetime as a formatted string.
     *
     * Returns a string in the format "YYYY-MM-DD HH:MM:SS".
     */
    static std::string past_string();

    /**
     * @brief Creates a time_point from date/time components.
     *
     * Uses C++20's timezone-agnostic sys_days for deterministic results
     * across different environments.
     *
     * @param year The year (e.g., 2025)
     * @param month The month (1-12)
     * @param day The day (1-31)
     * @param hour The hour (0-23, default 0)
     * @param min The minute (0-59, default 0)
     * @param sec The second (0-59, default 0)
     * @return A time_point representing the specified UTC datetime.
     */
    static std::chrono::system_clock::time_point make_timepoint(
        int year, int month, int day, int hour = 0, int min = 0, int sec = 0);
};

}

#endif
