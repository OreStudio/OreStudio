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
};

}

#endif
