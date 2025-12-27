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
#ifndef ORES_PLATFORM_TIME_TIME_UTILS_HPP
#define ORES_PLATFORM_TIME_TIME_UTILS_HPP

#include <ctime>

namespace ores::platform::time {

/**
 * @brief Cross-platform time utilities.
 */
class time_utils final {
public:
    /**
     * @brief Converts time_t to UTC tm struct (cross-platform).
     *
     * This is a cross-platform wrapper around gmtime_r (POSIX) and
     * gmtime_s (Windows).
     *
     * @param time The time_t value to convert.
     * @param result Pointer to tm struct to store the result.
     * @return Pointer to result on success, nullptr on failure.
     */
    static std::tm* gmtime_safe(const std::time_t* time, std::tm* result);

    /**
     * @brief Converts time_t to local tm struct (cross-platform).
     *
     * This is a cross-platform wrapper around localtime_r (POSIX) and
     * localtime_s (Windows).
     *
     * @param time The time_t value to convert.
     * @param result Pointer to tm struct to store the result.
     * @return Pointer to result on success, nullptr on failure.
     */
    static std::tm* localtime_safe(const std::time_t* time, std::tm* result);
};

}

#endif
