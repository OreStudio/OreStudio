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
#include <chrono>
#include <filesystem>

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

    /**
     * @brief Converts a UTC tm struct to time_t (cross-platform).
     *
     * This is a cross-platform wrapper around timegm (POSIX) and
     * _mkgmtime (Windows). Unlike std::mktime, this interprets the
     * tm struct as UTC rather than local time.
     *
     * @param tm Pointer to tm struct to convert (interpreted as UTC).
     * @return The corresponding time_t value.
     */
    static std::time_t timegm_safe(std::tm* tm);

    /**
     * @brief Converts a UTC tm struct directly to a system_clock::time_point.
     *
     * Interprets the tm fields as UTC. Use this when the source data is
     * known to be in UTC (e.g. a value already normalised to UTC before
     * storing in tm).
     *
     * @param tm The tm struct to convert (interpreted as UTC, passed by value
     *           because timegm may normalise the fields).
     * @return The corresponding system_clock::time_point.
     */
    static std::chrono::system_clock::time_point
    to_time_point_utc(std::tm tm);

    /**
     * @brief Converts a local-time tm struct directly to a
     *        system_clock::time_point.
     *
     * Interprets the tm fields as the process's local timezone (via
     * std::mktime), then converts to UTC. Use this when the source data
     * is in the session/system timezone — e.g. a timestamp returned by
     * libpq whose offset has been stripped by rfl::Timestamp::strptime.
     *
     * @param tm The tm struct to convert (passed by value because mktime
     *           may normalise DST and other fields).
     * @return The corresponding system_clock::time_point.
     */
    static std::chrono::system_clock::time_point
    to_time_point_local(std::tm tm);

    /**
     * @brief Converts a filesystem file_time_type to a system_clock::time_point.
     *
     * std::chrono::clock_cast is the C++20 standard way to do this but is not
     * implemented on all platforms (notably Apple libc++). This uses the
     * now()-based portable alternative that works on all supported platforms
     * without making epoch assumptions.
     *
     * @param ft The file time to convert.
     * @return The equivalent system_clock::time_point.
     */
    static std::chrono::system_clock::time_point
    file_time_to_system_clock(std::filesystem::file_time_type ft);
};

}

#endif
