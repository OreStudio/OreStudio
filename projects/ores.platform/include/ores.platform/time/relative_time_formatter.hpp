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
#ifndef ORES_PLATFORM_TIME_RELATIVE_TIME_FORMATTER_HPP
#define ORES_PLATFORM_TIME_RELATIVE_TIME_FORMATTER_HPP

#include <chrono>
#include <string>

namespace ores::platform::time {

/**
 * @brief Enumeration for relative time formatting style.
 *
 * Based on ECMAScript Intl.RelativeTimeFormat numeric option.
 */
enum class numeric_style {
    /**
     * @brief Always use numeric values (e.g., "1 day ago", "in 5 days").
     */
    always,

    /**
     * @brief Use special words where appropriate (e.g., "yesterday", "tomorrow").
     */
    automatic
};

/**
 * @brief Enumeration for time units used in relative time formatting.
 *
 * Based on ECMAScript Intl.RelativeTimeFormat units.
 */
enum class time_unit {
    second,
    minute,
    hour,
    day,
    week,
    month,
    quarter,
    year
};

/**
 * @brief Formats time points as relative time strings.
 *
 * Inspired by ECMAScript Intl.RelativeTimeFormat, this class provides
 * human-readable relative time strings like "just now", "5 minutes ago",
 * "in 3 days", etc.
 *
 * @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/RelativeTimeFormat
 */
class relative_time_formatter final {
public:
    /**
     * @brief Constructs a formatter with the specified numeric style.
     *
     * @param style The numeric style to use for formatting.
     */
    explicit relative_time_formatter(numeric_style style = numeric_style::automatic);

    /**
     * @brief Formats a time point relative to now.
     *
     * @param tp The time point to format.
     * @return Human-readable relative time string.
     */
    std::string format(const std::chrono::system_clock::time_point& tp) const;

    /**
     * @brief Formats a time point relative to a reference time.
     *
     * @param tp The time point to format.
     * @param reference The reference time point to compare against.
     * @return Human-readable relative time string.
     */
    std::string format(
        const std::chrono::system_clock::time_point& tp,
        const std::chrono::system_clock::time_point& reference) const;

    /**
     * @brief Formats a numeric value and unit as a relative time string.
     *
     * Based on ECMAScript Intl.RelativeTimeFormat.format().
     *
     * @param value The numeric value (positive for future, negative for past).
     * @param unit The time unit.
     * @return Human-readable relative time string.
     */
    std::string format(long long value, time_unit unit) const;

private:
    numeric_style style_;
};

}

#endif
