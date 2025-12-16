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
#include "ores.utility/datetime/relative_time_formatter.hpp"

#include <cmath>

namespace ores::utility::datetime {

namespace {

constexpr long long seconds_per_minute = 60;
constexpr long long seconds_per_hour = 3600;
constexpr long long seconds_per_day = 86400;
constexpr long long seconds_per_week = 604800;
constexpr long long seconds_per_month = 2629746; // Average month (30.44 days)
constexpr long long seconds_per_quarter = 7889238; // 3 months
constexpr long long seconds_per_year = 31556952; // Average year (365.24 days)

std::string unit_to_string(time_unit unit, bool plural) {
    switch (unit) {
    case time_unit::second:
        return plural ? "seconds" : "second";
    case time_unit::minute:
        return plural ? "minutes" : "minute";
    case time_unit::hour:
        return plural ? "hours" : "hour";
    case time_unit::day:
        return plural ? "days" : "day";
    case time_unit::week:
        return plural ? "weeks" : "week";
    case time_unit::month:
        return plural ? "months" : "month";
    case time_unit::quarter:
        return plural ? "quarters" : "quarter";
    case time_unit::year:
        return plural ? "years" : "year";
    }
    return "";
}

std::string format_numeric(long long value, time_unit unit) {
    const auto abs_value = std::abs(value);
    const bool plural = abs_value != 1;
    const auto unit_str = unit_to_string(unit, plural);

    if (value < 0) {
        return std::to_string(abs_value) + " " + unit_str + " ago";
    } else if (value > 0) {
        return "in " + std::to_string(abs_value) + " " + unit_str;
    }
    return "now";
}

std::string format_automatic(long long value, time_unit unit) {
    // Handle special cases for automatic style
    if (value == 0) {
        switch (unit) {
        case time_unit::second:
        case time_unit::minute:
            return "now";
        case time_unit::hour:
            return "this hour";
        case time_unit::day:
            return "today";
        case time_unit::week:
            return "this week";
        case time_unit::month:
            return "this month";
        case time_unit::quarter:
            return "this quarter";
        case time_unit::year:
            return "this year";
        }
    }

    // Handle -1 and +1 special cases
    if (value == -1) {
        switch (unit) {
        case time_unit::second:
            return "1 second ago";
        case time_unit::minute:
            return "1 minute ago";
        case time_unit::hour:
            return "1 hour ago";
        case time_unit::day:
            return "yesterday";
        case time_unit::week:
            return "last week";
        case time_unit::month:
            return "last month";
        case time_unit::quarter:
            return "last quarter";
        case time_unit::year:
            return "last year";
        }
    }

    if (value == 1) {
        switch (unit) {
        case time_unit::second:
            return "in 1 second";
        case time_unit::minute:
            return "in 1 minute";
        case time_unit::hour:
            return "in 1 hour";
        case time_unit::day:
            return "tomorrow";
        case time_unit::week:
            return "next week";
        case time_unit::month:
            return "next month";
        case time_unit::quarter:
            return "next quarter";
        case time_unit::year:
            return "next year";
        }
    }

    // Fall back to numeric format for other values
    return format_numeric(value, unit);
}

}

relative_time_formatter::relative_time_formatter(numeric_style style)
    : style_(style) {}

std::string relative_time_formatter::format(
    const std::chrono::system_clock::time_point& tp) const {
    return format(tp, std::chrono::system_clock::now());
}

std::string relative_time_formatter::format(
    const std::chrono::system_clock::time_point& tp,
    const std::chrono::system_clock::time_point& reference) const {
    using namespace std::chrono;

    const auto diff = tp - reference;
    const auto seconds = duration_cast<std::chrono::seconds>(diff).count();

    // Determine the appropriate unit based on the magnitude of the difference
    const auto abs_seconds = std::abs(seconds);
    const int sign = seconds >= 0 ? 1 : -1;

    if (abs_seconds < seconds_per_minute) {
        // Less than a minute
        if (abs_seconds < 10 && style_ == numeric_style::automatic) {
            return "just now";
        }
        return format(sign * static_cast<long long>(abs_seconds), time_unit::second);
    }

    if (abs_seconds < seconds_per_hour) {
        // Less than an hour
        const auto minutes = abs_seconds / seconds_per_minute;
        return format(sign * static_cast<long long>(minutes), time_unit::minute);
    }

    if (abs_seconds < seconds_per_day) {
        // Less than a day
        const auto hours = abs_seconds / seconds_per_hour;
        return format(sign * static_cast<long long>(hours), time_unit::hour);
    }

    if (abs_seconds < seconds_per_week) {
        // Less than a week
        const auto days = abs_seconds / seconds_per_day;
        return format(sign * static_cast<long long>(days), time_unit::day);
    }

    if (abs_seconds < seconds_per_month) {
        // Less than a month
        const auto weeks = abs_seconds / seconds_per_week;
        return format(sign * static_cast<long long>(weeks), time_unit::week);
    }

    if (abs_seconds < seconds_per_year) {
        // Less than a year
        const auto months = abs_seconds / seconds_per_month;
        return format(sign * static_cast<long long>(months), time_unit::month);
    }

    // One year or more
    const auto years = abs_seconds / seconds_per_year;
    return format(sign * static_cast<long long>(years), time_unit::year);
}

std::string relative_time_formatter::format(long long value, time_unit unit) const {
    if (style_ == numeric_style::automatic) {
        return format_automatic(value, unit);
    }
    return format_numeric(value, unit);
}

}
