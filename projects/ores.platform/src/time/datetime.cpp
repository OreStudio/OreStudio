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
#include "ores.platform/time/datetime.hpp"
#include "ores.platform/time/time_utils.hpp"
#include <ctime>
#include <format>
#include <iomanip>
#include <sstream>
#include <stdexcept>

namespace ores::platform::time {

std::string datetime::to_iso8601_utc(const std::chrono::system_clock::time_point& tp) {

    const auto time = std::chrono::system_clock::to_time_t(tp);
    std::tm tm_buf;

    if (time_utils::gmtime_safe(&time, &tm_buf) == nullptr)
        throw std::runtime_error("to_iso8601_utc: failed to convert time_t to UTC tm");

    std::ostringstream oss;
    oss << std::put_time(&tm_buf, "%Y-%m-%d %H:%M:%S");
    oss << 'Z';
    return oss.str();
}

namespace {

/**
 * Parses a UTC timestamp carrying either separator.
 *
 * The designator is the one thing the two forms differ on: the wire form always
 * states it, and a timestamp read back out of a text column never does, because
 * to_db_string writes the form PostgreSQL returns and that form has none. A
 * caller that must not accept an ambiguous local time requires it; a caller
 * reading a column cannot.
 */
std::chrono::system_clock::time_point
parse_utc_timestamp(const std::string& str, const bool designator_required, const char* const who) {
    if (str.empty())
        throw std::invalid_argument(std::string(who) + ": empty string");

    std::string clean = str;
    if (clean.back() == 'Z') {
        clean = clean.substr(0, clean.size() - 1);
    } else if (clean.size() >= 6 && clean.substr(clean.size() - 6) == "+00:00") {
        clean = clean.substr(0, clean.size() - 6);
    } else if (clean.size() >= 3 && clean.substr(clean.size() - 3) == "+00") {
        clean = clean.substr(0, clean.size() - 3);
    } else if (designator_required) {
        throw std::invalid_argument(std::string(who) +
                                    ": missing UTC designator (Z, +00:00, or +00) in: " + str);
    }

    // PostgreSQL may insert a trailing space before the offset.
    while (!clean.empty() && clean.back() == ' ')
        clean.pop_back();

    // Callers may supply either the ISO 8601 'T' separator or the relaxed space.
    if (clean.size() > 10 && clean[10] == 'T')
        clean[10] = ' ';

    std::tm tm = {};
    std::istringstream ss(clean);
    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");

    if (ss.fail())
        throw std::invalid_argument(std::string(who) + ": failed to parse: " + str);

    return time_utils::to_time_point_utc(tm);
}

}

std::chrono::system_clock::time_point datetime::from_iso8601_utc(const std::string& str) {
    return parse_utc_timestamp(str, true, "from_iso8601_utc");
}

std::chrono::system_clock::time_point datetime::from_db_string(const std::string& str) {
    return parse_utc_timestamp(str, false, "from_db_string");
}

std::string datetime::to_db_string(const std::chrono::system_clock::time_point& tp) {
    const auto s = to_iso8601_utc(tp);
    return s.substr(0, s.size() - 1);
}

std::string datetime::to_local_display_string(const std::chrono::system_clock::time_point& tp,
                                              const std::string& format) {
    const auto time = std::chrono::system_clock::to_time_t(tp);
    std::tm tm_buf;

    if (time_utils::localtime_safe(&time, &tm_buf) == nullptr)
        return "Invalid time";

    std::ostringstream oss;
    oss << std::put_time(&tm_buf, format.c_str());
    return oss.str();
}

std::string datetime::to_iso8601_date(const std::chrono::year_month_day& date) {
    return std::format("{:%Y-%m-%d}", date);
}

std::chrono::year_month_day datetime::from_iso8601_date(const std::string& str) {
    int yy{}, mm{}, dd{};
    char s1{}, s2{};
    std::istringstream ss(str);
    ss >> yy >> s1 >> mm >> s2 >> dd;

    if (ss.fail() || s1 != '-' || s2 != '-')
        throw std::invalid_argument("from_iso8601_date: failed to parse: " + str);

    const auto date = std::chrono::year{yy} / std::chrono::month{static_cast<unsigned>(mm)} /
                      std::chrono::day{static_cast<unsigned>(dd)};
    if (!date.ok())
        throw std::invalid_argument("from_iso8601_date: not a valid calendar date: " + str);

    return date;
}

}
