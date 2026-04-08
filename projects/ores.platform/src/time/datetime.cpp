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
#include <iomanip>
#include <sstream>
#include <stdexcept>

namespace ores::platform::time {

std::string datetime::to_iso8601_utc(
    const std::chrono::system_clock::time_point& tp) {

    const auto time = std::chrono::system_clock::to_time_t(tp);
    std::tm tm_buf;

    if (time_utils::gmtime_safe(&time, &tm_buf) == nullptr)
        throw std::runtime_error("to_iso8601_utc: failed to convert time_t to UTC tm");

    std::ostringstream oss;
    oss << std::put_time(&tm_buf, "%Y-%m-%d %H:%M:%S");
    oss << 'Z';
    return oss.str();
}

std::chrono::system_clock::time_point datetime::from_iso8601_utc(
    const std::string& str) {

    if (str.empty())
        throw std::invalid_argument("from_iso8601_utc: empty string");

    // Strip recognised UTC designators: 'Z', '+00:00', '+00'.
    // Anything else — including a missing designator — is rejected.
    std::string clean;
    if (str.back() == 'Z') {
        clean = str.substr(0, str.size() - 1);
    } else if (str.size() >= 6 && str.substr(str.size() - 6) == "+00:00") {
        clean = str.substr(0, str.size() - 6);
    } else if (str.size() >= 3 && str.substr(str.size() - 3) == "+00") {
        clean = str.substr(0, str.size() - 3);
    } else {
        throw std::invalid_argument(
            "from_iso8601_utc: missing UTC designator (Z, +00:00, or +00) in: " + str);
    }

    // Trim any trailing space that PostgreSQL may insert before the offset.
    while (!clean.empty() && clean.back() == ' ')
        clean.pop_back();

    std::tm tm = {};
    std::istringstream ss(clean);
    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");

    if (ss.fail())
        throw std::invalid_argument("from_iso8601_utc: failed to parse: " + str);

    return time_utils::to_time_point_utc(tm);
}

std::string datetime::to_db_string(
    const std::chrono::system_clock::time_point& tp) {

    const auto s = to_iso8601_utc(tp);
    return s.substr(0, s.size() - 1); // strip the Z suffix
}

std::string datetime::to_local_display_string(
    const std::chrono::system_clock::time_point& tp,
    const std::string& format) {

    const auto time = std::chrono::system_clock::to_time_t(tp);
    std::tm tm_buf;

    if (time_utils::localtime_safe(&time, &tm_buf) == nullptr)
        return "Invalid time";

    std::ostringstream oss;
    oss << std::put_time(&tm_buf, format.c_str());
    return oss.str();
}

}
