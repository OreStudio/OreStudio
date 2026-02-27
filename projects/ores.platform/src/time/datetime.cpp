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

std::string datetime::format_time_point(
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

std::string datetime::format_time_point_utc(
    const std::chrono::system_clock::time_point& tp,
    const std::string& format) {

    const auto time = std::chrono::system_clock::to_time_t(tp);
    std::tm tm_buf;

    if (time_utils::gmtime_safe(&time, &tm_buf) == nullptr)
        return "Invalid time";

    std::ostringstream oss;
    oss << std::put_time(&tm_buf, format.c_str());
    return oss.str();
}

std::chrono::system_clock::time_point datetime::parse_time_point(
    const std::string& str,
    const std::string& format) {

    std::tm tm = {};
    std::istringstream ss(str);
    ss >> std::get_time(&tm, format.c_str());

    if (ss.fail()) {
        throw std::invalid_argument("Failed to parse time string: " + str);
    }

    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}

std::chrono::system_clock::time_point datetime::parse_time_point_utc(
    const std::string& str,
    const std::string& format) {

    std::tm tm = {};
    std::istringstream ss(str);
    ss >> std::get_time(&tm, format.c_str());

    if (ss.fail()) {
        throw std::invalid_argument("Failed to parse time string: " + str);
    }

    return std::chrono::system_clock::from_time_t(time_utils::timegm_safe(&tm));
}

}
