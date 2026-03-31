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
#include <charconv>
#include <filesystem>
#include <format>
#include <stdexcept>
#include <string>
#include <string_view>
#include "ores.platform/time/time_utils.hpp"

namespace ores::platform::time {

std::tm* time_utils::gmtime_safe(const std::time_t* time, std::tm* result) {
#ifdef _WIN32
    // Windows: gmtime_s has reversed parameter order and returns errno_t
    if (gmtime_s(result, time) == 0) {
        return result;
    }
    return nullptr;
#else
    // POSIX: gmtime_r returns pointer to result
    return gmtime_r(time, result);
#endif
}

std::tm* time_utils::localtime_safe(const std::time_t* time, std::tm* result) {
#ifdef _WIN32
    // Windows: localtime_s has reversed parameter order and returns errno_t
    if (localtime_s(result, time) == 0) {
        return result;
    }
    return nullptr;
#else
    // POSIX: localtime_r returns pointer to result
    return localtime_r(time, result);
#endif
}

std::time_t time_utils::timegm_safe(std::tm* tm) {
#ifdef _WIN32
    return _mkgmtime(tm);
#else
    return timegm(tm);
#endif
}

std::chrono::system_clock::time_point time_utils::to_time_point_utc(std::tm tm) {
    return std::chrono::system_clock::from_time_t(timegm_safe(&tm));
}

std::chrono::system_clock::time_point time_utils::to_time_point_local(std::tm tm) {
    tm.tm_isdst = -1; // let mktime determine DST from the date/time fields
    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}

std::chrono::year_month_day time_utils::parse_date(std::string_view s) {
    auto parse_field = [](std::string_view sv, int& out) -> bool {
        const auto [ptr, ec] = std::from_chars(sv.data(), sv.data() + sv.size(), out);
        return ec == std::errc{} && ptr == sv.data() + sv.size();
    };

    int y = 0, m = 0, d = 0;

    if (s.size() == 8) {
        // YYYYMMDD
        if (!parse_field(s.substr(0, 4), y) ||
            !parse_field(s.substr(4, 2), m) ||
            !parse_field(s.substr(6, 2), d))
            throw std::invalid_argument("invalid date: " + std::string(s));
    } else if (s.size() == 10 && s[4] == '-' && s[7] == '-') {
        // YYYY-MM-DD
        if (!parse_field(s.substr(0, 4), y) ||
            !parse_field(s.substr(5, 2), m) ||
            !parse_field(s.substr(8, 2), d))
            throw std::invalid_argument("invalid date: " + std::string(s));
    } else {
        throw std::invalid_argument("unrecognised date format: " + std::string(s));
    }

    const std::chrono::year_month_day ymd{
        std::chrono::year{y},
        std::chrono::month{static_cast<unsigned>(m)},
        std::chrono::day{static_cast<unsigned>(d)}};
    if (!ymd.ok())
        throw std::invalid_argument("invalid date: " + std::string(s));
    return ymd;
}

std::string time_utils::format_date_compact(std::chrono::year_month_day date) {
    return std::format("{:04d}{:02d}{:02d}",
        static_cast<int>(date.year()),
        static_cast<unsigned>(date.month()),
        static_cast<unsigned>(date.day()));
}

std::string time_utils::format_date_iso(std::chrono::year_month_day date) {
    return std::format("{:04d}-{:02d}-{:02d}",
        static_cast<int>(date.year()),
        static_cast<unsigned>(date.month()),
        static_cast<unsigned>(date.day()));
}

std::chrono::system_clock::time_point
time_utils::file_time_to_system_clock(std::filesystem::file_time_type ft) {
    // std::chrono::clock_cast is C++20 but unimplemented on Apple libc++.
    // Compute the offset from file_clock::now() and apply it to
    // system_clock::now() — portable across all supported platforms.
    return std::chrono::system_clock::now() +
        std::chrono::duration_cast<std::chrono::system_clock::duration>(
            ft - std::filesystem::file_time_type::clock::now());
}

}
