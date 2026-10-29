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
#include <locale>
#include <sstream>
#include <stdexcept>

namespace ores::platform::time {

std::string datetime::to_iso8601_utc(const std::chrono::system_clock::time_point& tp) {

    // The fraction is formatted here rather than left to strftime, which has no
    // sub-second field. The clock carries more than the microsecond the column
    // keeps, so the instant is taken down to the microsecond it falls in --
    // the rounding the binary codec applies -- so a value that reaches a row
    // through this text is the value it reaches through the row.
    const auto since_epoch = std::chrono::floor<std::chrono::microseconds>(tp.time_since_epoch());
    const auto whole = std::chrono::floor<std::chrono::seconds>(since_epoch);
    const auto fraction = since_epoch - whole;

    const auto time =
        std::chrono::system_clock::to_time_t(std::chrono::system_clock::time_point{whole});
    std::tm tm_buf;

    if (time_utils::gmtime_safe(&time, &tm_buf) == nullptr)
        throw std::runtime_error("to_iso8601_utc: failed to convert time_t to UTC tm");

    std::ostringstream oss;
    oss << std::put_time(&tm_buf, "%Y-%m-%d %H:%M:%S");
    oss << '.' << std::setw(6) << std::setfill('0') << fraction.count();
    oss << 'Z';
    return oss.str();
}

namespace {

/**
 * Parses text written in a strptime format as a UTC instant.
 *
 * The one place a timestamp is read into a std::tm, so no caller handles one.
 * The C locale is imbued so that a month name is always the English one.
 * @p shown is what an error quotes, which is the caller's own text and not the
 * trimmed copy this parses.
 */
std::chrono::system_clock::time_point parse_utc_format(const std::string& text,
                                                       const std::string& format,
                                                       const char* const who,
                                                       const std::string& shown) {
    std::tm tm = {};
    std::istringstream ss(text);
    ss.imbue(std::locale::classic());
    ss >> std::get_time(&tm, format.c_str());

    if (ss.fail())
        throw std::invalid_argument(std::string(who) + ": failed to parse: " + shown);

    return time_utils::to_time_point_utc(tm);
}

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

    // A decimal fraction carries up to six digits of microseconds. PostgreSQL
    // states it only when it is non-zero, so the dot is optional, and a caller
    // may supply fewer than six digits.
    auto whole = clean;
    std::chrono::microseconds fraction{0};
    if (const auto dot = clean.rfind('.'); dot != std::string::npos) {
        const auto digits = clean.substr(dot + 1);
        if (digits.empty() || digits.find_first_not_of("0123456789") != std::string::npos)
            throw std::invalid_argument(std::string(who) +
                                        ": malformed fractional seconds in: " + str);
        whole = clean.substr(0, dot);
        // Right-pad to the six digits a time_point is read back at; anything past
        // six is below the microsecond it keeps.
        const auto six =
            digits.size() >= 6 ? digits.substr(0, 6) : digits + std::string(6 - digits.size(), '0');
        fraction = std::chrono::microseconds{std::stoll(six)};
    }

    return parse_utc_format(whole, "%Y-%m-%d %H:%M:%S", who, str) + fraction;
}

}

std::chrono::system_clock::time_point datetime::from_iso8601_utc(const std::string& str) {
    return parse_utc_timestamp(str, true, "from_iso8601_utc");
}

std::chrono::system_clock::time_point datetime::from_db_string(const std::string& str) {
    return parse_utc_timestamp(str, false, "from_db_string");
}

std::chrono::system_clock::time_point datetime::from_utc_format(const std::string& str,
                                                                const std::string& format) {
    return parse_utc_format(str, format, "from_utc_format", str);
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
    // ISO 8601 states a calendar date in the extended form ("YYYY-MM-DD")
    // and in the basic form ("YYYYMMDD"); ORE documents use both.
    std::string extended = str;
    if (str.size() == 8 && str.find('-') == std::string::npos)
        extended = str.substr(0, 4) + "-" + str.substr(4, 2) + "-" + str.substr(6, 2);

    int yy{}, mm{}, dd{};
    char s1{}, s2{};
    std::istringstream ss(extended);
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
