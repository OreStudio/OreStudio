/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DATABASE_REPOSITORY_RANGE_BOUND_HPP
#define ORES_DATABASE_REPOSITORY_RANGE_BOUND_HPP

#include "ores.database/repository/valid_at.hpp"
#include <chrono>
#include <exception>
#include <optional>
#include <regex>
#include <string>

namespace ores::database::repository {

/**
 * @brief Which end of a range a bound states.
 */
enum class range_end { from, to };

/**
 * @brief Whether the first ten characters of @p text are a date that exists.
 *
 * The parser of an instant normalises a day past the end of its month, so the
 * calendar decides here. The database has no year 0, though the time library
 * has, so a year below 1 is not a date.
 */
inline bool is_calendar_date(const std::string& text) {
    if (text.size() < 10)
        return false;
    const auto number = [&](std::size_t at, std::size_t n) {
        return std::stoi(text.substr(at, n));
    };
    try {
        using namespace std::chrono;
        if (number(0, 4) < 1)
            return false;
        return year_month_day{year{number(0, 4)},
                              month{static_cast<unsigned>(number(5, 2))},
                              day{static_cast<unsigned>(number(8, 2))}}
            .ok();
    } catch (const std::exception&) {
        return false;
    }
}

/**
 * @brief The bound a range member states, as text the database reads.
 *
 * A column of a date type takes a date, which is the bound itself. A column of
 * a timestamp type takes a UTC timestamp, which is the bound itself, or a date,
 * which is the start of that day for the lower end and the last microsecond of
 * that day for the upper end, so that "up to the 5th" holds the whole of the
 * 5th. A text that is none of these is not a bound, and neither is a date that
 * is not on the calendar.
 *
 * @return The text to compare against, or nothing when @p text is not a bound.
 */
inline std::optional<std::string>
range_bound(const std::string& text, range_end end, bool timestamp_column) {
    static const std::regex date_form(R"(\d{4}-\d{2}-\d{2})");
    if (std::regex_match(text, date_form)) {
        if (!is_calendar_date(text))
            return std::nullopt;
        if (!timestamp_column)
            return text;
        return end == range_end::from ? text + " 00:00:00+00" : text + " 23:59:59.999999+00";
    }
    if (!timestamp_column)
        return std::nullopt;
    const auto instant = parse_as_of(text);
    if (!instant || !is_calendar_date(*instant))
        return std::nullopt;
    return instant;
}

}

#endif
