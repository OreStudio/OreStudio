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
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include <magic_enum/magic_enum.hpp>
#include <stdexcept>

namespace ores::analytics::quant::service {

namespace {

long days_between(std::chrono::year_month_day start, std::chrono::year_month_day end) {
    return (std::chrono::sys_days(end) - std::chrono::sys_days(start)).count();
}

// US (bond-basis) 30/360: each month treated as 30 days, year as 360. The
// end-of-month adjustment rule below is the common simplified form (not the
// full ISDA 30/360 ruleset, which also special-cases February).
double thirty_three_sixty(std::chrono::year_month_day start, std::chrono::year_month_day end) {
    int y1 = int(start.year()), m1 = unsigned(start.month()), d1 = unsigned(start.day());
    int y2 = int(end.year()), m2 = unsigned(end.month()), d2 = unsigned(end.day());

    if (d1 == 31)
        d1 = 30;
    if (d2 == 31 && d1 == 30)
        d2 = 30;

    const double days360 = (y2 - y1) * 360.0 + (m2 - m1) * 30.0 + (d2 - d1);
    return days360 / 360.0;
}

}

double day_count_calculator::year_fraction(std::chrono::year_month_day start,
                                           std::chrono::year_month_day end,
                                           day_count_convention_code convention) {
    if (!start.ok() || !end.ok())
        throw std::invalid_argument("day_count_calculator: start/end must be valid calendar dates");
    if (std::chrono::sys_days(end) < std::chrono::sys_days(start))
        throw std::invalid_argument("day_count_calculator: end must not be before start");

    switch (convention) {
    case day_count_convention_code::A360:
        return static_cast<double>(days_between(start, end)) / 360.0;
    case day_count_convention_code::A365:
    case day_count_convention_code::A365F:
        return static_cast<double>(days_between(start, end)) / 365.0;
    case day_count_convention_code::THIRTY_360:
        return thirty_three_sixty(start, end);
    }
    throw std::invalid_argument("day_count_calculator: unrecognized day_count_convention_code");
}

day_count_convention_code parse_day_count_convention_code(const std::string& code) {
    // "30/360" cannot be an enumerator name ("/" is invalid), so it is
    // mapped by hand; every other code matches its enumerator exactly and
    // is delegated to magic_enum.
    if (code == "30/360")
        return day_count_convention_code::THIRTY_360;

    const auto parsed = magic_enum::enum_cast<day_count_convention_code>(code);
    if (!parsed)
        throw std::invalid_argument(
            "parse_day_count_convention_code: unsupported convention '" + code +
            "' -- only A360, A365, A365F, and 30/360 are implemented");
    return *parsed;
}

}
