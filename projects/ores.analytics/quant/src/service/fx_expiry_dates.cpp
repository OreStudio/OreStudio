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
#include "ores.analytics.quant/service/fx_expiry_dates.hpp"
#include "ores.analytics.quant/service/calendar_arithmetic.hpp"
#include <algorithm>
#include <charconv>
#include <utility>

namespace ores::analytics::quant::service {

fx_expiry_date_rule::fx_expiry_date_rule(domain::business_day_convention roll,
                                         bool end_of_month,
                                         std::size_t calendar_index,
                                         domain::business_day_calendar_set calendars)
    : roll_(roll)
    , end_of_month_(end_of_month)
    , calendar_index_(calendar_index)
    , calendars_(std::move(calendars)) {}

std::chrono::year_month_day fx_expiry_date_rule::expiry(std::chrono::year_month_day spot_date,
                                                        domain::date_period period) const {
    return calendar_arithmetic::advance(
        spot_date, period, roll_, end_of_month_, calendar_index_, calendars_);
}

std::chrono::year_month_day fx_expiry_date_rule::business_day(std::chrono::year_month_day from,
                                                              int n) const {
    return expiry(from, {.unit = domain::period_unit::day, .multiplier = n});
}

std::chrono::year_month_day fx_expiry_date_rule::horizon(std::chrono::year_month_day as_of) const {
    return calendar_arithmetic::adjust(as_of, roll_, calendar_index_, calendars_);
}

std::chrono::year_month_day fx_expiry_date_rule::spot_date(std::chrono::year_month_day as_of,
                                                           int spot_days) const {
    return business_day(horizon(as_of), spot_days);
}

std::optional<double> fx_vol_tenor_years(std::string_view tenor) {
    // A tenor is a count of one or two digits and one unit, as the column's check states.
    if (tenor.size() < 2 || tenor.size() > 3)
        return std::nullopt;
    const char unit = tenor.back();
    if (unit != 'D' && unit != 'W' && unit != 'M' && unit != 'Y')
        return std::nullopt;
    if (!std::all_of(tenor.begin(), tenor.end() - 1, [](char c) { return c >= '0' && c <= '9'; }))
        return std::nullopt;
    int count = 0;
    const auto parsed = std::from_chars(tenor.data(), tenor.data() + tenor.size() - 1, count);
    if (parsed.ec != std::errc{})
        return std::nullopt;
    const double n = static_cast<double>(count);
    const double years = unit == 'D' ? n / 365.0 :
                         unit == 'W' ? 7.0 * n / 365.0 :
                         unit == 'M' ? n / 12.0 :
                                       n;
    if (!(years > 0.0))
        return std::nullopt;
    return years;
}

}
