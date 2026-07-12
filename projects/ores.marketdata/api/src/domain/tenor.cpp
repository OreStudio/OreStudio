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
#include "ores.marketdata.api/domain/tenor.hpp"
#include <charconv>
#include <stdexcept>

namespace ores::marketdata::domain {

namespace {

// Short-end labels are approximated as fixed day counts from the horizon
// date, rather than resolved against real spot-lag/holiday conventions
// (out of scope here — see class comment).
constexpr int overnight_days = 1;
constexpr int tom_next_days = 2;
constexpr int spot_next_days = 3;
constexpr int spot_week_days = 9; // spot (day 2) + one week.

int days_per_unit(tenor_unit unit) {
    switch (unit) {
    case tenor_unit::days: return 1;
    case tenor_unit::weeks: return 7;
    case tenor_unit::months: return 30;
    case tenor_unit::years: return 365;
    }
    throw std::invalid_argument("Unrecognised tenor_unit");
}

}

tenor tenor::parse(std::string_view label) {
    if (label == "O/N") return tenor(tenor_unit::days, overnight_days);
    if (label == "T/N") return tenor(tenor_unit::days, tom_next_days);
    if (label == "S/N") return tenor(tenor_unit::days, spot_next_days);
    if (label == "S/W") return tenor(tenor_unit::days, spot_week_days);

    if (label.size() < 2)
        throw std::invalid_argument("Tenor label too short: " + std::string(label));

    const char unitChar = label.back();
    tenor_unit unit;
    switch (unitChar) {
    case 'D': unit = tenor_unit::days; break;
    case 'W': unit = tenor_unit::weeks; break;
    case 'M': unit = tenor_unit::months; break;
    case 'Y': unit = tenor_unit::years; break;
    default:
        throw std::invalid_argument("Unrecognised tenor label: " + std::string(label));
    }

    const auto digits = label.substr(0, label.size() - 1);
    int count = 0;
    const auto result = std::from_chars(digits.data(), digits.data() + digits.size(), count);
    if (result.ec != std::errc() || result.ptr != digits.data() + digits.size() || count <= 0)
        throw std::invalid_argument("Unrecognised tenor label: " + std::string(label));

    return tenor(unit, count);
}

int tenor::approx_days() const { return count_ * days_per_unit(unit_); }

std::chrono::year_month_day tenor::end_date(std::chrono::year_month_day horizon) const {
    using namespace std::chrono;

    switch (unit_) {
    case tenor_unit::days:
        return year_month_day(sys_days(horizon) + days(count_));
    case tenor_unit::weeks:
        return year_month_day(sys_days(horizon) + weeks(count_));
    case tenor_unit::months: {
        auto shifted = horizon + months(count_);
        if (!shifted.ok())
            shifted = year_month_day_last(shifted.year() / shifted.month() / last);
        return shifted;
    }
    case tenor_unit::years: {
        auto shifted = horizon + years(count_);
        if (!shifted.ok())
            shifted = year_month_day_last(shifted.year() / shifted.month() / last);
        return shifted;
    }
    }
    throw std::invalid_argument("Unrecognised tenor_unit");
}

tenor_window resolve_window(std::chrono::year_month_day horizon, const tenor& t) {
    return tenor_window{horizon, t.end_date(horizon)};
}

bool windows_overlap(const tenor_window& a, const tenor_window& b) {
    return a.start < b.end && b.start < a.end;
}

}
