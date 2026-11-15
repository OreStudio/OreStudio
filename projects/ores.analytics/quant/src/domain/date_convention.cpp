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
#include "ores.analytics.quant/domain/date_convention.hpp"
#include <utility>

namespace ores::analytics::quant::domain {

namespace {

bool counts_months(period_unit unit) noexcept {
    return unit == period_unit::month || unit == period_unit::year;
}

int months_of(date_period p) noexcept {
    return p.unit == period_unit::year ? 12 * p.multiplier : p.multiplier;
}

/// The fewest and the most days the period can span.
std::pair<int, int> day_bounds(date_period p) noexcept {
    switch (p.unit) {
        case period_unit::day:
            return {p.multiplier, p.multiplier};
        case period_unit::week:
            return {7 * p.multiplier, 7 * p.multiplier};
        case period_unit::month:
            return {28 * p.multiplier, 31 * p.multiplier};
        case period_unit::year:
            return {365 * p.multiplier, 366 * p.multiplier};
        case period_unit::none:
            break;
    }
    return {0, 0};
}

}

std::partial_ordering compare_periods(date_period a, date_period b) noexcept {
    if (a.is_empty() || b.is_empty())
        return a.is_empty() && b.is_empty() ? std::partial_ordering::equivalent :
                                              std::partial_ordering::unordered;
    if (counts_months(a.unit) == counts_months(b.unit))
        return counts_months(a.unit) ? months_of(a) <=> months_of(b) :
                                       day_bounds(a).first <=> day_bounds(b).first;
    const auto [a_min, a_max] = day_bounds(a);
    const auto [b_min, b_max] = day_bounds(b);
    if (a_max < b_min)
        return std::partial_ordering::less;
    if (a_min > b_max)
        return std::partial_ordering::greater;
    return std::partial_ordering::unordered;
}

} // namespace ores::analytics::quant::domain
