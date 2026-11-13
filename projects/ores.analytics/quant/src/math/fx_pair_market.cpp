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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/math/fx_pair_market.hpp"
#include <cmath>
#include <format>

namespace ores::analytics::quant::math {

namespace {

using days = std::chrono::sys_days;

int days_between(std::chrono::year_month_day from, std::chrono::year_month_day to) {
    return static_cast<int>((days(to) - days(from)).count());
}

double lerp_discount(double before, double after, double share) {
    return std::exp(std::log(before) + share * (std::log(after) - std::log(before)));
}

}

std::expected<fx_market, std::string> pair_market_at(double spot,
                                                     std::chrono::year_month_day spot_date,
                                                     std::span<const fx_discount_point> points,
                                                     std::chrono::year_month_day value_date,
                                                     std::string context) {
    const auto prefix = context.empty() ? std::string{} : context + ": ";
    if (!(spot > 0.0))
        return std::unexpected(prefix + "the spot is not above zero");
    if (points.empty())
        return std::unexpected(prefix + "the forward set has no row");
    if (days(value_date) < days(spot_date))
        return std::unexpected(prefix + "the value date " + std::format("{}", value_date) +
                               " is before the spot date " + std::format("{}", spot_date));

    auto previous = points.front().date;
    for (const auto& row : points) {
        if (!(row.base_discount > 0.0) || !(row.quote_discount > 0.0))
            return std::unexpected(prefix + "the row dated " + std::format("{}", row.date) +
                                   " has a discount that is not above zero");
        if (days(row.date) < days(previous))
            return std::unexpected(prefix + "the rows are not in date order at " +
                                   std::format("{}", row.date));
        if (days(row.date) < days(spot_date))
            return std::unexpected(prefix + "the row dated " + std::format("{}", row.date) +
                                   " is before the spot date " + std::format("{}", spot_date));
        previous = row.date;
    }

    if (days(value_date) == days(spot_date))
        return fx_market{.spot = spot, .domestic_discount = 1.0, .foreign_discount = 1.0};
    if (days(value_date) > days(points.back().date))
        return std::unexpected(prefix + "the forward set ends at " +
                               std::format("{}", points.back().date) + ", before " +
                               std::format("{}", value_date));

    auto before = fx_discount_point{.date = spot_date, .base_discount = 1.0, .quote_discount = 1.0};
    for (const auto& row : points) {
        if (days(row.date) == days(value_date))
            return fx_market{.spot = spot,
                             .domestic_discount = row.quote_discount,
                             .foreign_discount = row.base_discount};
        if (days(row.date) > days(value_date)) {
            const int span = days_between(before.date, row.date);
            const double share = span > 0 ?
                                     static_cast<double>(days_between(before.date, value_date)) /
                                         static_cast<double>(span) :
                                     1.0;
            return fx_market{.spot = spot,
                             .domestic_discount =
                                 lerp_discount(before.quote_discount, row.quote_discount, share),
                             .foreign_discount =
                                 lerp_discount(before.base_discount, row.base_discount, share)};
        }
        before = row;
    }
    return std::unexpected(prefix + "the forward set has no row at or after " +
                           std::format("{}", value_date));
}

namespace {

/// One side's discount factor at @p value_date: its own point, or a log-linear walk from
/// the spot date to it, which is the rule both overloads share.
std::expected<double, std::string> side_discount(std::span<const fx_curve_point> points,
                                                 std::chrono::year_month_day spot_date,
                                                 std::chrono::year_month_day value_date,
                                                 std::string_view side,
                                                 const std::string& prefix) {
    if (points.empty())
        return std::unexpected(prefix + "the " + std::string(side) + " curve is empty");

    auto previous = points.front().date;
    for (const auto& point : points) {
        if (!(point.discount_factor > 0.0))
            return std::unexpected(prefix + "the " + std::string(side) + " curve's factor on " +
                                   std::format("{}", point.date) + " is not above zero");
        if (days(point.date) < days(previous))
            return std::unexpected(prefix + "the " + std::string(side) +
                                   " curve is not in date order at " +
                                   std::format("{}", point.date));
        previous = point.date;
    }
    if (days(value_date) < days(spot_date))
        return std::unexpected(prefix + "the value date " + std::format("{}", value_date) +
                               " is before the spot date " + std::format("{}", spot_date));
    if (days(value_date) == days(spot_date))
        return 1.0;
    if (days(value_date) > days(points.back().date))
        return std::unexpected(prefix + "the " + std::string(side) + " curve ends at " +
                               std::format("{}", points.back().date) + ", before " +
                               std::format("{}", value_date));

    fx_curve_point before{.date = spot_date, .discount_factor = 1.0};
    for (const auto& point : points) {
        if (days(point.date) == days(value_date))
            return point.discount_factor;
        if (days(point.date) > days(value_date)) {
            const int span = days_between(before.date, point.date);
            const double share = span > 0 ?
                                     static_cast<double>(days_between(before.date, value_date)) /
                                         static_cast<double>(span) :
                                     1.0;
            return lerp_discount(before.discount_factor, point.discount_factor, share);
        }
        before = point;
    }
    return std::unexpected(prefix + "the " + std::string(side) +
                           " curve has no point at or after " + std::format("{}", value_date));
}

}

std::expected<fx_market, std::string> pair_market_at(double spot,
                                                     std::chrono::year_month_day spot_date,
                                                     std::span<const fx_curve_point> base,
                                                     std::span<const fx_curve_point> quote,
                                                     std::chrono::year_month_day value_date,
                                                     std::string context) {
    const auto prefix = context.empty() ? std::string{} : context + ": ";
    if (!(spot > 0.0))
        return std::unexpected(prefix + "the spot is not above zero");

    const auto foreign = side_discount(base, spot_date, value_date, "base", prefix);
    if (!foreign)
        return std::unexpected(foreign.error());
    const auto domestic = side_discount(quote, spot_date, value_date, "quote", prefix);
    if (!domestic)
        return std::unexpected(domestic.error());
    return fx_market{.spot = spot, .domestic_discount = *domestic, .foreign_discount = *foreign};
}

}
