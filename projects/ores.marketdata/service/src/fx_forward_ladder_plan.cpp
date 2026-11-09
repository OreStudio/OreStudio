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
#include "fx_forward_ladder_plan.hpp"
#include "ores.analytics.quant/service/calendar_arithmetic.hpp"
#include <algorithm>
#include <bitset>
#include <utility>

namespace ores::marketdata::service {

namespace {

namespace quant = ores::analytics::quant;
using quant::domain::date_period;
using quant::domain::period_unit;
using quant::service::calendar_arithmetic;
using quant::service::fx_forward_tenor;

constexpr unsigned weekend_mask = 0b1000001;
constexpr std::size_t only_calendar = 0;

struct ladder_tenor final {
    const char* id;
    date_period period;
};

const ladder_tenor standard_tenors[] = {
    {"1W", {period_unit::week, 1}},
    {"2W", {period_unit::week, 2}},
    {"1M", {period_unit::month, 1}},
    {"2M", {period_unit::month, 2}},
    {"3M", {period_unit::month, 3}},
    {"6M", {period_unit::month, 6}},
    {"9M", {period_unit::month, 9}},
    {"1Y", {period_unit::year, 1}},
    {"18M", {period_unit::month, 18}},
    {"2Y", {period_unit::year, 2}},
    {"5Y", {period_unit::year, 5}},
};

}

std::expected<fx_pair_codes, std::string> parse_usd_pair(const std::string& pair_code) {
    const auto slash = pair_code.find('/');
    if (slash == std::string::npos || slash == 0 || slash + 1 == pair_code.size() ||
        pair_code.find('/', slash + 1) != std::string::npos)
        return std::unexpected("'" + pair_code + "' is not a currency pair such as EUR/USD.");
    fx_pair_codes codes{pair_code.substr(0, slash), pair_code.substr(slash + 1)};
    if (codes.base != "USD" && codes.quote != "USD")
        return std::unexpected(pair_code +
                               " has no USD leg, and only USD pairs are answered for now.");
    if (codes.base == codes.quote)
        return std::unexpected(pair_code + " names one currency twice.");
    return codes;
}

std::optional<domain::curve_publish>
select_published_set(std::span<const domain::curve_publish> sets,
                     std::chrono::system_clock::time_point as_of) {
    const domain::curve_publish* best = nullptr;
    for (const auto& set : sets) {
        if (!set.published_at || set.withdrawn_at || set.as_of > as_of)
            continue;
        if (!best || set.as_of > best->as_of ||
            (set.as_of == best->as_of && *set.published_at > *best->published_at))
            best = &set;
    }
    return best ? std::optional(*best) : std::nullopt;
}

std::vector<quant::service::bootstrapped_point>
anchor_curve(std::vector<quant::service::bootstrapped_point> points,
             std::chrono::year_month_day reference_date) {
    if (points.empty() ||
        std::chrono::sys_days(points.front().date) > std::chrono::sys_days(reference_date))
        points.insert(points.begin(), {"", reference_date, 1.0});
    return points;
}

quant::domain::business_day_calendar_set
joint_calendar(std::span<const std::vector<ores::refdata::domain::calendar_date>> calendars) {
    std::vector<quant::domain::calendar_date_row> holidays;
    for (const auto& days : calendars)
        for (const auto& day : days)
            if (!day.is_business_day)
                holidays.push_back({only_calendar, day.date});
    const auto earlier = [](const auto& a, const auto& b) {
        return std::chrono::sys_days(a.date) < std::chrono::sys_days(b.date);
    };
    std::ranges::sort(holidays, earlier);
    const auto same = [](const auto& a, const auto& b) {
        return a.date == b.date;
    };
    holidays.erase(std::unique(holidays.begin(), holidays.end(), same), holidays.end());
    return quant::domain::business_day_calendar_set::from_rows(
        holidays, 1, {std::bitset<7>{weekend_mask}});
}

fx_ladder_dates build_ladder_dates(std::chrono::year_month_day as_of_date,
                                   int spot_days,
                                   quant::domain::business_day_convention roll,
                                   bool end_of_month,
                                   const quant::domain::business_day_calendar_set& calendar) {
    const auto horizon = calendar_arithmetic::adjust(
        as_of_date, quant::domain::business_day_convention::following, only_calendar, calendar);
    const auto advance = [&](std::chrono::year_month_day from, date_period period) {
        return calendar_arithmetic::advance(
            from, period, roll, end_of_month, only_calendar, calendar);
    };
    const auto business_days = [&](int n) {
        return advance(horizon, {period_unit::day, n});
    };

    fx_ladder_dates out;
    out.spot_date = business_days(spot_days);
    const auto tomorrow = business_days(1);
    out.tenors.push_back({.tenor_id = "ON", .value_date = tomorrow, .near_date = horizon});
    if (spot_days > 1)
        out.tenors.push_back(
            {.tenor_id = "TN", .value_date = business_days(2), .near_date = tomorrow});
    out.tenors.push_back({.tenor_id = "SP", .value_date = out.spot_date});
    out.tenors.push_back(
        {.tenor_id = "SN", .value_date = advance(out.spot_date, {period_unit::day, 1})});
    for (const auto& t : standard_tenors)
        out.tenors.push_back({.tenor_id = t.id, .value_date = advance(out.spot_date, t.period)});
    return out;
}

}
