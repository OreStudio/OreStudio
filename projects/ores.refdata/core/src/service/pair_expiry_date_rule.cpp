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
#include "ores.refdata.core/service/pair_expiry_date_rule.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata.core/mapping/quant_codes.hpp"
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_calendar_repository.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_repository.hpp"
#include <algorithm>
#include <bitset>
#include <chrono>
#include <optional>
#include <span>
#include <utility>
#include <vector>

namespace ores::refdata::service {

namespace {

namespace quant_domain = ores::analytics::quant::domain;
namespace refdata_repo = ores::refdata::repository;

/// Saturday and Sunday, in the bit positions a weekday takes: the weeks a pair
/// settles over are the ones reference data never materialises as holidays.
constexpr unsigned weekend_closed_mask = 0b1000001;
constexpr std::size_t only_calendar = 0;

}

quant_domain::business_day_calendar_set
joint_calendar(std::span<const std::vector<domain::calendar_date>> calendars) {
    std::vector<quant_domain::calendar_date_row> holidays;
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
    return quant_domain::business_day_calendar_set::from_rows(
        holidays, 1, {std::bitset<7>{weekend_closed_mask}});
}

std::expected<pair_expiry_date_rule, std::string>
read_pair_expiry_date_rule(const ores::database::context& ctx, const std::string& pair_code) {
    refdata_repo::currency_pair_convention_repository convention_repo;
    const auto conventions = convention_repo.read_latest(ctx, pair_code);
    if (conventions.empty())
        return std::unexpected("No convention is published for " + pair_code + ".");
    const auto& convention = conventions.front();

    std::vector<std::vector<domain::calendar_date>> calendars;
    std::string codes;
    refdata_repo::currency_pair_convention_calendar_repository links{ctx};
    refdata_repo::calendar_date_repository dates{ctx};
    for (const auto& link : links.read_latest_by_pair(pair_code)) {
        codes += (codes.empty() ? "" : ",") + link.calendar_code;
        calendars.push_back(dates.read_latest_by_calendar(link.calendar_code));
        if (calendars.back().empty())
            return std::unexpected("Calendar " + link.calendar_code + " of " + pair_code +
                                   " has no materialised dates.");
    }
    if (calendars.empty())
        return std::unexpected("The convention of " + pair_code + " names no calendar.");

    const auto roll = mapping::parse_business_day_convention_code(
        convention.business_day_convention.value_or("ModifiedFollowing"));
    auto rule = ores::analytics::quant::service::fx_expiry_date_rule(
        roll, convention.end_of_month.value_or(false), only_calendar, joint_calendar(calendars));
    return pair_expiry_date_rule{.rule = std::move(rule), .calendar_codes = std::move(codes)};
}

}
