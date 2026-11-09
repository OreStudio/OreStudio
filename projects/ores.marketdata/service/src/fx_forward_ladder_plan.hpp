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
#ifndef ORES_MARKETDATA_SERVICE_FX_FORWARD_LADDER_PLAN_HPP
#define ORES_MARKETDATA_SERVICE_FX_FORWARD_LADDER_PLAN_HPP

#include "ores.analytics.quant/domain/business_day_calendar_set.hpp"
#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.analytics.quant/service/fx_forward_calculator.hpp"
#include "ores.marketdata.api/domain/curve_publish.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.refdata.api/domain/calendar_date.hpp"
#include <chrono>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <vector>

namespace ores::marketdata::service {

struct fx_pair_codes final {
    std::string base;
    std::string quote;
};

/// Splits a pair code such as EUR/USD, and refuses a malformed code or a cross with no USD leg.
ORES_MARKETDATA_SERVICE_EXPORT std::expected<fx_pair_codes, std::string>
parse_usd_pair(const std::string& pair_code);

/**
 * @brief The set a reader at @p as_of should use: the applied set with the latest as-of at or
 * before @p as_of, and the latest applied among equals.
 *
 * A withdrawn set is never used. A superseded one still is, because it was the set in force when
 * the market stood at its own as-of.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::optional<domain::curve_publish>
select_published_set(std::span<const domain::curve_publish> sets,
                     std::chrono::system_clock::time_point as_of);

/// Adds the curve's reference date at a discount factor of one, which a stored curve leaves out.
ORES_MARKETDATA_SERVICE_EXPORT std::vector<analytics::quant::service::bootstrapped_point>
anchor_curve(std::vector<analytics::quant::service::bootstrapped_point> points,
             std::chrono::year_month_day reference_date);

/// One calendar index that is closed whenever any of @p calendars is closed.
ORES_MARKETDATA_SERVICE_EXPORT analytics::quant::domain::business_day_calendar_set
joint_calendar(std::span<const std::vector<ores::refdata::domain::calendar_date>> calendars);

struct fx_ladder_dates final {
    std::chrono::year_month_day spot_date{};
    std::vector<analytics::quant::service::fx_forward_tenor> tenors;
};

/**
 * @brief The spot date and the standard ladder of a pair.
 *
 * ON and TN are swaps of their own two dates, and TN exists only for a pair settling after T+1.
 * Every later tenor runs from the spot date.
 *
 * @param spot_days The pair's spot days, at least one.
 */
ORES_MARKETDATA_SERVICE_EXPORT fx_ladder_dates
build_ladder_dates(std::chrono::year_month_day horizon,
                   int spot_days,
                   analytics::quant::domain::business_day_convention roll,
                   bool end_of_month,
                   const analytics::quant::domain::business_day_calendar_set& calendar);

}

#endif
