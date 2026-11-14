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
#ifndef ORES_REFDATA_CORE_SERVICE_PAIR_EXPIRY_DATE_RULE_HPP
#define ORES_REFDATA_CORE_SERVICE_PAIR_EXPIRY_DATE_RULE_HPP

#include "ores.analytics.quant/service/fx_expiry_dates.hpp"
#include "ores.database/domain/context_fwd.hpp"
#include "ores.refdata.api/domain/calendar_date.hpp"
#include "ores.refdata.core/export.hpp"
#include <expected>
#include <span>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief The expiry date rule a pair's convention states, with the calendars it names.
 *
 * The forward ladder and a volatility surface feed both date a tenor, and both must date it
 * the same way. Neither reads the convention itself: they call this, which is the one reader
 * of the pair's roll, month-end rule and calendars, so a pair cannot be dated one way by the
 * ladder that publishes it and another by the surface that quotes on it.
 */
struct pair_expiry_date_rule final {
    ores::analytics::quant::service::fx_expiry_date_rule rule;
    /// The calendar codes the convention names, comma separated in the order it names them.
    std::string calendar_codes;
};

/**
 * @brief The convention's calendars as one holiday set, because an expiry that falls on a
 * holiday in either of the pair's currencies moves.
 *
 * One calendar index that is closed whenever any of @p calendars is closed, with weekends
 * closed and the dated holidays as stored.
 */
[[nodiscard]] ORES_REFDATA_CORE_EXPORT ores::analytics::quant::domain::business_day_calendar_set
joint_calendar(std::span<const std::vector<ores::refdata::domain::calendar_date>> calendars);

/**
 * @brief Reads the pair's rule, or says why it cannot be built.
 *
 * Refuses a pair with no convention, one that names no calendar, and a calendar with no
 * materialised dates: each is a fact the rule cannot invent.
 *
 * @param ctx The database the convention and its calendars are read from.
 * @param pair_code The pair, as =EUR/USD=.
 */
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::expected<pair_expiry_date_rule, std::string>
read_pair_expiry_date_rule(const ores::database::context& ctx, const std::string& pair_code);

}

#endif
