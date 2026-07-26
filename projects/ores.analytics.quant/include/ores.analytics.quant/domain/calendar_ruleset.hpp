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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_CALENDAR_RULESET_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_CALENDAR_RULESET_HPP

#include "ores.analytics.quant/domain/calendar_rule.hpp"
#include <bitset>
#include <chrono>
#include <cstddef>
#include <vector>

namespace ores::analytics::quant::domain {

/// One base-less calendar's full rule set -- the batch engine's input unit.
/// Weekends are not expressed as rules (every calendar has exactly one
/// weekend policy, not a variable number of them), so they are carried as a
/// dedicated mask rather than shoehorned into @c calendar_rule.
struct calendar_ruleset final {
    std::vector<calendar_rule> rules;
    std::vector<calendar_exception> exceptions;
    /// Bit i set (0 = Sunday, per std::chrono::weekday::c_encoding) means
    /// day i is a non-business weekend day. Defaults to Saturday/Sunday.
    std::bitset<7> weekend_mask{0b1000001};
};

/// One (calendar, date) holiday produced by @c instantiate_holidays_batch.
/// A flat output list -- the shape scanned once to bulk-insert into
/// calendar_dates, never a vector<vector<...>>.
struct instantiated_holiday final {
    std::size_t calendar_index;
    std::chrono::year_month_day date;
};

} // namespace ores::analytics::quant::domain

#endif
