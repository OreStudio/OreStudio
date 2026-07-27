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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_CALENDAR_RULE_ENGINE_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_CALENDAR_RULE_ENGINE_HPP

#include "ores.analytics.quant/domain/calendar_ruleset.hpp"
#include "ores.analytics.quant/export.hpp"
#include <chrono>
#include <span>
#include <vector>

namespace ores::analytics::quant::service {

/// Pure, batch-first evaluation of @c domain::calendar_rule /
/// @c domain::calendar_exception into concrete holiday dates. No calendar
/// object per calendar (unlike QuantLib's Calendar API), no global/static
/// state, no dependency on any refdata or persistence type -- every call
/// site supplies its own rulesets and gets back a flat, aligned answer.
class ORES_ANALYTICS_QUANT_EXPORT calendar_rule_engine {
public:
    /// Easter Sunday for @p y via the Meeus/Jones/Butcher Gregorian
    /// algorithm. Computed once per year by @c instantiate_holidays_batch
    /// and shared across every calendar with an @c easter_offset rule that
    /// year, rather than recomputed per calendar.
    [[nodiscard]] static std::chrono::year_month_day easter_sunday(std::chrono::year y);

    /// Walks every year in [start.year(), end.year()], evaluating each
    /// calendar's active rules and exceptions once, and returns the flat
    /// set of resulting holiday dates that fall within [start, end]
    /// (inclusive), one entry per (calendar_index, date), sorted by
    /// calendar_index then date, with no duplicates per calendar.
    /// Weekend days are never included -- they are not holidays, they are
    /// tracked separately (see domain::calendar_ruleset::weekend_mask and
    /// domain::business_day_calendar_set).
    [[nodiscard]] static std::vector<domain::instantiated_holiday>
    instantiate_holidays_batch(std::span<const domain::calendar_ruleset> calendars,
                               std::chrono::year_month_day start,
                               std::chrono::year_month_day end);
};

} // namespace ores::analytics::quant::service

#endif
