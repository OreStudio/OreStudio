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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_QUANTLIB_CALENDAR_RULESETS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_QUANTLIB_CALENDAR_RULESETS_HPP

#include "ores.analytics.quant/domain/calendar_ruleset.hpp"
#include "ores.analytics.quant/export.hpp"

namespace ores::analytics::quant::service {

/// Hand-transcribed @c domain::calendar_ruleset builders for a starter set
/// of QuantLib's built-in calendars, mirroring the corresponding
/// isBusinessDay() logic in the vendored QuantLib source
/// (Engine.remote/QuantLib/ql/time/calendars/) rule-for-rule. These are the
/// seed data that will eventually be codegen'd into ores.refdata's
/// calendar_rule/calendar_exception tables (task plan step 3) -- kept here,
/// in the pure engine's own component, so they can be verified against
/// QuantLib's own test-suite golden holiday lists without any refdata or
/// database dependency.
///
/// Only the calendars whose isBusinessDay() decomposes cleanly into this
/// engine's four rule kinds plus a single per-holiday observance_shift are
/// covered so far. Notably absent: UnitedKingdom, whose Christmas/Boxing Day
/// pair rolls as a unit (a Saturday Christmas and a Sunday Christmas both
/// resolve to the same observed date, Dec 27th, because Boxing Day claims
/// whichever weekday Christmas didn't) -- a shape this engine's per-rule,
/// single-date shift model cannot express yet. Filed as a follow-up; see the
/// parent task's plan.
class ORES_ANALYTICS_QUANT_EXPORT quantlib_calendar_rulesets {
public:
    /// TARGET (the Eurosystem's calendar) -- ql/time/calendars/target.cpp.
    [[nodiscard]] static domain::calendar_ruleset target();

    /// WeekendsOnly -- ql/time/calendars/weekendsonly.cpp. No rules or
    /// exceptions at all: every non-weekend day is a business day.
    [[nodiscard]] static domain::calendar_ruleset weekends_only();

    /// UnitedStates::Settlement -- ql/time/calendars/unitedstates.cpp.
    [[nodiscard]] static domain::calendar_ruleset united_states_settlement();
};

} // namespace ores::analytics::quant::service

#endif
