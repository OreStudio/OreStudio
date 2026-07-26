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
#include "ores.analytics.quant/service/quantlib_calendar_rulesets.hpp"

namespace ores::analytics::quant::service {

using domain::calendar_exception;
using domain::calendar_rule;
using domain::calendar_rule_kind;
using domain::calendar_ruleset;
using domain::observance_shift;

using namespace std::chrono;

domain::calendar_ruleset quantlib_calendar_rulesets::target() {
    calendar_ruleset cal;
    cal.rules = {
        // New Year's Day
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = January, .day = 1u},
        // Good Friday (Easter Sunday - 2 days), from 2000
        calendar_rule{.kind = calendar_rule_kind::easter_offset, .day_offset = -2,
                      .effective_from = year{2000}},
        // Easter Monday (Easter Sunday + 1 day), from 2000
        calendar_rule{.kind = calendar_rule_kind::easter_offset, .day_offset = 1,
                      .effective_from = year{2000}},
        // Labour Day, from 2000
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = May, .day = 1u,
                      .effective_from = year{2000}},
        // Christmas
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = December, .day = 25u},
        // Day of Goodwill, from 2000
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = December, .day = 26u,
                      .effective_from = year{2000}},
    };
    cal.exceptions = {
        // December 31st, 1998, 1999, and 2001 only
        calendar_exception{.date = year{1998} / December / 31, .is_business_day = false},
        calendar_exception{.date = year{1999} / December / 31, .is_business_day = false},
        calendar_exception{.date = year{2001} / December / 31, .is_business_day = false},
    };
    return cal;
}

domain::calendar_ruleset quantlib_calendar_rulesets::weekends_only() {
    return calendar_ruleset{};
}

domain::calendar_ruleset quantlib_calendar_rulesets::united_states_settlement() {
    calendar_ruleset cal;
    cal.rules = {
        // New Year's Day (Monday if Sunday, preceding Friday if Saturday --
        // the latter naturally lands in the prior December via date
        // arithmetic, matching QuantLib's separate "Dec 31st if Friday" leg).
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = January, .day = 1u,
                      .shift = observance_shift::nearest_weekday},
        // Martin Luther King's birthday (third Monday in January), from 1983
        calendar_rule{.kind = calendar_rule_kind::nth_weekday_of_month, .month = January,
                      .weekday = Monday, .occurrence = 3u, .effective_from = year{1983}},
        // Washington's birthday: February 22nd (adjusted) pre-1971...
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = February, .day = 22u,
                      .shift = observance_shift::nearest_weekday, .effective_to = year{1970}},
        // ...third Monday in February from 1971
        calendar_rule{.kind = calendar_rule_kind::nth_weekday_of_month, .month = February,
                      .weekday = Monday, .occurrence = 3u, .effective_from = year{1971}},
        // Memorial Day: May 30th (adjusted) pre-1971...
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = May, .day = 30u,
                      .shift = observance_shift::nearest_weekday, .effective_to = year{1970}},
        // ...last Monday in May from 1971
        calendar_rule{.kind = calendar_rule_kind::last_weekday_of_month, .month = May,
                      .weekday = Monday, .effective_from = year{1971}},
        // Juneteenth, declared 2021 but only observed by exchanges since 2022
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = June, .day = 19u,
                      .shift = observance_shift::nearest_weekday, .effective_from = year{2022}},
        // Independence Day
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = July, .day = 4u,
                      .shift = observance_shift::nearest_weekday},
        // Labor Day (first Monday in September)
        calendar_rule{.kind = calendar_rule_kind::nth_weekday_of_month, .month = September,
                      .weekday = Monday, .occurrence = 1u},
        // Columbus Day (second Monday in October), from 1971
        calendar_rule{.kind = calendar_rule_kind::nth_weekday_of_month, .month = October,
                      .weekday = Monday, .occurrence = 2u, .effective_from = year{1971}},
        // Veterans Day: November 11th (adjusted) up to and including 1970...
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = November, .day = 11u,
                      .shift = observance_shift::nearest_weekday, .effective_to = year{1970}},
        // ...fourth Monday in October from 1971 to 1977...
        calendar_rule{.kind = calendar_rule_kind::nth_weekday_of_month, .month = October,
                      .weekday = Monday, .occurrence = 4u, .effective_from = year{1971},
                      .effective_to = year{1977}},
        // ...back to November 11th (adjusted) from 1978
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = November, .day = 11u,
                      .shift = observance_shift::nearest_weekday, .effective_from = year{1978}},
        // Thanksgiving Day (fourth Thursday in November)
        calendar_rule{.kind = calendar_rule_kind::nth_weekday_of_month, .month = November,
                      .weekday = Thursday, .occurrence = 4u},
        // Christmas
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = December, .day = 25u,
                      .shift = observance_shift::nearest_weekday},
    };
    return cal;
}

} // namespace ores::analytics::quant::service
