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
#include "ores.analytics.quant/domain/calendar_ruleset.hpp"
#include "ores.analytics.quant/service/calendar_rule_engine.hpp"
#include "ores.analytics.quant/service/quantlib_calendar_rulesets.hpp"
#include <catch2/catch_test_macros.hpp>
#include <vector>

using namespace std::chrono;
using ores::analytics::quant::domain::calendar_ruleset;
using ores::analytics::quant::domain::instantiated_holiday;
using ores::analytics::quant::service::calendar_rule_engine;
using ores::analytics::quant::service::quantlib_calendar_rulesets;

namespace {

std::vector<year_month_day> dates_for(std::size_t calendar_index,
                                      const std::vector<instantiated_holiday>& all) {
    std::vector<year_month_day> result;
    for (const auto& h : all)
        if (h.calendar_index == calendar_index)
            result.push_back(h.date);
    return result;
}

} // namespace

TEST_CASE("WeekendsOnly has no holidays at all, over any range", "[quantlib_calendar_rulesets]") {
    const std::vector<calendar_ruleset> calendars = {quantlib_calendar_rulesets::weekends_only()};
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        calendars, year{2000} / January / 1, year{2030} / December / 31);
    CHECK(dates_for(0, holidays).empty());
}

TEST_CASE("US Settlement holiday list 2004-2005 matches QuantLib's testUSSettlement dataset",
         "[quantlib_calendar_rulesets][united_states]") {
    const std::vector<calendar_ruleset> calendars = {quantlib_calendar_rulesets::united_states_settlement()};
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        calendars, year{2004} / January / 1, year{2005} / December / 31);
    const auto actual = dates_for(0, holidays);

    const std::vector<year_month_day> expected = {
        year{2004} / January / 1,
        year{2004} / January / 19,
        year{2004} / February / 16,
        year{2004} / May / 31,
        year{2004} / July / 5,
        year{2004} / September / 6,
        year{2004} / October / 11,
        year{2004} / November / 11,
        year{2004} / November / 25,
        year{2004} / December / 24,

        year{2004} / December / 31,
        year{2005} / January / 17,
        year{2005} / February / 21,
        year{2005} / May / 30,
        year{2005} / July / 4,
        year{2005} / September / 5,
        year{2005} / October / 10,
        year{2005} / November / 11,
        year{2005} / November / 24,
        year{2005} / December / 26,
    };

    REQUIRE(actual.size() == expected.size());
    CHECK(actual == expected);
}

TEST_CASE("US Settlement holiday list 1961 (pre-Uniform Monday Holiday Act) matches QuantLib",
         "[quantlib_calendar_rulesets][united_states]") {
    const std::vector<calendar_ruleset> calendars = {quantlib_calendar_rulesets::united_states_settlement()};
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        calendars, year{1961} / January / 1, year{1961} / December / 31);
    const auto actual = dates_for(0, holidays);

    const std::vector<year_month_day> expected = {
        year{1961} / January / 2,
        year{1961} / February / 22,
        year{1961} / May / 30,
        year{1961} / July / 4,
        year{1961} / September / 4,
        year{1961} / November / 10,
        year{1961} / November / 23,
        year{1961} / December / 25,
    };

    REQUIRE(actual.size() == expected.size());
    CHECK(actual == expected);
}
