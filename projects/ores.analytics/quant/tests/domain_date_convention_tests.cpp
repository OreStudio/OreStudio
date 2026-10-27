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
#include "ores.analytics.quant/domain/calendar_rule.hpp"
#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/domain/tenor_resolution.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using ores::analytics::quant::domain::business_day_convention;
using ores::analytics::quant::domain::observance_shift;
using ores::analytics::quant::domain::parse_business_day_convention_code;
using ores::analytics::quant::domain::parse_observance_shift_code;
using ores::analytics::quant::domain::parse_period_unit_code;
using ores::analytics::quant::domain::parse_resolution_algorithm_code;
using ores::analytics::quant::domain::parse_tenor_anchor_code;
using ores::analytics::quant::domain::parse_tenor_kind_code;
using ores::analytics::quant::domain::period_unit;
using ores::analytics::quant::domain::resolution_algorithm;
using ores::analytics::quant::domain::resolution_status;
using ores::analytics::quant::domain::tenor_anchor;
using ores::analytics::quant::domain::tenor_kind;
using ores::analytics::quant::domain::to_business_day_convention_code;
using ores::analytics::quant::domain::to_observance_shift_code;
using ores::analytics::quant::domain::to_period_unit_code;
using ores::analytics::quant::domain::to_resolution_algorithm_code;
using ores::analytics::quant::domain::to_string;
using ores::analytics::quant::domain::to_tenor_anchor_code;
using ores::analytics::quant::domain::to_tenor_kind_code;

}

TEST_CASE("every business day convention code the database holds parses", "[date_convention]") {
    // The seven codes ores_refdata_business_day_convention_types_tbl carries,
    // spelled exactly as the table spells them.
    CHECK(parse_business_day_convention_code("Unadjusted") == business_day_convention::unadjusted);
    CHECK(parse_business_day_convention_code("Following") == business_day_convention::following);
    CHECK(parse_business_day_convention_code("ModifiedFollowing") ==
          business_day_convention::modified_following);
    CHECK(parse_business_day_convention_code("Preceding") == business_day_convention::preceding);
    CHECK(parse_business_day_convention_code("ModifiedPreceding") ==
          business_day_convention::modified_preceding);
    CHECK(parse_business_day_convention_code("Nearest") == business_day_convention::nearest);
    CHECK(parse_business_day_convention_code("HalfMonthModifiedFollowing") ==
          business_day_convention::half_month_modified_following);
}

TEST_CASE("a convention survives a round trip through its persisted code", "[date_convention]") {
    const business_day_convention conventions[] = {
        business_day_convention::unadjusted,
        business_day_convention::following,
        business_day_convention::modified_following,
        business_day_convention::preceding,
        business_day_convention::modified_preceding,
        business_day_convention::nearest,
        business_day_convention::half_month_modified_following,
    };

    for (const auto convention : conventions)
        CHECK(parse_business_day_convention_code(to_business_day_convention_code(convention)) ==
              convention);
}

TEST_CASE("an unknown business day convention code is refused rather than mapped", "[date_convention]") {
    CHECK_THROWS_AS(parse_business_day_convention_code("Modified"), std::invalid_argument);
    CHECK_THROWS_AS(parse_business_day_convention_code(""), std::invalid_argument);
    CHECK_THROWS_AS(parse_business_day_convention_code("unadjusted"), std::invalid_argument);
}

TEST_CASE("every period unit code the database holds parses", "[date_convention]") {
    CHECK(parse_period_unit_code("DAY") == period_unit::day);
    CHECK(parse_period_unit_code("WEEK") == period_unit::week);
    CHECK(parse_period_unit_code("MONTH") == period_unit::month);
    CHECK(parse_period_unit_code("YEAR") == period_unit::year);
    CHECK(parse_period_unit_code("NONE") == period_unit::none);
}

TEST_CASE("a period unit survives a round trip through its persisted code", "[date_convention]") {
    const period_unit units[] = {period_unit::day,
                                 period_unit::week,
                                 period_unit::month,
                                 period_unit::year,
                                 period_unit::none};

    for (const auto unit : units)
        CHECK(parse_period_unit_code(to_period_unit_code(unit)) == unit);
}

TEST_CASE("an unknown period unit code is refused rather than mapped", "[date_convention]") {
    CHECK_THROWS_AS(parse_period_unit_code("QUARTER"), std::invalid_argument);
    CHECK_THROWS_AS(parse_period_unit_code("day"), std::invalid_argument);
}

TEST_CASE("the tenor kind codes the database holds parse", "[date_convention]") {
    CHECK(parse_tenor_kind_code("PERIOD") == tenor_kind::period);
    CHECK(parse_tenor_kind_code("SPECIAL") == tenor_kind::special);
    CHECK(parse_tenor_kind_code(to_tenor_kind_code(tenor_kind::special)) == tenor_kind::special);
    CHECK_THROWS_AS(parse_tenor_kind_code("PERIODIC"), std::invalid_argument);
}

TEST_CASE("every tenor anchor code the database holds parses", "[date_convention]") {
    CHECK(parse_tenor_anchor_code("NONE") == tenor_anchor::none);
    CHECK(parse_tenor_anchor_code("SPOT") == tenor_anchor::spot);
    CHECK(parse_tenor_anchor_code("TODAY") == tenor_anchor::today);
    CHECK(parse_tenor_anchor_code("TOMORROW") == tenor_anchor::tomorrow);
    CHECK(parse_tenor_anchor_code("NEAR_LEG") == tenor_anchor::near_leg);
    CHECK(parse_tenor_anchor_code("IMM_ROLL") == tenor_anchor::imm_roll);

    CHECK(to_tenor_anchor_code(tenor_anchor::imm_roll) == "IMM_ROLL");
    CHECK_THROWS_AS(parse_tenor_anchor_code("SPOT_DATE"), std::invalid_argument);
}

TEST_CASE("IMM_ROLL is an anchor and not a resolution algorithm", "[date_convention]") {
    // The two code lists overlap in name and not in meaning: the anchor says
    // where a tenor counts from, the algorithm says how it gets there.
    CHECK(parse_tenor_anchor_code("IMM_ROLL") == tenor_anchor::imm_roll);
    CHECK_THROWS_AS(parse_resolution_algorithm_code("IMM_ROLL"), std::invalid_argument);
}

TEST_CASE("the resolution algorithm codes the database holds parse", "[date_convention]") {
    CHECK(parse_resolution_algorithm_code("ANCHOR_OFFSET") == resolution_algorithm::anchor_offset);
    CHECK(parse_resolution_algorithm_code("SCHEDULE_STEP") == resolution_algorithm::schedule_step);
    CHECK(to_resolution_algorithm_code(resolution_algorithm::schedule_step) == "SCHEDULE_STEP");
    CHECK_THROWS_AS(parse_resolution_algorithm_code("ROLL_QUARTER"), std::invalid_argument);
}

TEST_CASE("every resolution status spells itself for a refusal message", "[date_convention]") {
    const resolution_status statuses[] = {resolution_status::ok,
                                          resolution_status::tenor_not_in_convention,
                                          resolution_status::calendar_index_out_of_range,
                                          resolution_status::unsupported_anchor,
                                          resolution_status::missing_spot_date,
                                          resolution_status::period_tenor_without_period,
                                          resolution_status::special_tenor_without_offset,
                                          resolution_status::inconsistent_schedule_declaration,
                                          resolution_status::schedule_without_dates,
                                          resolution_status::schedule_dates_not_ordered,
                                          resolution_status::schedule_exhausted};

    for (const auto status : statuses) {
        CHECK_FALSE(to_string(status).empty());
        CHECK(to_string(status) != "unknown");
    }

    CHECK(to_string(resolution_status::unsupported_anchor) == "unsupported_anchor");
}

TEST_CASE("every observance shift code the database holds parses, and no other", "[date_convention]") {
    // The codes ores_refdata_calendar_rules_tbl.shift carries. The last two
    // differ only on a Saturday, and the persisted spelling is owned here so
    // the materialiser does not restate it.
    CHECK(parse_observance_shift_code("none") == observance_shift::none);
    CHECK(parse_observance_shift_code("nearest_weekday") == observance_shift::nearest_weekday);
    CHECK(parse_observance_shift_code("roll_forward_to_monday") ==
          observance_shift::roll_forward_to_monday);
    CHECK(parse_observance_shift_code("sunday_to_monday") == observance_shift::sunday_to_monday);

    CHECK(to_observance_shift_code(observance_shift::sunday_to_monday) == "sunday_to_monday");
    CHECK_THROWS_AS(parse_observance_shift_code("next_monday"), std::invalid_argument);
    CHECK_THROWS_AS(parse_observance_shift_code(""), std::invalid_argument);
}
