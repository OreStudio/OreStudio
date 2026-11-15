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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.refdata.core/mapping/quant_codes.hpp"
#include <catch2/catch_test_macros.hpp>
#include <magic_enum/magic_enum.hpp>
#include <stdexcept>

namespace {

using namespace ores::refdata::mapping;
using namespace ores::analytics::quant::domain;
using namespace ores::analytics::quant::service;
using enum bootstrap_curve_role_code;
using enum interpolation_method_code;

/// Every value of the enum reads back from the code it is written as, so a value added to the
/// quant library with no stored spelling fails here rather than at a caller.
template <class Value, class Spell, class Read>
void check_round_trip(Spell spell, Read read) {
    for (const auto value : magic_enum::enum_values<Value>())
        CHECK(read(spell(value)) == value);
}

}

TEST_CASE("every quant value has a stored code that reads back to it", "[quant_codes]") {
    check_round_trip<business_day_convention>(to_business_day_convention_code,
                                              parse_business_day_convention_code);
    check_round_trip<period_unit>(to_period_unit_code, parse_period_unit_code);
    check_round_trip<tenor_kind>(to_tenor_kind_code, parse_tenor_kind_code);
    check_round_trip<tenor_anchor>(to_tenor_anchor_code, parse_tenor_anchor_code);
    check_round_trip<resolution_algorithm>(to_resolution_algorithm_code,
                                           parse_resolution_algorithm_code);
    check_round_trip<observance_shift>(to_observance_shift_code, parse_observance_shift_code);
    check_round_trip<bootstrap_curve_role_code>(to_bootstrap_curve_role_code,
                                                parse_bootstrap_curve_role_code);
    check_round_trip<interpolation_method_code>(to_interpolation_method_code,
                                                parse_interpolation_method_code);
    check_round_trip<day_count_convention_code>(to_day_count_convention_code,
                                                parse_day_count_convention_code);
}

TEST_CASE("every business day convention code the database holds parses", "[quant_codes]") {
    // The seven codes ores_refdata_business_day_convention_types_tbl carries,
    // spelled exactly as the table spells them.
    CHECK(parse_business_day_convention_code("Unadjusted") == business_day_convention::unadjusted);
    CHECK(parse_business_day_convention_code("Following") == business_day_convention::following);
    CHECK(parse_business_day_convention_code("ModifiedFollowing") ==
          business_day_convention::modified_following);
    CHECK(parse_business_day_convention_code("Preceding") == business_day_convention::preceding);
    CHECK(parse_business_day_convention_code("ModifiedPreceding") ==
          business_day_convention::modified_preceding);
    CHECK(parse_business_day_convention_code("NEAREST") == business_day_convention::nearest);
    CHECK(parse_business_day_convention_code("HalfMonthModifiedFollowing") ==
          business_day_convention::half_month_modified_following);
}

TEST_CASE("a convention survives a round trip through its persisted code", "[quant_codes]") {
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

TEST_CASE("an unknown business day convention code is refused rather than mapped",
          "[quant_codes]") {
    CHECK_THROWS_AS(parse_business_day_convention_code("Modified"), std::invalid_argument);
    CHECK_THROWS_AS(parse_business_day_convention_code(""), std::invalid_argument);
    CHECK_THROWS_AS(parse_business_day_convention_code("unadjusted"), std::invalid_argument);
}

TEST_CASE("every period unit code the database holds parses", "[quant_codes]") {
    CHECK(parse_period_unit_code("DAY") == period_unit::day);
    CHECK(parse_period_unit_code("WEEK") == period_unit::week);
    CHECK(parse_period_unit_code("MONTH") == period_unit::month);
    CHECK(parse_period_unit_code("YEAR") == period_unit::year);
    CHECK(parse_period_unit_code("NONE") == period_unit::none);
}

TEST_CASE("a period unit survives a round trip through its persisted code", "[quant_codes]") {
    const period_unit units[] = {period_unit::day,
                                 period_unit::week,
                                 period_unit::month,
                                 period_unit::year,
                                 period_unit::none};

    for (const auto unit : units)
        CHECK(parse_period_unit_code(to_period_unit_code(unit)) == unit);
}

TEST_CASE("an unknown period unit code is refused rather than mapped", "[quant_codes]") {
    CHECK_THROWS_AS(parse_period_unit_code("QUARTER"), std::invalid_argument);
    CHECK_THROWS_AS(parse_period_unit_code("day"), std::invalid_argument);
}

TEST_CASE("the tenor kind codes the database holds parse", "[quant_codes]") {
    CHECK(parse_tenor_kind_code("PERIOD") == tenor_kind::period);
    CHECK(parse_tenor_kind_code("SPECIAL") == tenor_kind::special);
    CHECK(parse_tenor_kind_code(to_tenor_kind_code(tenor_kind::special)) == tenor_kind::special);
    CHECK_THROWS_AS(parse_tenor_kind_code("PERIODIC"), std::invalid_argument);
}

TEST_CASE("every tenor anchor code the database holds parses", "[quant_codes]") {
    CHECK(parse_tenor_anchor_code("NONE") == tenor_anchor::none);
    CHECK(parse_tenor_anchor_code("SPOT") == tenor_anchor::spot);
    CHECK(parse_tenor_anchor_code("TODAY") == tenor_anchor::today);
    CHECK(parse_tenor_anchor_code("TOMORROW") == tenor_anchor::tomorrow);
    CHECK(parse_tenor_anchor_code("NEAR_LEG") == tenor_anchor::near_leg);
    CHECK(parse_tenor_anchor_code("IMM_ROLL") == tenor_anchor::imm_roll);

    CHECK(to_tenor_anchor_code(tenor_anchor::imm_roll) == "IMM_ROLL");
    CHECK_THROWS_AS(parse_tenor_anchor_code("SPOT_DATE"), std::invalid_argument);
}

TEST_CASE("IMM_ROLL is an anchor and not a resolution algorithm", "[quant_codes]") {
    // The two code lists overlap in name and not in meaning: the anchor says
    // where a tenor counts from, the algorithm says how it gets there.
    CHECK(parse_tenor_anchor_code("IMM_ROLL") == tenor_anchor::imm_roll);
    CHECK_THROWS_AS(parse_resolution_algorithm_code("IMM_ROLL"), std::invalid_argument);
}

TEST_CASE("the resolution algorithm codes the database holds parse", "[quant_codes]") {
    CHECK(parse_resolution_algorithm_code("ANCHOR_OFFSET") == resolution_algorithm::anchor_offset);
    CHECK(parse_resolution_algorithm_code("SCHEDULE_STEP") == resolution_algorithm::schedule_step);
    CHECK(to_resolution_algorithm_code(resolution_algorithm::schedule_step) == "SCHEDULE_STEP");
    CHECK_THROWS_AS(parse_resolution_algorithm_code("ROLL_QUARTER"), std::invalid_argument);
}

TEST_CASE("every observance shift code the database holds parses, and no other", "[quant_codes]") {
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

TEST_CASE("parse_day_count_convention_code accepts every implemented convention", "[quant_codes]") {
    CHECK(parse_day_count_convention_code("A360") == day_count_convention_code::A360);
    CHECK(parse_day_count_convention_code("A365") == day_count_convention_code::A365);
    CHECK(parse_day_count_convention_code("A365F") == day_count_convention_code::A365F);
    CHECK(parse_day_count_convention_code("30/360") == day_count_convention_code::THIRTY_360);
}

TEST_CASE("parse_day_count_convention_code rejects an unsupported convention", "[quant_codes]") {
    CHECK_THROWS_AS(parse_day_count_convention_code("ActAct(ISDA)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("ActAct(ISMA)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("ActAct(AFB)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("30E/360"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("30E/360(ISDA)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("Business252"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("bogus"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code(""), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("a360"), std::invalid_argument);
}

TEST_CASE("parse_bootstrap_curve_role_code accepts every valid curve_role.code", "[quant_codes]") {
    CHECK(parse_bootstrap_curve_role_code("DEPOSIT") == DEPOSIT);
    CHECK(parse_bootstrap_curve_role_code("FRA") == FRA);
    CHECK(parse_bootstrap_curve_role_code("SWAP") == SWAP);
}

TEST_CASE("parse_bootstrap_curve_role_code rejects an unsupported curve_role.code",
          "[quant_codes]") {
    CHECK_THROWS_AS(parse_bootstrap_curve_role_code("FUTURE"), std::invalid_argument);
    CHECK_THROWS_AS(parse_bootstrap_curve_role_code("NONE"), std::invalid_argument);
    CHECK_THROWS_AS(parse_bootstrap_curve_role_code(""), std::invalid_argument);
    CHECK_THROWS_AS(parse_bootstrap_curve_role_code("deposit"), std::invalid_argument);
}

TEST_CASE("parse_interpolation_method_code accepts every valid interpolation_method.code, "
          "including the recognised-but-unimplemented CUBIC_SPLINE",
          "[quant_codes]") {
    CHECK(parse_interpolation_method_code("LOG_LINEAR_DISCOUNT") == LOG_LINEAR_DISCOUNT);
    CHECK(parse_interpolation_method_code("FLAT_FORWARD_THEN_LOG_LINEAR") ==
          FLAT_FORWARD_THEN_LOG_LINEAR);
    CHECK(parse_interpolation_method_code("CUBIC_SPLINE") ==
          interpolation_method_code::CUBIC_SPLINE);
}

TEST_CASE("parse_interpolation_method_code rejects an unsupported interpolation_method.code",
          "[quant_codes]") {
    CHECK_THROWS_AS(parse_interpolation_method_code("MONOTONE_CONVEX"), std::invalid_argument);
    CHECK_THROWS_AS(parse_interpolation_method_code(""), std::invalid_argument);
}

TEST_CASE("parse_day_count_convention_code accepts every valid day_count_fraction_type.code",
          "[quant_codes]") {
    CHECK(parse_day_count_convention_code("A360") == day_count_convention_code::A360);
    CHECK(parse_day_count_convention_code("A365") == day_count_convention_code::A365);
    CHECK(parse_day_count_convention_code("A365F") == day_count_convention_code::A365F);
    CHECK(parse_day_count_convention_code("30/360") == day_count_convention_code::THIRTY_360);
}

TEST_CASE("parse_day_count_convention_code rejects an unsupported day_count_fraction_type.code",
          "[quant_codes]") {
    CHECK_THROWS_AS(parse_day_count_convention_code("ActAct(ISDA)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("Business252"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("30E/360"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code(""), std::invalid_argument);
}
