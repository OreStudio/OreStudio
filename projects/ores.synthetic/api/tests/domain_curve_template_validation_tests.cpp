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
#include "ores.synthetic.api/domain/curve_template_validation.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention;
using ores::refdata::domain::tenor_convention_resolution;
using ores::synthetic::domain::curve_template_entry_ref;
using ores::synthetic::domain::validate_curve_template;

namespace {

tenor make_period_tenor(const std::string& code, const std::string& unit, int multiplier) {
    tenor t;
    t.code = code;
    t.display_name = code;
    t.kind = "PERIOD";
    t.unit = unit;
    t.multiplier = multiplier;
    return t;
}

tenor_convention make_rates_spot_forward_convention() {
    tenor_convention c;
    c.code = "RATES_SPOT_FORWARD";
    c.measured_from = "SPOT";
    c.resolution_algorithm = "ANCHOR_OFFSET";
    return c;
}

tenor_convention_resolution make_resolution(const std::string& tenor_code) {
    tenor_convention_resolution r;
    r.convention_code = "RATES_SPOT_FORWARD";
    r.tenor_code = tenor_code;
    return r;
}

// A small, self-contained catalog: SPOT (zero-duration), 3M, 6M, 1Y.
std::vector<tenor> test_tenors() {
    return {make_period_tenor("SPOT", "DAY", 0),
           make_period_tenor("3M", "MONTH", 3),
           make_period_tenor("6M", "MONTH", 6),
           make_period_tenor("1Y", "MONTH", 12)};
}

std::vector<tenor_convention_resolution> test_resolutions() {
    return {make_resolution("SPOT"), make_resolution("3M"), make_resolution("6M"),
           make_resolution("1Y")};
}

constexpr std::chrono::year_month_day horizon{std::chrono::year{2026}, std::chrono::month{1},
                                              std::chrono::day{1}};
constexpr std::chrono::year_month_day spot{std::chrono::year{2026}, std::chrono::month{1},
                                           std::chrono::day{3}};

}

TEST_CASE("validate_curve_template accepts an empty template", "[curve_template_validation]") {
    const auto result = validate_curve_template(
        {}, test_tenors(), make_rates_spot_forward_convention(), test_resolutions(), horizon, spot);
    CHECK(result.valid);
    CHECK(result.message.empty());
}

TEST_CASE("validate_curve_template accepts a single deposit-equivalent entry",
          "[curve_template_validation]") {
    const std::vector<curve_template_entry_ref> entries{
        {.sequence_index = 0, .start_tenor_code = "SPOT", .end_tenor_code = "3M",
         .instrument_code = "DEPO"}};
    const auto result = validate_curve_template(entries, test_tenors(),
                                                make_rates_spot_forward_convention(),
                                                test_resolutions(), horizon, spot);
    CHECK(result.valid);
}

TEST_CASE("validate_curve_template accepts multiple point instruments at different maturities "
          "(no collision between deposits/swaps sharing SPOT as their start)",
          "[curve_template_validation]") {
    const std::vector<curve_template_entry_ref> entries{
        {.sequence_index = 0, .start_tenor_code = "SPOT", .end_tenor_code = "3M",
         .instrument_code = "DEPO"},
        {.sequence_index = 1, .start_tenor_code = "SPOT", .end_tenor_code = "6M",
         .instrument_code = "IRS"},
        {.sequence_index = 2, .start_tenor_code = "SPOT", .end_tenor_code = "1Y",
         .instrument_code = "IRS"}};
    const auto result = validate_curve_template(entries, test_tenors(),
                                                make_rates_spot_forward_convention(),
                                                test_resolutions(), horizon, spot);
    CHECK(result.valid);
}

TEST_CASE("validate_curve_template accepts back-to-back FRA periods (touching endpoints, not a "
          "collision)",
          "[curve_template_validation]") {
    const std::vector<curve_template_entry_ref> entries{
        {.sequence_index = 0, .start_tenor_code = "3M", .end_tenor_code = "6M",
         .instrument_code = "FRA"},
        {.sequence_index = 1, .start_tenor_code = "6M", .end_tenor_code = "1Y",
         .instrument_code = "FRA"}};
    const auto result = validate_curve_template(entries, test_tenors(),
                                                make_rates_spot_forward_convention(),
                                                test_resolutions(), horizon, spot);
    CHECK(result.valid);
}

TEST_CASE("validate_curve_template rejects two overlapping FRA periods",
          "[curve_template_validation]") {
    const std::vector<curve_template_entry_ref> entries{
        {.sequence_index = 0, .start_tenor_code = "SPOT", .end_tenor_code = "6M",
         .instrument_code = "FRA"},
        {.sequence_index = 1, .start_tenor_code = "3M", .end_tenor_code = "1Y",
         .instrument_code = "FRA"}};
    const auto result = validate_curve_template(entries, test_tenors(),
                                                make_rates_spot_forward_convention(),
                                                test_resolutions(), horizon, spot);
    CHECK_FALSE(result.valid);
    CHECK(result.message.find("collides") != std::string::npos);
}

TEST_CASE("validate_curve_template rejects a swap tenor whose maturity falls strictly inside a "
          "FRA's active period",
          "[curve_template_validation]") {
    const std::vector<curve_template_entry_ref> entries{
        {.sequence_index = 0, .start_tenor_code = "3M", .end_tenor_code = "1Y",
         .instrument_code = "FRA"},
        {.sequence_index = 1, .start_tenor_code = "SPOT", .end_tenor_code = "6M",
         .instrument_code = "IRS"}};
    const auto result = validate_curve_template(entries, test_tenors(),
                                                make_rates_spot_forward_convention(),
                                                test_resolutions(), horizon, spot);
    CHECK_FALSE(result.valid);
    CHECK(result.message.find("collides") != std::string::npos);
}

TEST_CASE("validate_curve_template accepts a point instrument maturing exactly at a FRA's "
          "boundary (touching, not inside)",
          "[curve_template_validation]") {
    const std::vector<curve_template_entry_ref> entries{
        {.sequence_index = 0, .start_tenor_code = "3M", .end_tenor_code = "6M",
         .instrument_code = "FRA"},
        {.sequence_index = 1, .start_tenor_code = "SPOT", .end_tenor_code = "6M",
         .instrument_code = "IRS"}};
    const auto result = validate_curve_template(entries, test_tenors(),
                                                make_rates_spot_forward_convention(),
                                                test_resolutions(), horizon, spot);
    CHECK(result.valid);
}

TEST_CASE("validate_curve_template rejects an entry referencing an unknown tenor code",
          "[curve_template_validation]") {
    const std::vector<curve_template_entry_ref> entries{
        {.sequence_index = 0, .start_tenor_code = "SPOT", .end_tenor_code = "99Y",
         .instrument_code = "IRS"}};
    const auto result = validate_curve_template(entries, test_tenors(),
                                                make_rates_spot_forward_convention(),
                                                test_resolutions(), horizon, spot);
    CHECK_FALSE(result.valid);
    CHECK(result.message.find("99Y") != std::string::npos);
}

TEST_CASE("validate_curve_template rejects an entry whose start does not resolve before its end",
          "[curve_template_validation]") {
    const std::vector<curve_template_entry_ref> entries{
        {.sequence_index = 0, .start_tenor_code = "6M", .end_tenor_code = "3M",
         .instrument_code = "FRA"}};
    const auto result = validate_curve_template(entries, test_tenors(),
                                                make_rates_spot_forward_convention(),
                                                test_resolutions(), horizon, spot);
    CHECK_FALSE(result.valid);
}
