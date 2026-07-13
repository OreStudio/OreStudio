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
#include "ores.refdata.api/domain/tenor_resolution.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using ores::refdata::domain::resolve_end_date;
using ores::refdata::domain::resolve_window;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention;
using ores::refdata::domain::tenor_convention_resolution;
using ores::refdata::domain::windows_overlap;

const std::string tags("[tenor_resolution]");

tenor make_period_tenor(std::string code, std::string unit, int multiplier) {
    tenor t;
    t.code = std::move(code);
    t.kind = "PERIOD";
    t.unit = std::move(unit);
    t.multiplier = multiplier;
    return t;
}

tenor make_special_tenor(std::string code) {
    tenor t;
    t.code = std::move(code);
    t.kind = "SPECIAL";
    return t;
}

tenor_convention make_convention(std::string code, std::string measured_from,
    std::string algorithm = "ANCHOR_OFFSET") {
    tenor_convention c;
    c.code = std::move(code);
    c.measured_from = std::move(measured_from);
    c.resolution_algorithm = std::move(algorithm);
    return c;
}

tenor_convention_resolution make_resolution(std::string convention_code, std::string tenor_code,
    std::optional<std::string> anchor_override = std::nullopt,
    std::optional<std::string> offset_unit = std::nullopt,
    std::optional<int> offset_multiplier = std::nullopt) {
    tenor_convention_resolution r;
    r.convention_code = std::move(convention_code);
    r.tenor_code = std::move(tenor_code);
    r.anchor_override = std::move(anchor_override);
    r.offset_unit = std::move(offset_unit);
    r.offset_multiplier = offset_multiplier;
    return r;
}

}

TEST_CASE("resolve_end_date_uses_tenor_multiplier_for_period_tenors_from_spot", tags) {
    using namespace std::chrono;

    const auto oneMonth = make_period_tenor("1M", "MONTH", 1);
    const auto convention = make_convention("RATES_SPOT_FORWARD", "SPOT");
    const auto resolution = make_resolution("RATES_SPOT_FORWARD", "1M");

    const year_month_day horizon{year(2026), month(1), day(15)};
    const year_month_day spot{year(2026), month(1), day(19)};

    const auto end = resolve_end_date(oneMonth, convention, resolution, horizon, spot);
    CHECK(end == year_month_day{year(2026), month(2), day(19)});
}

TEST_CASE("resolve_end_date_clamps_to_last_valid_day_of_month", tags) {
    using namespace std::chrono;

    const auto oneMonth = make_period_tenor("1M", "MONTH", 1);
    const auto convention = make_convention("RATES_SPOT_FORWARD", "SPOT");
    const auto resolution = make_resolution("RATES_SPOT_FORWARD", "1M");

    // 31 Jan + 1M has no 31 Feb: must clamp to the last day of February.
    const year_month_day horizon{year(2026), month(1), day(29)};
    const year_month_day spot{year(2026), month(1), day(31)};

    const auto end = resolve_end_date(oneMonth, convention, resolution, horizon, spot);
    CHECK(end == year_month_day{year(2026), month(2), day(28)});
}

TEST_CASE("resolve_end_date_uses_resolution_row_offset_for_special_tenors", tags) {
    using namespace std::chrono;

    const auto overnight = make_special_tenor("O/N");
    const auto convention = make_convention("RATES_SPOT_FORWARD", "SPOT");
    // O/N under spot/forward: horizon to tomorrow.
    const auto resolution = make_resolution("RATES_SPOT_FORWARD", "O/N", "TODAY", "DAY", 1);

    const year_month_day horizon{year(2026), month(1), day(15)};
    const year_month_day spot{year(2026), month(1), day(19)};

    const auto end = resolve_end_date(overnight, convention, resolution, horizon, spot);
    CHECK(end == year_month_day{year(2026), month(1), day(16)});
}

TEST_CASE("resolve_end_date_resolves_same_tenor_differently_by_convention", tags) {
    using namespace std::chrono;

    const auto overnight = make_special_tenor("O/N");
    const year_month_day horizon{year(2026), month(1), day(15)};
    const year_month_day spot{year(2026), month(1), day(19)};

    // Spot/forward: O/N resolves to tomorrow (horizon + 1 day).
    const auto spotForward = make_convention("RATES_SPOT_FORWARD", "SPOT");
    const auto spotForwardResolution =
        make_resolution("RATES_SPOT_FORWARD", "O/N", "TODAY", "DAY", 1);
    const auto spotForwardEnd =
        resolve_end_date(overnight, spotForward, spotForwardResolution, horizon, spot);

    // FX swap near-leg: O/N resolves to today (horizon + 0 days) -- the same tenor code, a
    // different date, purely because the persisted rows differ. No per-label knowledge in code.
    const auto swap = make_convention("FX_SWAP_NEAR_LEG", "");
    const auto swapResolution = make_resolution("FX_SWAP_NEAR_LEG", "O/N", "TODAY", "DAY", 0);
    const auto swapEnd = resolve_end_date(overnight, swap, swapResolution, horizon, spot);

    CHECK(spotForwardEnd == year_month_day{year(2026), month(1), day(16)});
    CHECK(swapEnd == year_month_day{year(2026), month(1), day(15)});
    CHECK(spotForwardEnd != swapEnd);
}

TEST_CASE("resolve_end_date_rejects_tenor_not_a_member_of_the_convention", tags) {
    const auto overnight = make_special_tenor("O/N");
    const auto convention = make_convention("RATES_SPOT_FORWARD", "SPOT");

    CHECK_THROWS_AS(
        resolve_end_date(overnight, convention, std::nullopt, std::chrono::year_month_day{},
            std::chrono::year_month_day{}),
        std::invalid_argument);
}

TEST_CASE("resolve_end_date_rejects_special_tenor_missing_offset", tags) {
    const auto overnight = make_special_tenor("O/N");
    const auto convention = make_convention("RATES_SPOT_FORWARD", "SPOT");
    const auto resolution = make_resolution("RATES_SPOT_FORWARD", "O/N");

    CHECK_THROWS_AS(
        resolve_end_date(overnight, convention, resolution, std::chrono::year_month_day{},
            std::chrono::year_month_day{}),
        std::invalid_argument);
}

TEST_CASE("resolve_end_date_throws_not_implemented_for_imm_roll", tags) {
    const auto oneYear = make_period_tenor("1Y", "YEAR", 1);
    const auto cds = make_convention("CREDIT_CDS_IMM", "", "IMM_ROLL");
    const auto resolution = make_resolution("CREDIT_CDS_IMM", "1Y", std::nullopt, "ROLL_QUARTER", 4);

    CHECK_THROWS_AS(
        resolve_end_date(oneYear, cds, resolution, std::chrono::year_month_day{},
            std::chrono::year_month_day{}),
        std::logic_error);
}

TEST_CASE("windows_overlap_detects_overlapping_windows", tags) {
    using namespace std::chrono;

    const auto threeMonths = make_period_tenor("3M", "MONTH", 3);
    const auto convention = make_convention("RATES_SPOT_FORWARD", "SPOT");
    const auto resolution = make_resolution("RATES_SPOT_FORWARD", "3M");

    const year_month_day horizon{year(2026), month(1), day(1)};
    const year_month_day laterHorizon{year(2026), month(3), day(1)};
    const year_month_day spot{year(2026), month(1), day(3)};
    const year_month_day laterSpot{year(2026), month(3), day(3)};

    auto deposit = resolve_window(threeMonths, convention, resolution, horizon, spot);
    auto overlapping =
        resolve_window(threeMonths, convention, resolution, laterHorizon, laterSpot);

    CHECK(windows_overlap(deposit, overlapping));
}

TEST_CASE("windows_overlap_treats_windows_as_half_open", tags) {
    using namespace std::chrono;

    const auto oneMonth = make_period_tenor("1M", "MONTH", 1);
    const auto convention = make_convention("RATES_SPOT_FORWARD", "SPOT");
    const auto resolution = make_resolution("RATES_SPOT_FORWARD", "1M");

    const year_month_day horizon{year(2026), month(1), day(1)};
    const year_month_day spot{year(2026), month(1), day(3)};

    auto first = resolve_window(oneMonth, convention, resolution, horizon, spot);
    // Second window starts exactly where the first ends: half-open windows must not count that
    // as an overlap.
    auto second = resolve_window(oneMonth, convention, resolution, first.end, first.end);

    CHECK_FALSE(windows_overlap(first, second));
}
