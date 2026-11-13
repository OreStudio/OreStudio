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
#include "ores.analytics.quant/math/rng_stream.hpp"
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <string>
#include <vector>

using namespace ores::synthetic::feed;
using ores::analytics::quant::domain::curve_point;
using ores::analytics::quant::service::hull_white_process;

namespace {

const std::chrono::year_month_day board_curve_spot{
    std::chrono::year{2026}, std::chrono::November, std::chrono::day{16}};

std::chrono::year_month_day board_curve_day(int offset) {
    return std::chrono::year_month_day(std::chrono::sys_days(board_curve_spot) +
                                       std::chrono::days(offset));
}

/// A flat three percent Hull-White curve: zero volatility, so the discount factors are the
/// curve's own and a reader can check them by hand.
hull_white_process board_flat_curve() {
    return hull_white_process(
        0.1,
        std::vector<double>{0.03},
        0.0,
        0.03,
        ores::analytics::quant::math::rng_stream(ores::analytics::quant::math::stream_key{}),
        1.0 / 365.0);
}

ir_curve_resolved_entry board_entry(int offset, std::size_t ticks) {
    return {.sequence_index = 0,
            .point_id = std::to_string(offset),
            .start_tenor_code = "SPOT",
            .curve_role = "DEPOSIT",
            .ticks_ahead_start = 0,
            .ticks_ahead_end = ticks,
            .year_fraction = static_cast<double>(offset) / 365.0,
            .fixed_leg_schedule = {},
            .start_date = board_curve_spot,
            .end_date = board_curve_day(offset)};
}

}

TEST_CASE("the board's curve starts at the spot date, at one", "[ir_curve_board_points]") {
    const auto process = board_flat_curve();
    const std::vector<ir_curve_resolved_entry> entries{board_entry(90, 90), board_entry(365, 365)};
    std::vector<curve_point> points;

    append_curve_points(board_curve_spot, entries, process, points);

    REQUIRE(points.size() == 3);
    CHECK(points[0].date == board_curve_spot);
    CHECK(points[0].discount_factor == 1.0);
}

TEST_CASE("the board's curve carries the process's own factor at each pillar",
          "[ir_curve_board_points]") {
    const auto process = board_flat_curve();
    const std::vector<ir_curve_resolved_entry> entries{board_entry(90, 90), board_entry(365, 365)};
    std::vector<curve_point> points;

    append_curve_points(board_curve_spot, entries, process, points);

    REQUIRE(points.size() == 3);
    CHECK(points[1].date == board_curve_day(90));
    CHECK(points[1].discount_factor == process.discount_factor(90));
    CHECK(points[2].date == board_curve_day(365));
    CHECK(points[2].discount_factor == process.discount_factor(365));
    // And the curve is a flat three percent, which is where the numbers come from.
    CHECK(points[2].discount_factor == Catch::Approx(std::exp(-0.03)).epsilon(1e-12));
}

TEST_CASE("an entry whose date does not advance is left out", "[ir_curve_board_points]") {
    // The board's points answer in ascending date order, so a second entry ending on the same
    // day would break the rule a reader interpolates by.
    const auto process = board_flat_curve();
    const std::vector<ir_curve_resolved_entry> entries{
        board_entry(90, 90), board_entry(90, 90), board_entry(365, 365)};
    std::vector<curve_point> points;

    append_curve_points(board_curve_spot, entries, process, points);

    REQUIRE(points.size() == 3);
    for (std::size_t i = 1; i < points.size(); ++i)
        CHECK(points[i - 1].date < points[i].date);
}

TEST_CASE("a curve rebuilt every tick writes into the buffer it was given",
          "[ir_curve_board_points]") {
    // The feed's buffer is the point of the function's shape: after the first call of a
    // curve's size, a tick must not allocate.
    const auto process = board_flat_curve();
    const std::vector<ir_curve_resolved_entry> entries{board_entry(90, 90), board_entry(365, 365)};
    std::vector<curve_point> points;

    append_curve_points(board_curve_spot, entries, process, points);
    const auto first = points.data();
    for (int tick = 0; tick < 100; ++tick) {
        append_curve_points(board_curve_spot, entries, process, points);
        CHECK(points.data() == first);
        CHECK(points.size() == 3);
    }
}
