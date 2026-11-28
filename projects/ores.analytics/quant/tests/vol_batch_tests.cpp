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
#include "ores.analytics.quant/math/vol_batch.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <expected>
#include <future>
#include <random>
#include <string>
#include <vector>

using namespace ores::analytics::quant::math;

namespace {

using std::chrono::January;
using std::chrono::sys_days;
using std::chrono::year_month_day;

/// A day the batch's histories are dated on, clear of the other suites' helpers.
year_month_day vb_observation_day(int day) {
    return year_month_day{sys_days{std::chrono::year{2026} / January / day}};
}

/**
 * A rate that wanders from four percent, one observation a day, on a seed of its own.
 *
 * A batch is about many series measured together, so each series needs a history that is plausible
 * and reproducible: a random walk of a few basis points a day, held inside a band.
 */
std::vector<dated_level> vb_wandering_rate(int count, std::uint32_t seed) {
    std::mt19937 generator(seed);
    std::normal_distribution<double> step(0.0, 0.0002);
    std::vector<dated_level> out;
    out.reserve(static_cast<std::size_t>(count));
    double rate = 0.04;
    for (int i = 0; i < count; ++i) {
        rate = std::clamp(rate + step(generator), 0.005, 0.12);
        out.push_back({.date = vb_observation_day(i + 1), .level = rate});
    }
    return out;
}

/// The batch's view of the same histories, which is all a batch takes: levels and a ladder.
std::vector<std::span<const dated_level>>
vb_histories(const std::vector<std::vector<dated_level>>& levels) {
    std::vector<std::span<const dated_level>> out;
    out.reserve(levels.size());
    for (const auto& history : levels)
        out.emplace_back(history);
    return out;
}

}

TEST_CASE("a batch measures every series over the ladder, and a row is where its offset says",
          "[vol_batch]") {
    const std::vector<double> ladder{0.25, 1.0};
    const std::vector<std::vector<dated_level>> histories{
        vb_wandering_rate(400, 1), vb_wandering_rate(400, 2), vb_wandering_rate(400, 3)};
    const auto series = vb_histories(histories);

    const auto batch = measure_batch(series, ladder, volatility_transform::difference, {});
    REQUIRE(batch);
    CHECK(batch->series_count() == 3);
    CHECK(batch->pillar_count() == 2);
    REQUIRE(batch->offsets.size() == 4);
    CHECK(batch->offsets[0] == 0);
    CHECK(batch->offsets[1] == 2);
    CHECK(batch->offsets[2] == 4);
    CHECK(batch->offsets[3] == 6);
    CHECK(batch->states.size() == 6);
    CHECK(batch->levels.size() == 6);

    // One row at a time, the batch says what the engine says: the returns a window holds, the level
    // over it, and the shape of the series scaled to the cell's own life.
    for (std::size_t row = 0; row < histories.size(); ++row) {
        const auto returns = returns_of(histories[row], volatility_transform::difference);
        const auto moments = measure_series(returns.returns, {});
        REQUIRE(moments);
        const std::size_t first = batch->offset_of(row);
        for (std::size_t cell = 0; cell < ladder.size(); ++cell) {
            INFO("series " << row << " cell " << cell);
            REQUIRE(batch->states[first + cell] == vol_cell_state::quoted);
            const auto pillar = measure_pillar(returns.returns, ladder[cell], {});
            REQUIRE(pillar);
            CHECK(batch->windows[first + cell] == pillar->window);
            CHECK(batch->levels[first + cell] == Catch::Approx(pillar->level).epsilon(1e-10));
            const auto scaled = over_days(moments->shape, ladder[cell] * 252.0);
            CHECK(batch->skewnesses[first + cell] == Catch::Approx(scaled.skewness).epsilon(1e-10));
            CHECK(batch->excess_kurtoses[first + cell] ==
                  Catch::Approx(scaled.excess_kurtosis).epsilon(1e-10));
        }
    }
}

TEST_CASE("a batch's rows can be filled at once, in any order, with the same answer",
          "[vol_batch]") {
    const std::vector<double> ladder{0.25, 1.0};
    std::vector<std::vector<dated_level>> histories;
    for (std::uint32_t seed = 10; seed < 14; ++seed)
        histories.push_back(vb_wandering_rate(400, seed));
    const auto series = vb_histories(histories);

    const auto serial = measure_batch(series, ladder, volatility_transform::difference, {});
    REQUIRE(serial);

    // The same batch, allocated empty and filled one row per task: every task writes only its own
    // slice, and nothing but the caller's inputs is read, so the answers cannot disagree.
    auto concurrent = make_vol_batch(series.size(), ladder);
    std::vector<std::future<std::expected<void, std::string>>> tasks;
    for (std::size_t row = 0; row < series.size(); ++row) {
        tasks.push_back(std::async(std::launch::async, [&, row] {
            auto slice = row_of(concurrent, row);
            return measure_row(series[row], volatility_transform::difference, {}, slice);
        }));
    }
    for (auto& task : tasks) {
        const auto measured = task.get();
        REQUIRE(measured);
    }

    CHECK(concurrent.states == serial->states);
    CHECK(concurrent.windows == serial->windows);
    CHECK(concurrent.levels == serial->levels);
    CHECK(concurrent.skewnesses == serial->skewnesses);
    CHECK(concurrent.excess_kurtoses == serial->excess_kurtoses);
    CHECK(concurrent.reason_of == serial->reason_of);
}

TEST_CASE("a refused series and an uncovered pillar cost their own cells, and every cell says why",
          "[vol_batch]") {
    const std::vector<double> ladder{0.25, 1.0};
    const auto good = vb_wandering_rate(400, 20);
    const auto flat = vb_wandering_rate(400, 21);
    std::vector<dated_level> constant;
    constant.reserve(flat.size());
    for (const auto& level : flat)
        constant.push_back({.date = level.date, .level = 0.04});
    // Two levels are one change, which is no sample; a hundred levels are ninety-nine changes,
    // which cover the quarter's window and not the year's.
    const std::vector<dated_level> barely{good[0], good[1]};
    const auto short_history = vb_wandering_rate(100, 22);

    const std::vector<std::vector<dated_level>> histories{good, barely, constant, short_history};
    const auto series = vb_histories(histories);

    const auto batch = measure_batch(series, ladder, volatility_transform::difference, {});
    REQUIRE(batch);

    // The good series stands whatever the others do.
    const std::size_t good_first = batch->offset_of(0);
    CHECK(batch->states[good_first] == vol_cell_state::quoted);
    CHECK(batch->states[good_first + 1] == vol_cell_state::quoted);

    // A series with no return is refused whole, and its refusal is the series'.
    const std::size_t barely_first = batch->offset_of(1);
    for (std::size_t cell = 0; cell < ladder.size(); ++cell)
        CHECK(batch->states[barely_first + cell] == vol_cell_state::refused);
    CHECK(batch->reasons.at(batch->reason_offset_of(1)) ==
          "the series has no return: it holds fewer than two observations");

    // So is a series with no spread.
    const std::size_t flat_first = batch->offset_of(2);
    for (std::size_t cell = 0; cell < ladder.size(); ++cell)
        CHECK(batch->states[flat_first + cell] == vol_cell_state::refused);
    CHECK(batch->reasons.at(batch->reason_offset_of(2)) == "the returns have no spread");

    // A short history quotes the pillar it covers and states the one it does not.
    const std::size_t short_first = batch->offset_of(3);
    CHECK(batch->states[short_first] == vol_cell_state::quoted);
    CHECK(batch->states[short_first + 1] == vol_cell_state::unreached);
    CHECK(batch->reasons.at(batch->reason_offset_of(3)) ==
          "the pillar of 1 years needs 252 returns; the sample holds 99");

    // Every cell that is not quoted states why, and holds zeroes rather than a half answer.
    for (std::size_t cell = 0; cell < batch->states.size(); ++cell) {
        if (batch->states[cell] == vol_cell_state::quoted)
            continue;
        INFO("cell " << cell);
        REQUIRE(batch->reason_of[cell] >= 0);
        CHECK(batch->windows[cell] == 0);
        CHECK(batch->levels[cell] == 0.0);
        CHECK(batch->skewnesses[cell] == 0.0);
        CHECK(batch->excess_kurtoses[cell] == 0.0);
    }
}

TEST_CASE("the transform is the class's, and the batch applies whichever it is handed",
          "[vol_batch]") {
    const std::vector<double> ladder{0.25};
    const std::vector<std::vector<dated_level>> histories{vb_wandering_rate(400, 30)};
    const auto series = vb_histories(histories);

    const auto rategrid = measure_batch(series, ladder, volatility_transform::difference, {});
    const auto pricegrid = measure_batch(series, ladder, volatility_transform::log_return, {});
    REQUIRE(rategrid);
    REQUIRE(pricegrid);
    CHECK(rategrid->levels[0] != pricegrid->levels[0]);

    // A level that is not positive is no observation of a log return, and the return spans the
    // gap instead of reading a substituted value: the batch measures what returns_of measured.
    std::vector<dated_level> crossed = histories[0];
    crossed[350].level = -0.01;
    const std::vector<std::vector<dated_level>> with_crossing{crossed};
    const auto crossed_series = vb_histories(with_crossing);
    const auto crossing =
        measure_batch(crossed_series, ladder, volatility_transform::log_return, {});
    REQUIRE(crossing);
    CHECK(crossing->windows[0] == rategrid->windows[0]);
    CHECK(crossing->levels[0] != pricegrid->levels[0]);
}

TEST_CASE("the batch's ladder is the caller's, not the order the pass reaches the windows",
          "[vol_batch]") {
    const std::vector<double> ladder{3.0, 0.25, 1.0};
    const std::vector<std::vector<dated_level>> histories{vb_wandering_rate(400, 40)};
    const auto series = vb_histories(histories);

    const auto batch = measure_batch(series, ladder, volatility_transform::difference, {});
    REQUIRE(batch);
    REQUIRE(batch->states.size() == 3);
    // Three years is beyond four hundred observations; the other two are in their own places.
    CHECK(batch->states[0] == vol_cell_state::unreached);
    CHECK(batch->states[1] == vol_cell_state::quoted);
    CHECK(batch->states[2] == vol_cell_state::quoted);
    CHECK(batch->windows[1] == 63);
    CHECK(batch->windows[2] == 252);
    CHECK(batch->pillars[1] == 0.25);
}

TEST_CASE("the batch refuses only what a caller gets wrong, and states the rest", "[vol_batch]") {
    const std::vector<double> ladder{1.0};
    const std::vector<std::vector<dated_level>> histories{vb_wandering_rate(400, 50)};
    const auto series = vb_histories(histories);

    CHECK_FALSE(
        measure_batch(series, ladder, volatility_transform::difference, {.periods_per_year = 0.0}));

    // A row that is not the ladder's length is a caller error, and not a refusal.
    auto whole = make_vol_batch(1, ladder);
    auto short_row = row_of(whole, 0);
    short_row.levels = short_row.levels.first(0);
    const auto refused = measure_row(series[0], volatility_transform::difference, {}, short_row);
    REQUIRE_FALSE(refused);
    CHECK(refused.error() == "the row does not hold the 1 cell(s) of the ladder");
}
