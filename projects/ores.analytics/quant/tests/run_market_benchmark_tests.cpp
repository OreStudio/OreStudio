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
#include "ores.analytics.quant/domain/market_board.hpp"
#include "ores.analytics.quant/service/run_clock.hpp"
#include "ores.analytics.quant/service/run_market.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <format>
#include <iostream>
#include <string>
#include <vector>

/*
 * The two layout questions the market board's review left open, measured rather than argued: a
 * run's lookups, which find a series' drivers in a map on every tick, and a board of a thousand
 * members, which holds each member's points in its own vector. Hidden from the suite; run it with
 * "[.benchmark]".
 */

using namespace ores::analytics::quant::domain;
using namespace ores::analytics::quant::math;
using namespace ores::analytics::quant::service;

namespace {

/// A clock that never moves: one step fires the ticks that are due and nothing else.
class benchmark_clock final : public i_run_clock {
public:
    time_point now() const override {
        return now_;
    }
    std::chrono::system_clock::time_point wall(time_point) const override {
        return std::chrono::system_clock::time_point{};
    }
    wake wait_until(time_point) override {
        return wake::deadline;
    }
    void interrupt() override {}

private:
    time_point now_{};
};

/// How many series one block adds: its anchor, its segments, its skew and its curvature.
constexpr int benchmark_series_per_block = 8;
constexpr int benchmark_series = 1000;
constexpr int benchmark_blocks = benchmark_series / benchmark_series_per_block;
constexpr int benchmark_points_per_curve = 20;
constexpr int benchmark_calls = 2'000'000;
constexpr int benchmark_runs = 5;

template <typename F>
double nanos_per_call(int calls, F&& f) {
    const auto start = std::chrono::steady_clock::now();
    f();
    const auto elapsed =
        std::chrono::duration<double, std::nano>(std::chrono::steady_clock::now() - start).count();
    return elapsed / static_cast<double>(calls);
}

void report(std::string_view what, std::vector<double> samples) {
    std::ranges::sort(samples);
    std::cout << "tick path: " << what << ": median " << samples[samples.size() / 2] << " ns ("
              << samples.front() << "-" << samples.back() << "), " << samples.size() << " runs\n";
}

}

TEST_CASE("a run's lookups are measured against the work a tick does", "[.benchmark][run_market]") {
    benchmark_clock clock;
    run_market run(clock, {}, 42);
    for (int block = 0; block < benchmark_blocks; ++block) {
        run.add_block(series_key(std::format("synthetic.bench.spot.{}", block)),
                      std::format("synthetic.bench.surface.{}", block),
                      5,
                      0.3,
                      -0.4,
                      0.9);
    }
    run.build(3600.0, 0.0, 60.0);
    REQUIRE(run.noise().series_id.size() == static_cast<std::size_t>(benchmark_series));

    const std::vector<std::uint64_t> ids(run.noise().series_id.begin(),
                                         run.noise().series_id.end());
    std::vector<double> samples;
    std::uint64_t sink = 0;
    for (int pass = 0; pass < benchmark_runs; ++pass) {
        samples.push_back(nanos_per_call(benchmark_calls, [&] {
            for (int i = 0; i < benchmark_calls; ++i)
                sink += run.driver_count_of(ids[static_cast<std::size_t>(i) % ids.size()]);
        }));
    }
    // A tick's own body builds one sentence, which is the other thing the loop pays for. The
    // body lives in the feed that ticks, so this is the same shape built here rather than the
    // body itself: it is a reference to read the lookup against, not a regression guard.
    std::vector<double> sentences;
    for (int pass = 0; pass < benchmark_runs; ++pass)
        sentences.push_back(nanos_per_call(benchmark_calls, [&] {
            for (int i = 0; i < benchmark_calls; ++i)
                sink += std::format("{} point(s)", i).size();
        }));
    report("one driver lookup, a run of a thousand series (250 series keys)", samples);
    report("one sentence of the tick body's shape", sentences);
    // The sums keep the compiler from removing the work; they are not a claim about the numbers.
    REQUIRE(sink > 0);
}

TEST_CASE("a board's reads are measured at a thousand members", "[.benchmark][market_board]") {
    market_board board(benchmark_series);
    std::vector<market_board::member_id> members;
    members.reserve(benchmark_series);
    // One buffer, reused: each publish copies it into the member's own storage.
    std::vector<curve_point> points(benchmark_points_per_curve + 1);
    const std::chrono::year_month_day start_date{
        std::chrono::year{2026}, std::chrono::November, std::chrono::day{16}};
    for (int member = 0; member < benchmark_series; ++member) {
        for (std::size_t i = 0; i < points.size(); ++i)
            points[i] = {
                .date = std::chrono::year_month_day(std::chrono::sys_days(start_date) +
                                                    std::chrono::days(static_cast<int>(i) * 30)),
                .discount_factor = std::exp(-0.03 * static_cast<double>(i) * 30.0 / 365.0)};
        const auto id = board.member_of(market_member_kind::curve, std::format("CCY{}", member));
        board.publish_curve(id, start_date, points, std::chrono::system_clock::time_point{});
        members.push_back(id);
    }
    REQUIRE(board.size() == static_cast<std::size_t>(benchmark_series));

    // A surface reads two curves and a spot: the id it resolved when it was built, by index.
    std::vector<double> samples;
    std::size_t sink = 0;
    for (int pass = 0; pass < benchmark_runs; ++pass) {
        samples.push_back(nanos_per_call(benchmark_calls, [&] {
            for (int i = 0; i < benchmark_calls; ++i)
                sink +=
                    board.view(members[static_cast<std::size_t>(i) % members.size()]).points.size();
        }));
    }
    report("one member read, a thousand members", samples);
    REQUIRE(sink > 0);
}
