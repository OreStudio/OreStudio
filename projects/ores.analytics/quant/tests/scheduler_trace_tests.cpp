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
#include "ores.analytics.quant/service/scheduler_trace.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

using ores::analytics::quant::domain::arrival_spec;
using ores::analytics::quant::domain::market_config;
using ores::analytics::quant::domain::market_state;
using ores::analytics::quant::service::manual_run_clock;
using ores::analytics::quant::service::tick_context;
using ores::analytics::quant::service::tick_scheduler;
using ores::analytics::quant::service::trace_event;
using ores::analytics::quant::service::trace_kind;
using ores::analytics::quant::service::trace_ring;

namespace {

const auto trace_wall_origin = std::chrono::system_clock::time_point(std::chrono::days(20000));

const market_config traced_market{
    .wake_rate = 2.0, .calm_rate = 3.0, .active_multiplier = 2.0, .jump = 0.0, .decay = 1.0};

arrival_spec every_second(double rate) {
    return arrival_spec{.mode = "periodic", .ticks_per_hour = rate * 3600.0};
}

std::string silent(const tick_context&) {
    return {};
}

}

TEST_CASE("the trace of a fixed-seed run on the manual clock is exact", "[scheduler_trace]") {
    manual_run_clock clock(trace_wall_origin);
    tick_scheduler scheduler(clock, traced_market, 42);
    scheduler.pin(market_state::calm);
    scheduler.add({.series = 7, .arrival = every_second(2.0), .name = "fast"}, silent);
    scheduler.add({.series = 9, .arrival = every_second(1.0), .sensitivity = 2.0, .name = "slow"},
                  silent);
    for (int i = 0; i < 6; ++i)
        scheduler.step();
    scheduler.pin(market_state::active);
    for (int i = 0; i < 4; ++i)
        scheduler.step();

    const auto page = scheduler.trace(std::nullopt, 1e9);
    REQUIRE(page.events.size() == 14);
    CHECK(page.events[0].kind == trace_kind::pin);
    CHECK(page.events[1].kind == trace_kind::add);
    CHECK(page.events[1].series == 7);
    CHECK(page.events[2].kind == trace_kind::add);
    CHECK(page.events[2].series == 9);
    CHECK(page.events[9].kind == trace_kind::pin);
    CHECK(page.events[9].state == market_state::active);
    CHECK(page.names.at(7) == "fast");
    CHECK(page.names.at(9) == "slow");

    // The clock time of a tick is its tau over the pace in force: a third while calm, and two
    // from the instant the pin to active applied.
    double last_tau = 0.0;
    double tau_at_pin = 0.0;
    double at_pin = 0.0;
    std::vector<std::uint64_t> seen;
    for (std::size_t i = 3; i < page.events.size(); ++i) {
        const auto& e = page.events[i];
        if (e.kind == trace_kind::pin) {
            tau_at_pin = e.tau;
            at_pin = e.at;
            continue;
        }
        REQUIRE(e.kind == trace_kind::tick);
        CHECK(e.tau >= last_tau);
        // A first tick comes after a drawn phase; after it, fast's law gap is half an
        // operational second and slow's a whole one, which is half a unit of tau at its
        // sensitivity of two.
        if (std::ranges::find(seen, e.series) != seen.end())
            CHECK(e.operational_gap == Catch::Approx(e.series == 7 ? 0.5 : 1.0).epsilon(1e-12));
        seen.push_back(e.series);
        const double expected_at = i < 9 ? e.tau * 3.0 : at_pin + (e.tau - tau_at_pin) / 2.0;
        CHECK(e.at == Catch::Approx(expected_at).epsilon(1e-12));
        last_tau = e.tau;
    }
}

TEST_CASE("the trace stamps each regime turn with the instant it happened", "[scheduler_trace]") {
    manual_run_clock clock(trace_wall_origin);
    tick_scheduler scheduler(clock, traced_market, 5);
    scheduler.add({.series = 1, .arrival = every_second(20.0), .name = "busy"}, silent);
    for (int i = 0; i < 400; ++i)
        scheduler.step();

    const auto page = scheduler.trace(std::nullopt, 1e9);
    int turns = 0;
    auto state = page.events.front().state;
    double last_turn = 0.0;
    for (const auto& e : page.events) {
        if (e.kind == trace_kind::transition) {
            ++turns;
            CHECK(e.state != state);
            CHECK(e.at >= last_turn);
            state = e.state;
            last_turn = e.at;
        } else if (e.kind == trace_kind::tick) {
            // Every tick after a turn happens in the state that turn entered.
            CHECK(e.state == state);
        }
    }
    CHECK(turns > 5);
}

TEST_CASE("the trace is filtered by series and by window", "[scheduler_trace]") {
    manual_run_clock clock(trace_wall_origin);
    tick_scheduler scheduler(clock, traced_market, 3);
    scheduler.pin(market_state::active);
    scheduler.add({.series = 1, .arrival = every_second(4.0), .name = "a"}, silent);
    scheduler.add({.series = 2, .arrival = every_second(1.0), .name = "b"}, silent);
    for (int i = 0; i < 50; ++i)
        scheduler.step();

    const auto only_b = scheduler.trace(2, 1e9);
    for (const auto& e : only_b.events)
        CHECK((e.series == 2 || e.series == 0));
    CHECK_FALSE(only_b.names.contains(1));

    // At pace two, a fast series' 4 ticks a second come 8 a second: the last second of the
    // run holds about 8 of a's ticks and 2 of b's.
    const auto recent = scheduler.trace(std::nullopt, 1.0);
    std::size_t ticks = 0;
    for (const auto& e : recent.events)
        ticks += e.kind == trace_kind::tick ? 1 : 0;
    CHECK(ticks >= 9);
    CHECK(ticks <= 11);
}

TEST_CASE("the trace ring keeps its capacity and the newest events", "[scheduler_trace]") {
    trace_ring ring(4);
    for (int i = 0; i < 10; ++i)
        ring.record(trace_event{.kind = trace_kind::tick,
                                .series = static_cast<std::uint64_t>(i),
                                .at = static_cast<double>(i),
                                .tau = 0.0,
                                .operational_gap = 0.0,
                                .wall_gap = 0.0,
                                .heat = 0.0,
                                .state = market_state::calm});
    const auto kept = ring.copy_if([](const trace_event&) { return true; });
    REQUIRE(kept.size() == 4);
    CHECK(kept.front().series == 6);
    CHECK(kept.back().series == 9);
    CHECK(ring.written() == 10);
}

TEST_CASE("the scheduler reports its rate, heap and lateness", "[scheduler_trace]") {
    manual_run_clock clock(trace_wall_origin);
    tick_scheduler scheduler(clock, traced_market, 1);
    scheduler.add({.series = 1, .arrival = every_second(1.0), .name = "a"}, silent);
    scheduler.add({.series = 2, .arrival = every_second(1.0), .name = "b"}, silent);
    const auto stats = scheduler.stats();
    CHECK(stats.series == 2);
    CHECK(stats.heap_size == 2);
    CHECK(stats.lateness_seconds == 0.0);
}
