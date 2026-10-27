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
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include <algorithm>
#include <atomic>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <map>
#include <stdexcept>
#include <thread>
#include <vector>

using namespace std::chrono_literals;
using ores::analytics::quant::domain::arrival_spec;
using ores::analytics::quant::domain::hawkes_law;
using ores::analytics::quant::domain::market_config;
using ores::analytics::quant::domain::market_state;
using ores::analytics::quant::domain::periodic_law;
using ores::analytics::quant::math::draw_purpose;
using ores::analytics::quant::math::rng_stream;
using ores::analytics::quant::math::stream_key;
using ores::analytics::quant::service::manual_run_clock;
using ores::analytics::quant::service::series_spec;
using ores::analytics::quant::service::steady_run_clock;
using ores::analytics::quant::service::tick_context;
using ores::analytics::quant::service::tick_scheduler;

namespace {

const auto scheduler_wall_origin = std::chrono::system_clock::time_point(std::chrono::days(20000));

/// A market with no heat, so tau is the multiplier times the time.
const market_config no_heat{
    .wake_rate = 2.0, .calm_rate = 3.0, .active_multiplier = 2.0, .jump = 0.0, .decay = 1.0};

arrival_spec per_second(const char* mode, double rate, double n = 0.0, double decay = 1.0) {
    return arrival_spec{
        .mode = mode, .ticks_per_hour = rate * 3600.0, .branching_ratio = n, .decay = decay};
}

/// The law a scheduler with @p run_seed builds for @p spec, built again here.
ores::analytics::quant::domain::arrival_law
twin_law(std::uint64_t run_seed, const series_spec& spec, rng_stream& stream) {
    stream = rng_stream(
        stream_key{.run_seed = run_seed, .series = spec.series, .purpose = draw_purpose::arrival});
    return ores::analytics::quant::domain::make_arrival_law(spec.arrival, stream);
}

std::string quiet(const tick_context&) {
    return {};
}

double ks_exponential(std::vector<double> xs) {
    std::ranges::sort(xs);
    const auto n = static_cast<double>(xs.size());
    double d = 0.0;
    for (std::size_t i = 0; i < xs.size(); ++i) {
        const double f = 1.0 - std::exp(-xs[i]);
        d = std::max({d, f - static_cast<double>(i) / n, static_cast<double>(i + 1) / n - f});
    }
    return d;
}

}

TEST_CASE("the scheduler ticks an exact sequence across a pinned regime change",
          "[tick_scheduler]") {
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, no_heat, 42);
    scheduler.pin(market_state::calm);

    const series_spec a{.series = 1, .arrival = per_second("periodic", 4.0), .sensitivity = 1.0};
    const series_spec b{.series = 2, .arrival = per_second("periodic", 1.0), .sensitivity = 2.0};
    const auto id_a = scheduler.add(a, quiet);
    const auto id_b = scheduler.add(b, quiet);

    rng_stream unused(stream_key{.run_seed = 0, .series = 0, .purpose = draw_purpose::arrival});
    const double phase_a = std::get<periodic_law>(twin_law(42, a, unused)).offset;
    const double phase_b = std::get<periodic_law>(twin_law(42, b, unused)).offset;

    // In tau, a fires every quarter and b, twice as sensitive, every half.
    std::vector<std::pair<double, std::size_t>> marks;
    for (int k = 0; k < 40; ++k) {
        marks.emplace_back((phase_a + k) / 4.0, id_a);
        marks.emplace_back((phase_b + k) / 2.0, id_b);
    }
    std::ranges::sort(marks);

    const double calm = 1.0 / 3.0;
    const double active = 2.0;
    double tau_at_pin = 0.0;
    for (std::size_t i = 0; i < 30; ++i) {
        // Pinned calm, tau runs at a third of the time; after ten ticks the pin
        // moves to active, and from that tick's instant tau runs at two.
        if (i == 10) {
            tau_at_pin = marks[9].first;
            scheduler.pin(market_state::active);
        }
        const auto fired = scheduler.step();
        REQUIRE(fired);
        const double tau = marks[i].first;
        const double at = i < 10 ? tau / calm : tau_at_pin / calm + (tau - tau_at_pin) / active;
        CHECK(fired->id == marks[i].second);
        CHECK(fired->tau == Catch::Approx(tau).epsilon(1e-12));
        CHECK(fired->at == Catch::Approx(at).epsilon(1e-12));
    }
}

TEST_CASE("each series keeps its own law in operational time", "[tick_scheduler]") {
    // A busy market with heat: the pace moves all the time, and each series'
    // gaps in its own operational time must still be its law's gaps.
    const market_config busy{
        .wake_rate = 2.0, .calm_rate = 3.0, .active_multiplier = 2.0, .jump = 0.5, .decay = 2.0};
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, busy, 7);

    const series_spec hawkes{
        .series = 11, .arrival = per_second("hawkes", 50.0, 0.5, 10.0), .sensitivity = 2.0};
    const series_spec poisson{
        .series = 12, .arrival = per_second("poisson", 20.0), .sensitivity = 0.5};
    const auto id_h = scheduler.add(hawkes, quiet);
    scheduler.add(poisson, quiet);

    std::map<std::size_t, std::vector<double>> taus;
    for (int i = 0; i < 60000; ++i) {
        const auto fired = scheduler.step();
        taus[fired->id].push_back(fired->tau);
    }

    rng_stream stream(stream_key{.run_seed = 0, .series = 0, .purpose = draw_purpose::arrival});
    auto law = twin_law(7, hawkes, stream);
    std::vector<double> compensated;
    double last = 0.0;
    for (const double tau : taus[id_h]) {
        const double gap = ores::analytics::quant::domain::draw_gap(law, stream);
        CHECK((tau - last) * hawkes.sensitivity == Catch::Approx(gap).margin(1e-9));
        const auto& h = std::get<hawkes_law>(law);
        compensated.push_back(h.baseline * gap +
                              h.excess * (1.0 - std::exp(-h.decay * gap)) / h.decay);
        ores::analytics::quant::domain::record_event(law, gap);
        last = tau;
    }
    REQUIRE(compensated.size() > 10000);
    CHECK(ks_exponential(compensated) < 1.63 / std::sqrt(static_cast<double>(compensated.size())));
}

TEST_CASE("a series ticks at its configured rate when the mean pace is one", "[tick_scheduler]") {
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, no_heat, 3);
    scheduler.add({.series = 5, .arrival = per_second("poisson", 10.0)}, quiet);

    constexpr int ticks = 50000;
    double at = 0.0;
    for (int i = 0; i < ticks; ++i)
        at = scheduler.step()->at;
    CHECK(ticks / at == Catch::Approx(10.0).epsilon(0.03));
}

TEST_CASE("a tick costs O(log n) in the number of series", "[tick_scheduler]") {
    const auto cost_per_step = [](std::size_t n) {
        manual_run_clock clock(scheduler_wall_origin);
        tick_scheduler scheduler(clock, no_heat, 1);
        for (std::size_t i = 0; i < n; ++i)
            scheduler.add({.series = i, .arrival = per_second("poisson", 1.0)}, quiet);
        constexpr int steps = 20000;
        const auto start = std::chrono::steady_clock::now();
        for (int i = 0; i < steps; ++i)
            scheduler.step();
        return std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count() /
               steps;
    };

    // A hundred times the series would cost a hundred times as much in O(n);
    // in O(log n) it costs about one and two thirds as much.
    const double small = cost_per_step(1000);
    const double large = cost_per_step(100000);
    INFO("seconds per step: " << small << " at 1,000 series, " << large << " at 100,000");
    CHECK(large < 5.0 * small);
}

TEST_CASE("a body that throws is reported and its series keeps ticking", "[tick_scheduler]") {
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, no_heat, 1);
    int failures = 0;
    scheduler.set_hooks([](std::uint64_t, const std::string&) {},
                        [&](const std::exception&) { ++failures; });
    int calls = 0;
    scheduler.add({.series = 1, .arrival = per_second("periodic", 1.0)},
                  [&](const tick_context&) -> std::string {
                      if (++calls == 1)
                          throw std::runtime_error("first tick failed");
                      return "ok";
                  });

    scheduler.step();
    scheduler.step();
    scheduler.step();
    CHECK(calls == 3);
    CHECK(failures == 1);
    CHECK(scheduler.publish_count() == 2);
}

TEST_CASE("a body is told the wall time and the wait from the run clock", "[tick_scheduler]") {
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, no_heat, 1);
    scheduler.pin(market_state::active);
    std::vector<tick_context> seen;
    scheduler.add({.series = 1, .arrival = per_second("periodic", 1.0)},
                  [&](const tick_context& c) {
                      seen.push_back(c);
                      return std::string{};
                  });

    const auto first = scheduler.step();
    const auto second = scheduler.step();
    // Active doubles the pace, so a one-second gap takes half a second.
    CHECK(second->at - first->at == Catch::Approx(0.5));
    CHECK(seen[1].elapsed_seconds == Catch::Approx(0.5));
    CHECK(seen[1].observed_at - scheduler_wall_origin ==
          std::chrono::duration_cast<std::chrono::system_clock::duration>(
              std::chrono::duration<double>(second->at)));
}

TEST_CASE("an empty scheduler takes no tick", "[tick_scheduler]") {
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, no_heat, 1);
    CHECK_FALSE(scheduler.step());
    CHECK(scheduler.size() == 0);
}

TEST_CASE("a series leaves the population and the heap when removed", "[tick_scheduler]") {
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, no_heat, 1);
    const auto gone = scheduler.add({.series = 1, .arrival = per_second("periodic", 4.0)}, quiet);
    const auto kept = scheduler.add({.series = 2, .arrival = per_second("periodic", 1.0)}, quiet);
    CHECK(scheduler.snapshot().population_rate == Catch::Approx(5.0));

    scheduler.remove(gone);
    CHECK(scheduler.size() == 1);
    CHECK(scheduler.snapshot().population_rate == Catch::Approx(1.0));
    for (int i = 0; i < 5; ++i)
        CHECK(scheduler.step()->id == kept);
}

TEST_CASE("a running scheduler applies a pin and a removal from another thread",
          "[tick_scheduler]") {
    steady_run_clock clock;
    tick_scheduler scheduler(clock, no_heat, 1);
    std::thread loop([&] { scheduler.run(); });

    std::atomic<bool> in_body{false};
    std::atomic<int> calls{0};
    const auto id = scheduler.add({.series = 1, .arrival = per_second("poisson", 1000.0)},
                                  [&](const tick_context&) {
                                      in_body = true;
                                      std::this_thread::sleep_for(1ms);
                                      ++calls;
                                      in_body = false;
                                      return std::string{};
                                  });

    scheduler.pin(market_state::active);
    const auto until = std::chrono::steady_clock::now() + 5s;
    while ((!scheduler.snapshot().pinned || calls < 20) && std::chrono::steady_clock::now() < until)
        std::this_thread::sleep_for(5ms);
    CHECK(scheduler.snapshot().pinned);
    CHECK(scheduler.snapshot().state == market_state::active);
    CHECK(calls >= 20);

    // Once remove returns the body is not running and never runs again.
    scheduler.remove(id);
    CHECK_FALSE(in_body);
    const int after = calls;
    std::this_thread::sleep_for(50ms);
    CHECK(calls == after);

    scheduler.stop();
    loop.join();
}

TEST_CASE("a running scheduler reads the heat on its sample period", "[tick_scheduler]") {
    steady_run_clock clock;
    tick_scheduler scheduler(clock, no_heat, 1);
    std::thread loop([&] { scheduler.run(); });
    std::this_thread::sleep_for(350ms);
    scheduler.stop();
    loop.join();

    const auto readings = scheduler.readings_since(std::chrono::system_clock::time_point{});
    CHECK(readings.size() >= 3);
    CHECK(readings.size() <= 6);
}
