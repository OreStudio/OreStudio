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

    // A hundred times the series would cost a hundred times as much in O(n). In O(log n) the
    // comparisons grow by two thirds, and cache misses on a heap that no longer fits in cache
    // add a few times more, so the bound sits well between the two.
    const double small = cost_per_step(1000);
    const double large = cost_per_step(100000);
    INFO("seconds per step: " << small << " at 1,000 series, " << large << " at 100,000");
    CHECK(large < 20.0 * small);
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

TEST_CASE("a body may remove its own series", "[tick_scheduler]") {
    steady_run_clock clock;
    tick_scheduler scheduler(clock, no_heat, 1);
    std::thread loop([&] { scheduler.run(); });

    std::atomic<int> calls{0};
    std::atomic<std::size_t> own{0};
    // A hundred a second leaves time for add to return the id before the third tick.
    own = scheduler.add({.series = 1, .arrival = per_second("poisson", 100.0)},
                        [&](const tick_context&) {
                            if (++calls == 3)
                                scheduler.remove(own);
                            return std::string{};
                        });

    // The add is posted to the loop, so the size can read zero before it lands: wait for the
    // third call itself.
    const auto until = std::chrono::steady_clock::now() + 5s;
    while (calls < 3 && std::chrono::steady_clock::now() < until)
        std::this_thread::sleep_for(5ms);
    std::this_thread::sleep_for(50ms);
    CHECK(scheduler.size() == 0);
    CHECK(calls == 3);

    scheduler.stop();
    loop.join();
}

TEST_CASE("a remove that waits on a busy loop returns when the loop stops", "[tick_scheduler]") {
    steady_run_clock clock;
    tick_scheduler scheduler(clock, no_heat, 1);
    std::thread loop([&] { scheduler.run(); });

    std::atomic<bool> in_body{false};
    const auto id = scheduler.add({.series = 1, .arrival = per_second("periodic", 1000.0)},
                                  [&](const tick_context&) {
                                      in_body = true;
                                      std::this_thread::sleep_for(100ms);
                                      return std::string{};
                                  });
    const auto until = std::chrono::steady_clock::now() + 5s;
    while (!in_body && std::chrono::steady_clock::now() < until)
        std::this_thread::sleep_for(1ms);

    std::thread remover([&] { scheduler.remove(id); });
    scheduler.stop();
    remover.join();
    loop.join();
    CHECK(scheduler.size() == 0);
}

TEST_CASE("the heap drops the marks of removed series", "[tick_scheduler]") {
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, no_heat, 1);
    const auto kept = scheduler.add({.series = 0, .arrival = per_second("poisson", 1.0)}, quiet);
    for (std::size_t i = 1; i <= 1000; ++i)
        scheduler.remove(
            scheduler.add({.series = i, .arrival = per_second("poisson", 0.001)}, quiet));

    CHECK(scheduler.size() == 1);
    for (int i = 0; i < 10; ++i)
        CHECK(scheduler.step()->id == kept);
}

TEST_CASE("a body is told the simulated time its tick covers, at the market's speed",
          "[tick_scheduler]") {
    // A periodic series at one a second ticks every unit of tau, so at a speed of ten each
    // tick after the first covers ten simulated seconds, whatever the pace: a busy market
    // brings the ticks sooner and leaves the step alone.
    auto fast = no_heat;
    fast.speed = 10.0;
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, fast, 1);
    scheduler.pin(market_state::active);
    std::vector<double> simulated;
    scheduler.add({.series = 1, .arrival = per_second("periodic", 1.0)},
                  [&](const tick_context& c) {
                      simulated.push_back(c.simulated_seconds);
                      return std::string{};
                  });
    for (int i = 0; i < 4; ++i)
        scheduler.step();

    REQUIRE(simulated.size() == 4);
    for (std::size_t i = 1; i < simulated.size(); ++i)
        CHECK(simulated[i] == Catch::Approx(10.0));
    CHECK(scheduler.snapshot().speed == 10.0);
    CHECK(scheduler.market_speed() == 10.0);
}

TEST_CASE("a configure is in force on return, in the trace, and sets the simulated step",
          "[tick_scheduler]") {
    auto slow = no_heat;
    slow.speed = 10.0;
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, slow, 1);
    scheduler.pin(market_state::active);
    std::vector<double> simulated;
    scheduler.add({.series = 1, .arrival = per_second("periodic", 1.0)},
                  [&](const tick_context& c) {
                      simulated.push_back(c.simulated_seconds);
                      return std::string{};
                  });
    scheduler.step();
    scheduler.step();

    auto fast = slow;
    fast.speed = 20.0;
    fast.wake_rate = 1.0;
    scheduler.configure(fast);
    CHECK(scheduler.market_speed() == 20.0);
    CHECK(scheduler.snapshot().wake_rate == 1.0);

    auto runaway = fast;
    runaway.jump = 3.0;
    CHECK_THROWS_AS(scheduler.configure(runaway), std::invalid_argument);
    CHECK(scheduler.snapshot().jump == 0.0);

    scheduler.step();
    REQUIRE(simulated.size() == 3);
    CHECK(simulated[1] == Catch::Approx(10.0));
    CHECK(simulated[2] == Catch::Approx(20.0));

    const auto page = scheduler.trace(std::nullopt, 1e9);
    const auto configured = std::ranges::count_if(page.events, [](const auto& e) {
        return e.kind == ores::analytics::quant::service::trace_kind::configure;
    });
    CHECK(configured == 1);
}

TEST_CASE("a body is told the tail tilt the market's pace gives", "[tick_scheduler]") {
    auto loaded = no_heat;
    loaded.tail_loading = 2.0;
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, loaded, 1);
    scheduler.pin(market_state::active);
    std::vector<double> tilts;
    scheduler.add({.series = 1, .arrival = per_second("periodic", 1.0)},
                  [&](const tick_context& c) {
                      tilts.push_back(c.tail_tilt);
                      return std::string{};
                  });
    scheduler.step();
    auto off = loaded;
    off.tail_loading = 0.0;
    scheduler.configure(off);
    scheduler.step();

    REQUIRE(tilts.size() == 2);
    // Active at two with no heat: a pace of two.
    CHECK(tilts[0] == Catch::Approx(2.0 * std::log(2.0)));
    CHECK(tilts[1] == 0.0);
    auto negative = loaded;
    negative.tail_loading = -1.0;
    CHECK_THROWS_AS(scheduler.configure(negative), std::invalid_argument);
    auto extreme = loaded;
    extreme.tail_loading = 11.0;
    CHECK_THROWS_AS(scheduler.configure(extreme), std::invalid_argument);
}

TEST_CASE("the history counts each group's ticks by the second and by the tenth",
          "[tick_scheduler]") {
    manual_run_clock clock(scheduler_wall_origin);
    tick_scheduler scheduler(clock, no_heat, 1);
    scheduler.pin(market_state::active);
    scheduler.add({.series = 1, .arrival = per_second("periodic", 1.0), .group = "fx"}, quiet);
    scheduler.add({.series = 2, .arrival = per_second("periodic", 0.5), .group = "rates"}, quiet);
    for (int i = 0; i < 30; ++i)
        scheduler.step();

    const auto by_second = scheduler.history(ores::analytics::quant::service::history_resolution::second, 3600);
    REQUIRE(by_second.groups == std::vector<std::string>{"fx", "rates"});
    CHECK(by_second.slot_seconds == 1.0);
    std::uint32_t fx = 0;
    std::uint32_t rates = 0;
    for (const auto& bin : by_second.bins) {
        fx += bin.counts.empty() ? 0 : bin.counts[0];
        rates += bin.counts.size() < 2 ? 0 : bin.counts[1];
    }
    CHECK(fx + rates == 30);
    // Twice the rate, so about two thirds; each law starts at a random phase.
    CHECK(fx >= 19);
    CHECK(fx <= 21);

    const auto by_tenth = scheduler.history(ores::analytics::quant::service::history_resolution::tenth, 600);
    CHECK(by_tenth.slot_seconds == 0.1);
    std::uint32_t all = 0;
    for (const auto& bin : by_tenth.bins)
        for (const auto count : bin.counts)
            all += count;
    CHECK(all == 30);
    // Active at two, so the thirty ticks span about ten seconds of the run.
    CHECK(std::chrono::duration<double>(by_second.bins.back().at - by_second.bins.front().at).count() ==
          Catch::Approx(9.0).margin(1.0));
}

TEST_CASE("a reading carries the ticks a second every law asks for", "[tick_scheduler]") {
    steady_run_clock clock;
    tick_scheduler scheduler(clock, no_heat, 1);
    scheduler.pin(market_state::active);
    scheduler.add({.series = 1, .arrival = per_second("poisson", 3.0), .group = "fx"}, quiet);
    scheduler.add({.series = 2, .arrival = per_second("poisson", 1.0), .group = "fx"}, quiet);
    std::thread loop([&] { scheduler.run(); });
    std::this_thread::sleep_for(250ms);
    scheduler.stop();
    loop.join();

    const auto readings = scheduler.readings_since(std::chrono::system_clock::time_point{});
    REQUIRE_FALSE(readings.empty());
    // Four a second at a pace of two, with no heat.
    CHECK(readings.back().intensity == Catch::Approx(8.0));
}
