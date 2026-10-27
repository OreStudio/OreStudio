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
#include "ores.analytics.quant/domain/operational_clock.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <cmath>
#include <cstdint>
#include <limits>
#include <stdexcept>

using ores::analytics::quant::domain::market_config;
using ores::analytics::quant::domain::market_state;
using ores::analytics::quant::domain::operational_clock;
using ores::analytics::quant::math::draw_purpose;
using ores::analytics::quant::math::rng_stream;
using ores::analytics::quant::math::series_key;
using ores::analytics::quant::math::stream_key;

namespace {

/// A busy chain, active two fifths of the time, with heat that settles.
const market_config busy{
    .wake_rate = 2.0, .calm_rate = 3.0, .active_multiplier = 2.0, .jump = 0.5, .decay = 2.0};

rng_stream market_stream(std::uint64_t seed) {
    return rng_stream(stream_key{
        .run_seed = seed, .series = series_key("market"), .purpose = draw_purpose::market});
}

}

TEST_CASE("an operational clock refuses heat that would not settle, naming its gain",
          "[operational_clock]") {
    auto runaway = busy;
    runaway.jump = 2.5;
    CHECK_THROWS_WITH(operational_clock(runaway, market_stream(1)),
                      Catch::Matchers::ContainsSubstring("is 1.25"));
}

TEST_CASE("an operational clock's calm and active paces average to one", "[operational_clock]") {
    const operational_clock clock(busy, market_stream(1));
    // Active two fifths of the time at two, so calm is a third.
    CHECK(clock.multiplier(market_state::active) == 2.0);
    CHECK(clock.multiplier(market_state::calm) == Catch::Approx(1.0 / 3.0));
    CHECK(clock.loop_gain() == 0.25);
}

TEST_CASE("an operational clock advances tau by the closed form", "[operational_clock]") {
    operational_clock clock(busy, market_stream(1));
    clock.pin(market_state::active);
    clock.enrol(10.0);
    for (int i = 0; i < 20; ++i)
        clock.record_event();

    // Heat ten over a population of ten adds one to the pace of two.
    CHECK(clock.pace() == Catch::Approx(3.0));

    clock.advance(1.0);
    CHECK(clock.tau() == Catch::Approx(2.0 + (1.0 - std::exp(-2.0)) / 2.0).epsilon(1e-14));
    CHECK(clock.heat() == Catch::Approx(10.0 * std::exp(-2.0)).epsilon(1e-14));
    CHECK(clock.now() == 1.0);
}

TEST_CASE("time_to_reach and advance are inverses across transitions and decay",
          "[operational_clock]") {
    operational_clock clock(busy, market_stream(7));
    clock.enrol(4.0);

    int transitions = 0;
    for (int i = 0; i < 400; ++i) {
        // Events every few steps keep the heat moving, so most spans decay.
        if (i % 3 == 0)
            for (int e = 0; e < 5; ++e)
                clock.record_event();

        const auto before = clock.state();
        const double target = clock.tau() + 0.05 + 0.01 * (i % 50);
        const double wait = clock.time_to_reach(target);
        clock.advance(wait);

        CHECK(clock.tau() == Catch::Approx(target).epsilon(1e-12));
        if (clock.state() != before)
            ++transitions;
    }
    CHECK(transitions > 50);
}

TEST_CASE("a pin, an enrolment and a jump change only how fast tau moves from then",
          "[operational_clock]") {
    operational_clock clock(busy, market_stream(3));
    clock.enrol(5.0);
    clock.advance(0.5);
    constexpr double h = 1e-7;

    const auto check_slope = [&] {
        const double tau = clock.tau();
        const double pace = clock.pace();
        clock.advance(h);
        CHECK((clock.tau() - tau) / h == Catch::Approx(pace).epsilon(1e-5));
    };

    const double tau = clock.tau();
    clock.pin(market_state::active);
    CHECK(clock.tau() == tau);
    CHECK(clock.pace() == Catch::Approx(2.0));
    check_slope();

    const double after_pin = clock.tau();
    clock.record_event();
    CHECK(clock.tau() == after_pin);
    CHECK(clock.pace() == Catch::Approx(2.0 + 0.5 / 5.0).epsilon(1e-6));
    check_slope();

    const double after_jump = clock.tau();
    clock.enrol(5.0);
    CHECK(clock.tau() == after_jump);
    CHECK(clock.pace() == Catch::Approx(2.0 + clock.heat() / 10.0));
    check_slope();
}

TEST_CASE("a pinned chain holds its place and resumes from it", "[operational_clock]") {
    operational_clock clock(busy, market_stream(5));
    const auto state = clock.state();
    const double left = *clock.next_transition_in();

    clock.pin(market_state::active);
    clock.advance(100.0);
    CHECK_FALSE(clock.next_transition_in().has_value());

    clock.unpin();
    CHECK(clock.state() == state);
    CHECK(*clock.next_transition_in() == left);
}

TEST_CASE("the chain draws from its keyed stream", "[operational_clock]") {
    operational_clock one(busy, market_stream(11));
    operational_clock same(busy, market_stream(11));
    operational_clock other(busy, market_stream(12));

    bool differs = false;
    for (int i = 0; i < 20; ++i) {
        CHECK(*one.next_transition_in() == *same.next_transition_in());
        differs = differs || *one.next_transition_in() != *other.next_transition_in();
        one.advance(*one.next_transition_in());
        same.advance(*same.next_transition_in());
        other.advance(*other.next_transition_in());
    }
    CHECK(differs);
}

TEST_CASE("the chain spends its share of time active and keeps the mean pace at one",
          "[operational_clock]") {
    // About 48,000 dwells; the active share and the mean pace settle within a
    // percent or two.
    operational_clock clock(busy, market_stream(13));
    double active = 0.0;
    while (clock.now() < 20000.0) {
        const double dwell = *clock.next_transition_in();
        if (clock.state() == market_state::active)
            active += dwell;
        clock.advance(dwell);
    }
    CHECK(active / clock.now() == Catch::Approx(0.4).epsilon(0.02));
    CHECK(clock.tau() / clock.now() == Catch::Approx(1.0).epsilon(0.02));
}

TEST_CASE("time_to_reach answers a pinned clock and a passed target", "[operational_clock]") {
    operational_clock clock(busy, market_stream(17));
    clock.pin(market_state::calm);
    clock.advance(1.0);

    CHECK(clock.time_to_reach(clock.tau()) == 0.0);
    CHECK(clock.time_to_reach(clock.tau() - 1.0) == 0.0);

    // Calm with no heat moves tau at a third, so a third more takes a second.
    CHECK(clock.time_to_reach(clock.tau() + 1.0 / 3.0) == Catch::Approx(1.0));
    CHECK_THROWS_AS(clock.time_to_reach(std::numeric_limits<double>::quiet_NaN()),
                    std::invalid_argument);
}

TEST_CASE("withdraw refuses a rate that was never enrolled", "[operational_clock]") {
    operational_clock clock(busy, market_stream(19));
    CHECK_THROWS_AS(clock.withdraw(1.0), std::logic_error);
    clock.enrol(2.0);
    CHECK_THROWS_AS(clock.withdraw(3.0), std::invalid_argument);
    CHECK_THROWS_AS(clock.withdraw(-1.0), std::invalid_argument);
    clock.withdraw(2.0);
    CHECK(clock.population_rate() == 0.0);
}
