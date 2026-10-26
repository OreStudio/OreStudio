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
#include "ores.analytics.quant/domain/market_regime.hpp"
#include <atomic>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <thread>
#include <vector>

using ores::analytics::quant::domain::market_regime;

TEST_CASE("market_regime starts calm, quiet and unbiased", "[market_regime]") {
    // The calm multiplier follows from the other three, so the mean of the
    // factor is one: an active share of a third and an active multiplier of two
    // leave a calm multiplier of a half.
    market_regime regime;
    CHECK(regime.current() == market_regime::state::calm);
    CHECK(regime.multiplier() == Catch::Approx(0.5));
    CHECK_FALSE(regime.is_pinned());
    CHECK(regime.long_run_mean_multiplier() == Catch::Approx(1.0));
}

TEST_CASE("market_regime pins a state and reports its multiplier", "[market_regime]") {
    market_regime regime(1.0 / 600.0, 1.0 / 300.0, 2.0);

    regime.pin(market_regime::state::active);
    CHECK(regime.is_pinned());
    CHECK(regime.current() == market_regime::state::active);
    CHECK(regime.multiplier() == Catch::Approx(2.0));

    // A pin holds however much time passes, which is what a scenario asks for.
    regime.advance(10'000.0);
    CHECK(regime.current() == market_regime::state::active);

    regime.unpin();
    CHECK_FALSE(regime.is_pinned());
}

TEST_CASE("market_regime spends the share of time its rates name", "[market_regime]") {
    // A calm market turns active five times as often as an active one turns
    // calm, so it should be active five sixths of the time. An active multiplier
    // of 1.1 keeps the calm multiplier positive at this share.
    constexpr double wake = 1.0 / 60.0;
    constexpr double calm = 1.0 / 300.0;
    market_regime regime(wake, calm, 1.1);
    CHECK(regime.long_run_active_share() == Catch::Approx(5.0 / 6.0));
    CHECK(regime.long_run_mean_multiplier() == Catch::Approx(1.0));

    double active_seconds = 0.0;
    constexpr double slice = 1.0;
    constexpr int slices = 200'000;
    for (int i = 0; i < slices; ++i) {
        if (regime.current() == market_regime::state::active)
            active_seconds += slice;
        regime.advance(slice);
    }

    CHECK(active_seconds / (slice * static_cast<double>(slices)) ==
          Catch::Approx(regime.long_run_active_share()).epsilon(0.05));
}

TEST_CASE("market_regime refuses parameters that cannot describe a market", "[market_regime]") {
    CHECK_THROWS_AS(market_regime(0.0, 1.0, 2.0), std::invalid_argument);
    CHECK_THROWS_AS(market_regime(1.0, -1.0, 2.0), std::invalid_argument);
    CHECK_THROWS_AS(market_regime(1.0, 1.0, 0.0), std::invalid_argument);

    // An active multiplier at or above the reciprocal of the active share leaves
    // no positive calm multiplier, so it is refused rather than silently skewing
    // every mean rate.
    CHECK_THROWS_AS(market_regime(1.0 / 60.0, 1.0 / 300.0, 6.1), std::invalid_argument);

    market_regime regime;
    CHECK_THROWS_AS(regime.advance(-1.0), std::invalid_argument);
}

TEST_CASE("market_regime is read without a lock or a shared write", "[market_regime]") {
    // The tick path is one atomic load: tens of thousands of feeds read this.
    static_assert(std::atomic<double>::is_always_lock_free);
    static_assert(std::atomic<int>::is_always_lock_free);

    market_regime regime;
    regime.pin(market_regime::state::active);
    CHECK(regime.multiplier() == Catch::Approx(2.0));
    regime.pin(market_regime::state::calm);
    CHECK(regime.multiplier() == Catch::Approx(0.5));
}

TEST_CASE("market_regime advances with wall time under one owner", "[market_regime]") {
    // One owner advances the chain while many readers read it, which is the shape
    // of a run: thousands of feeds and one market thread. The rates are so slow
    // that the chain stays in one state, so a torn read would be visible as a
    // multiplier that is neither of the two published values.
    market_regime regime(1e-6, 1e-6, 1.5);

    std::atomic<bool> stop{false};
    std::atomic<int> bad_reads{0};

    std::thread owner([&] {
        while (!stop.load())
            regime.advance_to_now();
    });
    std::vector<std::thread> readers;
    for (int i = 0; i < 4; ++i) {
        readers.emplace_back([&] {
            while (!stop.load()) {
                const double m = regime.multiplier();
                if (m != 0.5 && m != 1.5)
                    bad_reads.fetch_add(1);
            }
        });
    }

    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    stop.store(true);
    owner.join();
    for (auto& r : readers)
        r.join();

    CHECK(bad_reads.load() == 0);
}
