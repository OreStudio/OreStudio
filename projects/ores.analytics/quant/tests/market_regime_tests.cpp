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

using ores::analytics::quant::domain::market_regime;

TEST_CASE("market_regime starts calm and multiplies nothing", "[market_regime]") {
    market_regime regime;
    CHECK(regime.current() == market_regime::state::calm);
    CHECK(regime.multiplier() == 1.0);
    CHECK_FALSE(regime.is_pinned());
}

TEST_CASE("market_regime pins a state and reports its multiplier", "[market_regime]") {
    market_regime regime(1.0 / 600.0, 1.0 / 300.0, 3.0);

    regime.pin(market_regime::state::active);
    CHECK(regime.is_pinned());
    CHECK(regime.current() == market_regime::state::active);
    CHECK(regime.multiplier() == 3.0);

    // A pin holds however much time passes, which is what a scenario asks for.
    regime.advance(10'000.0);
    CHECK(regime.current() == market_regime::state::active);

    regime.unpin();
    CHECK_FALSE(regime.is_pinned());
}

TEST_CASE("market_regime spends the share of time its rates name", "[market_regime]") {
    // A calm market turns active five times as often as an active one turns
    // calm, so it should be active five sixths of the time.
    constexpr double wake = 1.0 / 60.0;
    constexpr double calm = 1.0 / 300.0;
    market_regime regime(wake, calm, 3.0);
    CHECK(regime.long_run_active_share() == Catch::Approx(5.0 / 6.0));

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
    CHECK_THROWS_AS(market_regime(0.0, 1.0, 3.0), std::invalid_argument);
    CHECK_THROWS_AS(market_regime(1.0, -1.0, 3.0), std::invalid_argument);
    CHECK_THROWS_AS(market_regime(1.0, 1.0, 0.0), std::invalid_argument);

    market_regime regime;
    CHECK_THROWS_AS(regime.advance(-1.0), std::invalid_argument);
}

TEST_CASE("market_regime moves with wall time, not once per caller", "[market_regime]") {
    // Two callers share one market, as two feeds' clocks do. Each calls
    // synchronise() in a tight loop for about a tenth of a second; the market
    // must advance by about that much, not by twice it.
    market_regime regime(1e-6, 1e-6, 3.0);
    const double before = regime.remaining_seconds();

    std::atomic<bool> stop{false};
    auto spin = [&] {
        while (!stop.load())
            regime.synchronise();
    };
    std::thread a(spin);
    std::thread b(spin);
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    stop.store(true);
    a.join();
    b.join();

    const double advanced = before - regime.remaining_seconds();
    CHECK(advanced == Catch::Approx(0.1).margin(0.06));
}
