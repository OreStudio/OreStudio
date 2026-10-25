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
#include "ores.analytics.quant/domain/shared_intensity.hpp"
#include <atomic>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <thread>
#include <vector>

using ores::analytics::quant::domain::shared_intensity;

TEST_CASE("shared_intensity starts cold", "[shared_intensity]") {
    shared_intensity heat(0.5, 20.0);
    CHECK(heat.value() == 0.0);
    CHECK(heat.excess() == 0.0);
    CHECK(heat.jump() == 0.5);
    CHECK(heat.decay() == 20.0);
}

TEST_CASE("shared_intensity settles at the rate its events arrive", "[shared_intensity]") {
    // Ten events a second, each adding half a rate, fading at twenty a second:
    // the heat settles at 10 * 0.5 / 20 = 0.25.
    shared_intensity heat(0.5, 20.0);
    CHECK(heat.stationary_mean(10.0) == Catch::Approx(0.25));
}

TEST_CASE("shared_intensity raises on an event and fades between them", "[shared_intensity]") {
    shared_intensity heat(0.5, 20.0);
    heat.record_event();
    CHECK(heat.value() == Catch::Approx(0.5));

    heat.record_event();
    CHECK(heat.value() == Catch::Approx(1.0));

    // A tenth of a second at a decay of twenty leaves exp(-2) of the heat.
    heat.fade(0.1);
    CHECK(heat.value() == Catch::Approx(std::exp(-2.0)));

    heat.fade(1.0);
    CHECK(heat.value() < 1e-8);
}

TEST_CASE("shared_intensity keeps every event under concurrency", "[shared_intensity]") {
    // Events arrive from several feeds at once and the owner fades in between; no
    // event may be lost to the fade.
    shared_intensity heat(1.0, 0.0 + 1e-9); // a decay so slow the fade is invisible
    constexpr int threads = 4;
    constexpr int per_thread = 25'000;

    std::atomic<bool> stop{false};
    std::thread owner([&] {
        while (!stop.load())
            heat.fade(0.0);
    });
    std::vector<std::thread> recorders;
    for (int i = 0; i < threads; ++i)
        recorders.emplace_back([&] {
            for (int n = 0; n < per_thread; ++n)
                heat.record_event();
        });
    for (auto& r : recorders)
        r.join();
    stop.store(true);
    owner.join();

    CHECK(heat.excess() == Catch::Approx(static_cast<double>(threads * per_thread)).epsilon(1e-9));
}

TEST_CASE("shared_intensity refuses parameters that cannot describe heat", "[shared_intensity]") {
    CHECK_THROWS_AS(shared_intensity(-1.0, 20.0), std::invalid_argument);
    CHECK_THROWS_AS(shared_intensity(0.5, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(shared_intensity(0.5, -1.0), std::invalid_argument);

    shared_intensity heat;
    CHECK_THROWS_AS(heat.fade(-1.0), std::invalid_argument);
}
