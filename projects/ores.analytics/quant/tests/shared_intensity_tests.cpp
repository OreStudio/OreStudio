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
#include "ores.analytics.quant/domain/market_link.hpp"
#include "ores.analytics.quant/domain/shared_intensity.hpp"
#include <algorithm>
#include <array>
#include <atomic>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <random>
#include <stdexcept>
#include <thread>
#include <vector>

using ores::analytics::quant::domain::market_link;
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

TEST_CASE("shared_intensity keeps a reading of what it faded", "[shared_intensity]") {
    shared_intensity heat(0.5, 20.0);
    REQUIRE(heat.reading_count() == 0);

    heat.record_event();
    heat.fade(0.1);

    REQUIRE(heat.reading_count() == 1);
    const auto readings = heat.readings_since(std::chrono::system_clock::time_point{});
    REQUIRE(readings.size() == 1);
    CHECK(readings.front().heat == Catch::Approx(0.5 * std::exp(-2.0)).epsilon(1e-9));
}

TEST_CASE("shared_intensity reads no faster than its period", "[shared_intensity]") {
    shared_intensity heat(0.5, 20.0);

    heat.fade(0.01);
    heat.fade(0.01);

    // Two fades inside one period are one reading: the history is a picture of
    // the heat, not a copy of every fade.
    CHECK(heat.reading_count() == 1);
}

TEST_CASE("shared_intensity reports the readings from an instant on", "[shared_intensity]") {
    shared_intensity heat(0.5, 20.0);
    heat.record_event();
    heat.fade(0.1);

    const auto now = std::chrono::system_clock::now();
    CHECK(heat.readings_since(now).empty());
    CHECK(heat.readings_since(now - std::chrono::minutes(1)).size() == 1);
}

TEST_CASE("shared_intensity refuses parameters that cannot describe heat", "[shared_intensity]") {
    CHECK_THROWS_AS(shared_intensity(-1.0, 20.0), std::invalid_argument);
    CHECK_THROWS_AS(shared_intensity(0.5, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(shared_intensity(0.5, -1.0), std::invalid_argument);

    shared_intensity heat;
    CHECK_THROWS_AS(heat.fade(-1.0), std::invalid_argument);
}

namespace {

/*
 * Two series on one market, on a fine fixed time grid: in each step a series
 * produces an event with the probability its rate allows, and every event feeds
 * the shared heat. Fixed steps rather than event times, because the point here is
 * the coupling, and a grid cannot stall on a degenerate draw.
 */
std::vector<int> run_two_series(double heat_per_event,
                                double decay,
                                bool couple,
                                std::uint32_t seed,
                                double seconds,
                                double window) {
    shared_intensity heat(heat_per_event, decay);
    std::mt19937 rng(seed);
    std::uniform_real_distribution<double> uniform(0.0, 1.0);

    constexpr double mean_rate = 5.0;
    constexpr double step = 0.001;
    const int windows = static_cast<int>(seconds / window);
    std::vector<int> counts(static_cast<std::size_t>(windows) * 2, 0);

    const long steps = static_cast<long>(seconds / step);
    for (long s = 0; s < steps; ++s) {
        const auto w = static_cast<std::size_t>((static_cast<double>(s) * step) / window) * 2;
        for (std::size_t i = 0; i < 2; ++i) {
            const double rate = mean_rate + (couple ? heat.value() : 0.0);
            if (uniform(rng) < rate * step) {
                if (w + i < counts.size())
                    ++counts[w + i];
                if (couple)
                    heat.record_event();
            }
        }
        if (couple)
            heat.fade(step);
    }
    return counts;
}

double correlation(const std::vector<int>& counts) {
    const std::size_t n = counts.size() / 2;
    double mean_a = 0.0;
    double mean_b = 0.0;
    for (std::size_t k = 0; k < n; ++k) {
        mean_a += counts[2 * k];
        mean_b += counts[2 * k + 1];
    }
    mean_a /= static_cast<double>(n);
    mean_b /= static_cast<double>(n);

    double cov = 0.0;
    double var_a = 0.0;
    double var_b = 0.0;
    for (std::size_t k = 0; k < n; ++k) {
        const double a = counts[2 * k] - mean_a;
        const double b = counts[2 * k + 1] - mean_b;
        cov += a * b;
        var_a += a * a;
        var_b += b * b;
    }
    if (var_a <= 0.0 || var_b <= 0.0)
        return 0.0;
    return cov / std::sqrt(var_a * var_b);
}

}

TEST_CASE("two series on one heat move together and separately without it", "[shared_intensity]") {
    // The point of the endogenous factor: counts of two independent series must
    // be positively correlated when they share heat and near zero when they do
    // not. This is the statistic no per-series test can see.
    const auto coupled = run_two_series(2.0, 2.0, true, 7, 5'000.0, 1.0);
    const auto separate = run_two_series(2.0, 2.0, false, 7, 5'000.0, 1.0);

    CHECK(correlation(coupled) > 0.2);
    CHECK(std::abs(correlation(separate)) < 0.1);
}

TEST_CASE("the heat stays bounded however many series share it", "[shared_intensity]") {
    // The synthetic service's own heat: half a rate per event, fading at twenty a
    // second. Sixty series at one tick a second each is more than decay over jump,
    // the count at which every series taking the whole heat makes it run away.
    shared_intensity heat(0.5, 20.0);
    constexpr int series = 60;
    constexpr double mean_rate = 1.0;
    std::vector<shared_intensity::enrolment> enrolled;
    for (int i = 0; i < series; ++i)
        enrolled.push_back(heat.enrol(mean_rate));
    const market_link link{.intensity = &heat};

    const double bound = 2.0 * heat.stationary_mean(series * mean_rate);
    constexpr double step = 0.001;
    double owed = 0.0;
    for (int s = 0; s < 10'000 && heat.value() < bound; ++s) {
        owed += series * mean_rate * link.rate_multiplier() * step;
        for (; owed >= 1.0; owed -= 1.0)
            heat.record_event();
        heat.fade(step);
    }

    CHECK(heat.value() < bound);
}

TEST_CASE("the population rate follows the series that are enrolled", "[shared_intensity]") {
    shared_intensity heat(0.5, 20.0);
    CHECK(heat.rate_share() == 0.0);
    {
        auto one = heat.enrol(2.0);
        auto two = heat.enrol(3.0);
        CHECK(heat.population_rate() == Catch::Approx(5.0));

        auto moved = std::move(one);
        CHECK(heat.population_rate() == Catch::Approx(5.0));

        heat.record_event();
        CHECK(heat.rate_share() == Catch::Approx(0.5 / 5.0));
    }
    CHECK(heat.population_rate() == Catch::Approx(0.0).margin(1e-12));
    CHECK_THROWS_AS(heat.enrol(0.0), std::invalid_argument);
}

TEST_CASE("the population rate returns to zero when every series leaves", "[shared_intensity]") {
    shared_intensity heat(0.5, 20.0);
    heat.record_event();
    std::mt19937 rng(11);
    std::uniform_real_distribution<double> rate(0.0001, 3.0);
    std::vector<shared_intensity::enrolment> enrolled;
    for (int round = 0; round < 50; ++round) {
        for (int i = 0; i < 200; ++i)
            enrolled.push_back(heat.enrol(rate(rng)));
        enrolled.erase(enrolled.begin(), enrolled.begin() + 150);
    }
    enrolled.clear();

    CHECK(heat.population_rate() == 0.0);
    CHECK(heat.rate_share() == 0.0);
}
