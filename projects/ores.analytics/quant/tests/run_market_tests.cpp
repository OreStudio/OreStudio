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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/service/run_clock.hpp"
#include "ores.analytics.quant/service/run_market.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <stdexcept>
#include <string>
#include <vector>

using namespace ores::analytics::quant::domain;
using namespace ores::analytics::quant::math;
using namespace ores::analytics::quant::service;

namespace {

/// A clock that never moves: one step fires the ticks that are due and nothing else.
class run_market_clock final : public i_run_clock {
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

arrival_spec run_market_arrival() {
    return {.mode = "periodic", .ticks_per_hour = 4.0, .branching_ratio = 0.0, .decay = 1.0};
}

const std::string run_market_surface = "synthetic.fx_vol.eurusd";
const std::uint64_t spot = series_key("synthetic.fx.eurusd");

}

TEST_CASE("a run's market maps a spot's one driver and a surface's seven", "[run_market]") {
    run_market_clock clock;
    run_market run(clock, {}, 42);
    run.add_block(spot, run_market_surface, 5, 0.3, -0.4, 0.9);

    run.build(3600.0, 0.0, 60.0);

    CHECK(run.built());
    CHECK(run.has_block(run_market_surface));
    CHECK_FALSE(run.has_block("another.surface"));
    CHECK(run.driver_count_of(spot) == 1);
    CHECK(run.driver_count_of(series_key(run_market_surface)) == 7);
    // A series the market does not know is driven by nothing.
    CHECK(run.driver_count_of(series_key("nobody")) == 0);
    // The spot is one series and the surface's drivers are seven more.
    CHECK(run.noise().series_id.size() == 8);
}

TEST_CASE("a market that is built refuses another block", "[run_market]") {
    run_market_clock clock;
    run_market run(clock, {}, 42);
    run.add_block(spot, run_market_surface, 5, 0.3, -0.4, 0.9);
    CHECK_THROWS_AS(run.add_block(spot, run_market_surface, 5, 0.3, -0.4, 0.9),
                    std::invalid_argument);

    run.build(3600.0, 0.0, 60.0);
    CHECK_THROWS_AS(run.add_block(spot, "another.surface", 5, 0.3, -0.4, 0.9),
                    std::invalid_argument);
    CHECK_THROWS_AS(run.build(3600.0, 0.0, 60.0), std::invalid_argument);
}

TEST_CASE("a surface's drivers reach its tick as the market's normals", "[run_market]") {
    run_market_clock clock;
    run_market run(clock, {}, 42);
    run.add_block(spot, run_market_surface, 5, 0.3, -0.4, 0.9);
    run.build(3600.0, 0.0, 60.0);

    std::vector<double> surface_drivers;
    std::vector<double> spot_driver;
    std::vector<double> stranger_driver;
    const series_spec surface_spec{.series = series_key(run_market_surface),
                                   .arrival = run_market_arrival(),
                                   .sensitivity = 1.0,
                                   .name = run_market_surface,
                                   .group = "fx",
                                   .driver_count = 7};
    const series_spec spot_spec{.series = spot,
                                .arrival = run_market_arrival(),
                                .sensitivity = 1.0,
                                .name = "spot",
                                .group = "fx",
                                .driver_count = 1};
    const series_spec stranger_spec{.series = series_key("nobody"),
                                    .arrival = run_market_arrival(),
                                    .sensitivity = 1.0,
                                    .name = "nobody",
                                    .group = "fx",
                                    .driver_count = 1};
    run.loop().add(surface_spec, [&surface_drivers](const tick_context& c) {
        surface_drivers.assign(c.drivers.begin(), c.drivers.end());
        return std::string("ok");
    });
    run.loop().add(spot_spec, [&spot_driver](const tick_context& c) {
        spot_driver.assign(c.drivers.begin(), c.drivers.end());
        return std::string("ok");
    });
    run.loop().add(stranger_spec, [&stranger_driver](const tick_context& c) {
        stranger_driver.assign(c.drivers.begin(), c.drivers.end());
        return std::string("ok");
    });

    REQUIRE(run.loop().step());
    REQUIRE(run.loop().step());
    REQUIRE(run.loop().step());

    REQUIRE(surface_drivers.size() == 7);
    REQUIRE(spot_driver.size() == 1);
    // A series the market does not drive reads no driver at all: a zero would be a draw a
    // feed could not tell from a real one.
    CHECK(stranger_driver.empty());

    // A driver is a standard normal, so it is finite and not one repeated value.
    bool any_nonzero = false;
    for (const double z : surface_drivers) {
        CHECK(std::isfinite(z));
        if (z != 0.0)
            any_nonzero = true;
    }
    CHECK(any_nonzero);
    CHECK(std::isfinite(spot_driver[0]));
    CHECK(stranger_driver.empty());
}

TEST_CASE("a run with no surface builds a market with nothing in it", "[run_market]") {
    run_market_clock clock;
    run_market run(clock, {}, 42);
    run.build(3600.0, 0.0, 60.0);
    CHECK(run.noise().series_id.empty());
    CHECK(run.driver_count_of(spot) == 0);
}
