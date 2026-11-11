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
#include "ores.marketdata.service/app/fx_vol_surface_grid.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using namespace ores::marketdata::service::app;

const std::vector<std::string> blueprint_strikes{"DEL/Spot/Put/25", "ATM", "DEL/Spot/Call/25"};

/// A 1Y, 1W and 3M surface by the three strikes, stored in no order.
std::vector<surface_grid_point> stored_points() {
    std::vector<surface_grid_point> out;
    double v = 0.10;
    for (const std::string expiry : {"1Y", "1W", "3M"})
        for (const std::string strike : {"ATM", "DEL/Spot/Call/25", "DEL/Spot/Put/25"})
            out.push_back({expiry, strike, v += 0.001});
    return out;
}

}

TEST_CASE("a surface reads with its expiries by length and its strikes in the blueprint's order",
          "[app][fx_vol_surface_grid]") {
    const auto grid = surface_grid_of(stored_points(), blueprint_strikes, "", "");
    REQUIRE(grid);
    CHECK(grid->expiries == std::vector<std::string>{"1W", "3M", "1Y"});
    CHECK(grid->strikes == blueprint_strikes);
    REQUIRE(grid->points.size() == 9);
    CHECK(grid->points[0].expiry == "1W");
    CHECK(grid->points[0].strike == "DEL/Spot/Put/25");
    CHECK(grid->points[8].expiry == "1Y");
    CHECK(grid->points[8].strike == "DEL/Spot/Call/25");
}

TEST_CASE("a slice reads one expiry across strikes or one strike across expiries",
          "[app][fx_vol_surface_grid]") {
    const auto expiry = surface_grid_of(stored_points(), blueprint_strikes, "3M", "");
    REQUIRE(expiry);
    CHECK(expiry->expiries == std::vector<std::string>{"3M"});
    CHECK(expiry->points.size() == 3);

    const auto strike = surface_grid_of(stored_points(), blueprint_strikes, "", "ATM");
    REQUIRE(strike);
    CHECK(strike->strikes == std::vector<std::string>{"ATM"});
    CHECK(strike->expiries == std::vector<std::string>{"1W", "3M", "1Y"});
}

TEST_CASE("a slice the surface does not hold is refused, naming it", "[app][fx_vol_surface_grid]") {
    const auto expiry = surface_grid_of(stored_points(), blueprint_strikes, "5Y", "");
    REQUIRE_FALSE(expiry);
    CHECK(expiry.error() == "the surface has no expiry 5Y");
    const auto strike = surface_grid_of(stored_points(), blueprint_strikes, "", "DEL/Spot/Put/10");
    REQUIRE_FALSE(strike);
    CHECK(strike.error() == "the surface has no strike DEL/Spot/Put/10");
}

TEST_CASE("expiries of the same length stay grouped, each with its own points",
          "[app][fx_vol_surface_grid]") {
    std::vector<surface_grid_point> points{{"1Y", "ATM", 0.1}, {"12M", "ATM", 0.2},
                                           {"1Y", "DEL/Spot/Put/25", 0.3}, {"12M", "DEL/Spot/Put/25", 0.4}};
    const auto grid = surface_grid_of(points, blueprint_strikes, "", "");
    REQUIRE(grid);
    CHECK(grid->expiries == std::vector<std::string>{"12M", "1Y"});
    REQUIRE(grid->points.size() == 4);
    CHECK(grid->points[0].expiry == "12M");
    CHECK(grid->points[1].expiry == "12M");
    CHECK(grid->points[2].expiry == "1Y");
    CHECK(grid->points[3].expiry == "1Y");
}
