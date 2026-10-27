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
#include "ores.synthetic.api/feeds/fx_mixture_conversion.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <stdexcept>
#include <string>

using Catch::Approx;
using ores::synthetic::feed::convert_fx_mixture;
using ores::synthetic::feed::fx_mixture;
using ores::synthetic::feed::kappa_for_half_life;

TEST_CASE("fx_mixture_conversion scales μ and σ by the price between log-returns and price units",
          "[fx_mixture_conversion]") {
    const fx_mixture geometric{{0.0001, 0.0}, {0.0005, 0.002}, {0.95, 0.05}};

    const auto arithmetic = convert_fx_mixture(geometric, "geometric", "arithmetic", 150.0, 3600);
    REQUIRE(arithmetic.means.size() == 2);
    CHECK(arithmetic.means[0] == Approx(0.015));
    CHECK(arithmetic.stdevs[0] == Approx(0.075));
    CHECK(arithmetic.stdevs[1] == Approx(0.3));
    CHECK(arithmetic.weights == geometric.weights);

    const auto back = convert_fx_mixture(arithmetic, "arithmetic", "geometric", 150.0, 3600);
    CHECK(back.means[0] == Approx(0.0001));
    CHECK(back.stdevs[1] == Approx(0.002));
}

TEST_CASE("fx_mixture_conversion keeps the first component for Ornstein-Uhlenbeck, κ from a 15 "
          "minute half-life",
          "[fx_mixture_conversion]") {
    const fx_mixture geometric{{0.0001, 0.0}, {0.0005, 0.002}, {0.95, 0.05}};

    const auto ou = convert_fx_mixture(geometric, "geometric", "ornstein_uhlenbeck", 2.0, 60);
    REQUIRE(ou.means.size() == 1);
    CHECK(ou.means[0] == 0.0);
    CHECK(ou.stdevs[0] == Approx(0.001));
    CHECK(ou.weights[0] == Approx(std::log(2.0) / 15.0));
    CHECK(kappa_for_half_life(15.0, 60) == Approx(std::log(2.0) / 15.0));
}

TEST_CASE("fx_mixture_conversion gives the one component the whole weight leaving "
          "Ornstein-Uhlenbeck",
          "[fx_mixture_conversion]") {
    const fx_mixture ou{{0.0}, {0.001}, {0.046}};

    const auto geometric = convert_fx_mixture(ou, "ornstein_uhlenbeck", "geometric", 2.0, 60);
    REQUIRE(geometric.means.size() == 1);
    CHECK(geometric.stdevs[0] == Approx(0.0005));
    CHECK(geometric.weights[0] == 1.0);
}

TEST_CASE("fx_mixture_conversion returns a mixture whose engine does not change as it is",
          "[fx_mixture_conversion]") {
    const fx_mixture mixture{{0.1}, {0.2}, {0.3}};
    const auto same = convert_fx_mixture(mixture, "arithmetic", "arithmetic", 2.0, 60);
    CHECK(same.means == mixture.means);
    CHECK(same.stdevs == mixture.stdevs);
    CHECK(same.weights == mixture.weights);
}

TEST_CASE("fx_mixture_conversion refuses what it cannot convert", "[fx_mixture_conversion]") {
    const fx_mixture mixture{{0.1}, {0.2}, {0.3}};
    CHECK_THROWS_AS(convert_fx_mixture(mixture, "geometric", "jump", 2.0, 60),
                    std::invalid_argument);
    CHECK_THROWS_AS(convert_fx_mixture(mixture, "geometric", "arithmetic", 0.0, 60),
                    std::invalid_argument);
    CHECK_THROWS_AS(convert_fx_mixture(mixture, "geometric", "arithmetic", 2.0, 0),
                    std::invalid_argument);
    CHECK_THROWS_AS(
        convert_fx_mixture(fx_mixture{{0.1}, {0.2}, {}}, "geometric", "arithmetic", 2.0, 60),
        std::invalid_argument);
    CHECK_THROWS_AS(kappa_for_half_life(0.0, 60), std::invalid_argument);
    CHECK_THROWS_AS(kappa_for_half_life(15.0, 0), std::invalid_argument);
}
