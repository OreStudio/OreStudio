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
#include "ores.analytics.quant/math/nelder_mead.hpp"
#include <array>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>

using ores::analytics::quant::math::nelder_mead;
using ores::analytics::quant::math::nelder_mead_options;

TEST_CASE("the simplex finds the minimum of a quadratic bowl", "[nelder_mead]") {
    const auto bowl = [](const std::array<double, 3>& x) {
        return (x[0] - 1.0) * (x[0] - 1.0) + 2.0 * (x[1] + 0.5) * (x[1] + 0.5) +
               3.0 * (x[2] - 2.0) * (x[2] - 2.0) + 4.0;
    };
    const auto r = nelder_mead<3>(bowl, {0.0, 0.0, 0.0}, {0.5, 0.5, 0.5});
    CHECK(r.converged);
    CHECK(r.value == Catch::Approx(4.0).epsilon(1e-10));
    CHECK(r.x[0] == Catch::Approx(1.0).margin(1e-4));
    CHECK(r.x[1] == Catch::Approx(-0.5).margin(1e-4));
    CHECK(r.x[2] == Catch::Approx(2.0).margin(1e-4));
}

TEST_CASE("the simplex follows Rosenbrock's valley to its minimum", "[nelder_mead]") {
    const auto rosenbrock = [](const std::array<double, 2>& x) {
        return 100.0 * (x[1] - x[0] * x[0]) * (x[1] - x[0] * x[0]) + (1.0 - x[0]) * (1.0 - x[0]);
    };
    const auto r = nelder_mead<2>(rosenbrock, {-1.2, 1.0}, {0.1, 0.1});
    CHECK(r.converged);
    CHECK(r.x[0] == Catch::Approx(1.0).margin(1e-3));
    CHECK(r.x[1] == Catch::Approx(1.0).margin(2e-3));
}

TEST_CASE("the simplex leaves a region of large values for an admissible minimum",
          "[nelder_mead]") {
    // The unconstrained minimum is at -1, outside the admissible half-line x >= 0.
    const auto bounded = [](const std::array<double, 1>& x) {
        return x[0] < 0.0 ? 1e10 : (x[0] + 1.0) * (x[0] + 1.0);
    };
    const auto r = nelder_mead<1>(bounded, {2.0}, {0.5});
    CHECK(r.x[0] >= 0.0);
    CHECK(r.x[0] == Catch::Approx(0.0).margin(1e-4));
}

TEST_CASE("a search that runs out of iterations says it did not converge", "[nelder_mead]") {
    const auto rosenbrock = [](const std::array<double, 2>& x) {
        return 100.0 * (x[1] - x[0] * x[0]) * (x[1] - x[0] * x[0]) + (1.0 - x[0]) * (1.0 - x[0]);
    };
    const auto r = nelder_mead<2>(
        rosenbrock, {-1.2, 1.0}, {0.1, 0.1}, nelder_mead_options{.max_iterations = 5});
    CHECK_FALSE(r.converged);
    CHECK(r.iterations == 5);
}

TEST_CASE("a search is a pure function of its inputs", "[nelder_mead]") {
    const auto f = [](const std::array<double, 2>& x) {
        return std::cos(x[0]) * std::sin(x[1]) + 0.1 * (x[0] * x[0] + x[1] * x[1]);
    };
    const auto a = nelder_mead<2>(f, {0.3, -0.7}, {0.2, 0.2});
    const auto b = nelder_mead<2>(f, {0.3, -0.7}, {0.2, 0.2});
    CHECK(a.x == b.x);
    CHECK(a.value == b.value);
    CHECK(a.iterations == b.iterations);
}
