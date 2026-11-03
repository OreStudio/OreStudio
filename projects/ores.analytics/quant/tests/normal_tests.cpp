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
#include "ores.analytics.quant/math/brent.hpp"
#include "ores.analytics.quant/math/normal.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <limits>

using namespace ores::analytics::quant::math;

TEST_CASE("the normal quantile gives the textbook critical values", "[normal]") {
    CHECK(normal_quantile(0.5) == 0.0);
    CHECK(normal_quantile(0.975) == Catch::Approx(1.959963984540054).epsilon(1e-15));
    CHECK(normal_quantile(0.75) == Catch::Approx(0.6744897501960817).epsilon(1e-15));
    CHECK(normal_quantile(0.9) == Catch::Approx(1.2815515655446004).epsilon(1e-15));
    CHECK(normal_quantile(0.025) == Catch::Approx(-1.959963984540054).epsilon(1e-15));
    CHECK(normal_quantile(1e-10) == Catch::Approx(-6.361340902404056).epsilon(1e-14));
}

TEST_CASE("the normal quantile inverts the distribution function across all three regions",
          "[normal]") {
    // A relative error e in x is about x^2 e in p, so the tail is held to that.
    for (double p = 1e-300; p < 0.5; p *= 7.3) {
        const double x = normal_quantile(p);
        INFO("p = " << p);
        CHECK(std::abs(normal_cdf(x) / p - 1.0) < 1e-15 * (1.0 + x * x));
    }
    for (int i = 1; i < 1000; ++i) {
        const double p = i / 1000.0;
        INFO("p = " << p);
        CHECK(normal_cdf(normal_quantile(p)) == Catch::Approx(p).epsilon(1e-14));
        CHECK(normal_quantile(1.0 - p) == Catch::Approx(-normal_quantile(p)).margin(1e-13));
    }
}

TEST_CASE("the normal quantile is infinite at the ends and undefined outside them", "[normal]") {
    CHECK(normal_quantile(0.0) == -std::numeric_limits<double>::infinity());
    CHECK(normal_quantile(1.0) == std::numeric_limits<double>::infinity());
    CHECK(std::isnan(normal_quantile(-0.1)));
    CHECK(std::isnan(normal_quantile(1.1)));
    CHECK(std::isnan(normal_quantile(std::numeric_limits<double>::quiet_NaN())));
}

TEST_CASE("the normal density integrates to the distribution function", "[normal]") {
    constexpr int steps = 113'000;
    constexpr double h = 11.3 / steps;
    double total = 0.0;
    for (int i = 0; i < steps; ++i) {
        const double x = -10.0 + i * h;
        total += h * 0.5 * (normal_pdf(x) + normal_pdf(x + h));
    }
    CHECK(total == Catch::Approx(normal_cdf(1.3)).epsilon(1e-6));
    CHECK(normal_pdf(0.0) == Catch::Approx(0.3989422804014327).epsilon(1e-15));
}

TEST_CASE("Brent finds a root to the tolerance", "[brent]") {
    const auto r = brent_root([](double x) { return x * x * x - 2.0 * x - 5.0; }, 2.0, 3.0);
    REQUIRE(r);
    CHECK(*r == Catch::Approx(2.0945514815423265).epsilon(1e-14));
    const auto c = brent_root([](double x) { return std::cos(x) - x; }, 0.0, 1.0);
    REQUIRE(c);
    CHECK(*c == Catch::Approx(0.7390851332151607).epsilon(1e-14));
}

TEST_CASE("Brent returns an end that is already a root", "[brent]") {
    CHECK(*brent_root([](double x) { return x - 1.0; }, 1.0, 3.0) == 1.0);
    CHECK(*brent_root([](double x) { return x - 3.0; }, 1.0, 3.0) == 3.0);
}

TEST_CASE("Brent refuses a bracket without a sign change, and a search out of iterations",
          "[brent]") {
    CHECK_FALSE(brent_root([](double x) { return x * x + 1.0; }, -1.0, 1.0));
    CHECK_FALSE(brent_root([](double x) { return std::exp(x) - 1e6; },
                           -100.0,
                           100.0,
                           brent_options{.tolerance = 1e-15, .max_iterations = 2}));
}

TEST_CASE("Brent finds a root of a function that is flat near it", "[brent]") {
    const auto r = brent_root([](double x) { return std::pow(x - 0.3, 5); }, -1.0, 1.0);
    REQUIRE(r);
    CHECK(*r == Catch::Approx(0.3).margin(1e-2));
}
