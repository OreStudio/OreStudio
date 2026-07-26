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
#include "ores.analytics.quant/service/processes/arithmetic_gaussian_mixture_model_process.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

using ores::analytics::quant::service::arithmetic_gaussian_mixture_model_process;

TEST_CASE("arithmetic_gaussian_mixture_model_process reports the initial price before any next() call",
          "[arithmetic_gaussian_mixture_model_process]") {
    arithmetic_gaussian_mixture_model_process p({0.0}, {0.01}, {1.0}, 100.0);
    CHECK(p.current() == 100.0);
}

TEST_CASE("arithmetic_gaussian_mixture_model_process next() updates and returns the same value as current()",
          "[arithmetic_gaussian_mixture_model_process]") {
    arithmetic_gaussian_mixture_model_process p({0.0}, {0.01}, {1.0}, 100.0);
    const double n = p.next();
    CHECK(n == p.current());
}

TEST_CASE("arithmetic_gaussian_mixture_model_process is deterministic for a fixed seed", "[arithmetic_gaussian_mixture_model_process]") {
    arithmetic_gaussian_mixture_model_process a({0.1, -0.2}, {0.5, 1.0}, {0.5, 0.5}, 100.0, 7);
    arithmetic_gaussian_mixture_model_process b({0.1, -0.2}, {0.5, 1.0}, {0.5, 0.5}, 100.0, 7);

    for (int i = 0; i < 20; ++i)
        CHECK(a.next() == b.next());
}

TEST_CASE("arithmetic_gaussian_mixture_model_process applies increments additively, unlike the geometric engine",
          "[arithmetic_gaussian_mixture_model_process]") {
    // sd == 0 hits the degenerate branch: the draw is exactly the mean.
    arithmetic_gaussian_mixture_model_process p({5.0}, {0.0}, {1.0}, 100.0);
    CHECK(p.next() == Catch::Approx(105.0));
    CHECK(p.next() == Catch::Approx(110.0));
}

TEST_CASE("arithmetic_gaussian_mixture_model_process price may cross zero", "[arithmetic_gaussian_mixture_model_process]") {
    arithmetic_gaussian_mixture_model_process p({-50.0}, {0.0}, {1.0}, 10.0);
    p.next(); // 10 - 50 = -40
    CHECK(p.current() == Catch::Approx(-40.0));
}

TEST_CASE("arithmetic_gaussian_mixture_model_process rejects mismatched parameter vector sizes",
          "[arithmetic_gaussian_mixture_model_process]") {
    CHECK_THROWS_AS(arithmetic_gaussian_mixture_model_process({0.0, 0.0}, {0.01}, {1.0}, 100.0),
                    std::invalid_argument);
    CHECK_THROWS_AS(arithmetic_gaussian_mixture_model_process({0.0}, {0.01, 0.01}, {1.0}, 100.0),
                    std::invalid_argument);
    CHECK_THROWS_AS(arithmetic_gaussian_mixture_model_process({0.0}, {0.01}, {1.0, 0.0}, 100.0),
                    std::invalid_argument);
}

TEST_CASE("arithmetic_gaussian_mixture_model_process rejects an empty component set", "[arithmetic_gaussian_mixture_model_process]") {
    CHECK_THROWS_AS(arithmetic_gaussian_mixture_model_process({}, {}, {}, 100.0), std::invalid_argument);
}

TEST_CASE("arithmetic_gaussian_mixture_model_process rejects a non-positive initial price",
          "[arithmetic_gaussian_mixture_model_process]") {
    CHECK_THROWS_AS(arithmetic_gaussian_mixture_model_process({0.0}, {0.01}, {1.0}, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(arithmetic_gaussian_mixture_model_process({0.0}, {0.01}, {1.0}, -1.0), std::invalid_argument);
}
