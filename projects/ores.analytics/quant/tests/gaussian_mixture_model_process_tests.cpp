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
#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include "ores.analytics.quant/service/processes/gaussian_mixture_model_process.hpp"
#include "price_stream.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <numeric>
#include <stdexcept>
#include <vector>

using ores::analytics::quant::service::gaussian_mixture_model_process;

TEST_CASE("gaussian_mixture_model_process reports the initial price before any next() call",
          "[gaussian_mixture_model_process]") {
    gaussian_mixture_model_process p({0.0}, {0.01}, {1.0}, 100.0, price_stream(42));
    CHECK(p.current() == 100.0);
}

TEST_CASE("gaussian_mixture_model_process next() updates and returns the same value as current()",
          "[gaussian_mixture_model_process]") {
    gaussian_mixture_model_process p({0.0}, {0.01}, {1.0}, 100.0, price_stream(42));
    const double n = p.next();
    CHECK(n == p.current());
}

TEST_CASE("gaussian_mixture_model_process is deterministic for a fixed seed",
          "[gaussian_mixture_model_process]") {
    gaussian_mixture_model_process a(
        {0.001, -0.002}, {0.01, 0.02}, {0.5, 0.5}, 100.0, price_stream(7));
    gaussian_mixture_model_process b(
        {0.001, -0.002}, {0.01, 0.02}, {0.5, 0.5}, 100.0, price_stream(7));

    for (int i = 0; i < 20; ++i)
        CHECK(a.next() == b.next());
}

TEST_CASE("gaussian_mixture_model_process with different seeds diverges",
          "[gaussian_mixture_model_process]") {
    gaussian_mixture_model_process a({0.001}, {0.05}, {1.0}, 100.0, price_stream(1));
    gaussian_mixture_model_process b({0.001}, {0.05}, {1.0}, 100.0, price_stream(2));

    bool any_different = false;
    for (int i = 0; i < 20; ++i) {
        if (a.next() != b.next())
            any_different = true;
    }
    CHECK(any_different);
}

TEST_CASE("gaussian_mixture_model_process price stays strictly positive (geometric engine)",
          "[gaussian_mixture_model_process]") {
    gaussian_mixture_model_process p({-1.0, 1.0}, {0.5, 0.5}, {0.5, 0.5}, 100.0, price_stream(99));
    for (int i = 0; i < 500; ++i)
        REQUIRE(p.next() > 0.0);
}

TEST_CASE("gaussian_mixture_model_process with a zero-variance component draws exactly the mean",
          "[gaussian_mixture_model_process]") {
    // sd == 0 hits the degenerate branch (avoids libstdc++ aborting on sigma <= 0).
    gaussian_mixture_model_process p({0.1}, {0.0}, {1.0}, 100.0, price_stream(42));
    const double expected = 100.0 * std::exp(0.1);
    CHECK(p.next() == Catch::Approx(expected));
}

TEST_CASE("gaussian_mixture_model_process rejects mismatched parameter vector sizes",
          "[gaussian_mixture_model_process]") {
    CHECK_THROWS_AS(
        gaussian_mixture_model_process({0.0, 0.0}, {0.01}, {1.0}, 100.0, price_stream(42)),
        std::invalid_argument);
    CHECK_THROWS_AS(
        gaussian_mixture_model_process({0.0}, {0.01, 0.01}, {1.0}, 100.0, price_stream(42)),
        std::invalid_argument);
    CHECK_THROWS_AS(
        gaussian_mixture_model_process({0.0}, {0.01}, {1.0, 0.0}, 100.0, price_stream(42)),
        std::invalid_argument);
}

TEST_CASE("gaussian_mixture_model_process rejects an empty component set",
          "[gaussian_mixture_model_process]") {
    CHECK_THROWS_AS(gaussian_mixture_model_process({}, {}, {}, 100.0, price_stream(42)),
                    std::invalid_argument);
}

TEST_CASE("gaussian_mixture_model_process rejects a non-positive initial price",
          "[gaussian_mixture_model_process]") {
    CHECK_THROWS_AS(gaussian_mixture_model_process({0.0}, {0.01}, {1.0}, 0.0, price_stream(42)),
                    std::invalid_argument);
    CHECK_THROWS_AS(gaussian_mixture_model_process({0.0}, {0.01}, {1.0}, -1.0, price_stream(42)),
                    std::invalid_argument);
}

TEST_CASE("gaussian_mixture_model_process: next() is next(1.0)",
          "[gaussian_mixture_model_process]") {
    gaussian_mixture_model_process one_tick(
        {0.0, 0.2}, {0.01, 0.05}, {0.7, 0.3}, 100.0, price_stream(7));
    gaussian_mixture_model_process explicit_tick(
        {0.0, 0.2}, {0.01, 0.05}, {0.7, 0.3}, 100.0, price_stream(7));
    for (int i = 0; i < 20; ++i)
        CHECK(one_tick.next() == explicit_tick.next(1.0));
}

TEST_CASE("gaussian_mixture_model_process: the spread over a span does not depend on the cut",
          "[gaussian_mixture_model_process]") {
    // The same span, cut into many short steps and into few long ones, must give
    // the same variance of the log-return. Both runs cover 400 ticks.
    constexpr int paths = 400;
    auto spread = [](int steps, double each) {
        std::vector<double> returns;
        returns.reserve(paths);
        for (int seed = 0; seed < paths; ++seed) {
            gaussian_mixture_model_process p(
                {0.0, 0.2}, {0.01, 0.05}, {0.7, 0.3}, 100.0, price_stream(seed));
            for (int i = 0; i < steps; ++i)
                p.next(each);
            returns.push_back(std::log(p.current() / 100.0));
        }
        const double mean =
            std::accumulate(returns.begin(), returns.end(), 0.0) / static_cast<double>(paths);
        double sum = 0.0;
        for (const double r : returns)
            sum += (r - mean) * (r - mean);
        return sum / static_cast<double>(paths - 1);
    };

    const double many_short = spread(400, 1.0);
    const double few_long = spread(100, 4.0);
    CHECK(few_long == Catch::Approx(many_short).epsilon(0.15));
}

TEST_CASE("gaussian_mixture_model_process: a non-positive elapsed is refused",
          "[gaussian_mixture_model_process]") {
    gaussian_mixture_model_process p({0.0}, {0.01}, {1.0}, 100.0, price_stream(42));
    CHECK_THROWS_AS(p.next(0.0), std::invalid_argument);
    CHECK_THROWS_AS(p.next(-1.0), std::invalid_argument);
    CHECK(p.supports_variable_step());
}

namespace {

/*
 * A value process with a law for one tick only. The interface's default is what
 * a caller meets when a process cannot answer for a longer step, so this pins
 * the default rather than a process that happens to exist today.
 */
struct one_tick_process final : ores::analytics::quant::domain::i_stochastic_process {
    using ores::analytics::quant::domain::i_stochastic_process::next;

    double value = 100.0;
    double next() override {
        return value;
    }
    double current() const override {
        return value;
    }
};

}

TEST_CASE("a process with no time-scaled law refuses a step other than one tick",
          "[gaussian_mixture_model_process]") {
    one_tick_process p;
    CHECK_FALSE(p.supports_variable_step());
    CHECK(p.next(1.0) == 100.0);
    CHECK_THROWS_AS(p.next(2.0), std::invalid_argument);
    CHECK_THROWS_AS(p.next(0.5), std::invalid_argument);
}
