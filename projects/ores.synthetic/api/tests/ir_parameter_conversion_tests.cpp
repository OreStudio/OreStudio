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
#include "ores.synthetic.api/feeds/ir_parameter_conversion.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <stdexcept>
#include <string>

using Catch::Approx;
using ores::synthetic::feed::ir_parameters_from_simple;
using ores::synthetic::feed::ir_simple_from_parameters;
using ores::synthetic::feed::ir_simple_view;

namespace {

const ir_simple_view sample{
    .start_rate = 0.03, .long_run_rate = 0.04, .half_life_years = 2.0, .yearly_move_bp = 100.0};

}

TEST_CASE("ir_parameter_conversion turns a half-life into κ and a move into σ for Vasicek",
          "[ir_parameter_conversion]") {
    const auto p = ir_parameters_from_simple("VASICEK", sample);
    CHECK(p.at("kappa") == Approx(std::log(2.0) / 2.0));
    CHECK(p.at("theta") == Approx(0.04));
    CHECK(p.at("sigma") == Approx(0.01));
    CHECK(p.at("initial_rate") == Approx(0.03));
}

TEST_CASE("ir_parameter_conversion scales σ by the square root of the level for CIR",
          "[ir_parameter_conversion]") {
    const auto p = ir_parameters_from_simple("COX_INGERSOLL_ROSS", sample);
    CHECK(p.at("sigma") == Approx(0.01 / std::sqrt(0.04)));
}

TEST_CASE("ir_parameter_conversion writes Black-Karasinski on the log rate",
          "[ir_parameter_conversion]") {
    const auto p = ir_parameters_from_simple("BLACK_KARASINSKI", sample);
    CHECK(p.at("theta") == Approx(std::log(0.04)));
    CHECK(p.at("sigma") == Approx(0.01 / 0.04));
}

TEST_CASE("ir_parameter_conversion gives Two-Factor Gaussian a derived second factor",
          "[ir_parameter_conversion]") {
    const auto p = ir_parameters_from_simple("TWO_FACTOR_GAUSSIAN", sample);
    CHECK(p.size() == 7);
    CHECK(p.at("kappa_y") == Approx(0.6 * p.at("kappa_x")));
    CHECK(p.at("sigma_y") == Approx(0.5 * p.at("sigma_x")));
    CHECK(p.at("rho") == 0.0);
    const double total =
        std::sqrt(p.at("sigma_x") * p.at("sigma_x") + p.at("sigma_y") * p.at("sigma_y"));
    CHECK(total == Approx(0.01));
}

TEST_CASE("ir_parameter_conversion reads every engine's parameters back to the same view",
          "[ir_parameter_conversion]") {
    for (const auto* engine : {"VASICEK",
                               "COX_INGERSOLL_ROSS",
                               "HULL_WHITE",
                               "BLACK_KARASINSKI",
                               "TWO_FACTOR_GAUSSIAN"}) {
        CAPTURE(engine);
        const auto reading =
            ir_simple_from_parameters(engine, ir_parameters_from_simple(engine, sample));
        REQUIRE(reading.view.has_value());
        CHECK(reading.exact);
        CHECK(reading.view->start_rate == Approx(sample.start_rate));
        CHECK(reading.view->long_run_rate == Approx(sample.long_run_rate));
        CHECK(reading.view->half_life_years == Approx(sample.half_life_years));
        CHECK(reading.view->yearly_move_bp == Approx(sample.yearly_move_bp));
    }
}

TEST_CASE("ir_parameter_conversion warns when a CIR rate can reach zero",
          "[ir_parameter_conversion]") {
    auto p = ir_parameters_from_simple("COX_INGERSOLL_ROSS", sample);
    CHECK(ir_simple_from_parameters("COX_INGERSOLL_ROSS", p).caution.empty());
    p["sigma"] = 0.5;
    CHECK(ir_simple_from_parameters("COX_INGERSOLL_ROSS", p).caution == "touches_zero");
    CHECK(ir_simple_from_parameters("VASICEK", ir_parameters_from_simple("VASICEK", sample))
              .caution.empty());
}

TEST_CASE("ir_parameter_conversion accepts the process type in either case",
          "[ir_parameter_conversion]") {
    CHECK(ir_parameters_from_simple("vasicek", sample).size() == 4);
}

TEST_CASE("ir_parameter_conversion has no simple view without a reversion",
          "[ir_parameter_conversion]") {
    auto p = ir_parameters_from_simple("VASICEK", sample);
    p["kappa"] = 0.0;
    const auto reading = ir_simple_from_parameters("VASICEK", p);
    CHECK_FALSE(reading.view.has_value());
    CHECK_FALSE(reading.reason.empty());
}

TEST_CASE("ir_parameter_conversion has no simple view for a hand-set second factor",
          "[ir_parameter_conversion]") {
    auto p = ir_parameters_from_simple("TWO_FACTOR_GAUSSIAN", sample);
    p["rho"] = 0.4;
    const auto reading = ir_simple_from_parameters("TWO_FACTOR_GAUSSIAN", p);
    CHECK(reading.view.has_value());
    CHECK_FALSE(reading.exact);
    CHECK_FALSE(reading.reason.empty());
}

TEST_CASE("ir_parameter_conversion refuses what no engine can read", "[ir_parameter_conversion]") {
    CHECK_THROWS_AS(ir_parameters_from_simple("NOPE", sample), std::invalid_argument);
    auto zero_life = sample;
    zero_life.half_life_years = 0.0;
    CHECK_THROWS_AS(ir_parameters_from_simple("VASICEK", zero_life), std::invalid_argument);
    auto no_level = sample;
    no_level.long_run_rate = 0.0;
    CHECK_THROWS_AS(ir_parameters_from_simple("COX_INGERSOLL_ROSS", no_level),
                    std::invalid_argument);
    CHECK_NOTHROW(ir_parameters_from_simple("VASICEK", no_level));
    CHECK_THROWS_AS(ir_simple_from_parameters("VASICEK", {{"kappa", 0.5}}), std::invalid_argument);
}
