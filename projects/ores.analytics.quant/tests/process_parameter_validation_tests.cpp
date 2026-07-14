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
#include "ores.analytics.quant/domain/process_parameter_validation.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::analytics::quant::domain::validate_process_parameters;
using ores::analytics::quant::domain::validate_yield_curve_process_parameters;

TEST_CASE("valid geometric parameters pass", "[process_parameter_validation]") {
    const auto r = validate_process_parameters("geometric", {0.0}, {0.01}, {1.0}, 100.0);
    CHECK(r.valid);
    CHECK(r.message.empty());
}

TEST_CASE("valid arithmetic parameters pass", "[process_parameter_validation]") {
    const auto r =
        validate_process_parameters("arithmetic", {0.1, -0.2}, {0.5, 1.0}, {0.5, 0.5}, 100.0);
    CHECK(r.valid);
}

TEST_CASE("valid ou parameters pass", "[process_parameter_validation]") {
    const auto r = validate_process_parameters("ou", {}, {1.0}, {0.3}, 100.0);
    CHECK(r.valid);
}

TEST_CASE("unrecognised process_type falls back to the mixing rule",
          "[process_parameter_validation]") {
    const auto r = validate_process_parameters("bogus", {0.0}, {0.01}, {1.0}, 100.0);
    CHECK(r.valid);
}

TEST_CASE("non-positive initial price is rejected for every engine",
          "[process_parameter_validation]") {
    CHECK_FALSE(validate_process_parameters("geometric", {0.0}, {0.01}, {1.0}, 0.0).valid);
    CHECK_FALSE(validate_process_parameters("arithmetic", {0.0}, {0.01}, {1.0}, -1.0).valid);
    CHECK_FALSE(validate_process_parameters("ou", {}, {1.0}, {0.3}, 0.0).valid);
}

TEST_CASE("mixing engines reject empty component vectors", "[process_parameter_validation]") {
    const auto r = validate_process_parameters("geometric", {}, {}, {}, 100.0);
    CHECK_FALSE(r.valid);
    CHECK_FALSE(r.message.empty());
}

TEST_CASE("mixing engines reject mismatched component counts", "[process_parameter_validation]") {
    CHECK_FALSE(validate_process_parameters("geometric", {0.0, 0.0}, {0.01}, {1.0}, 100.0).valid);
    CHECK_FALSE(validate_process_parameters("arithmetic", {0.0}, {0.01, 0.02}, {1.0}, 100.0).valid);
    CHECK_FALSE(validate_process_parameters("geometric", {0.0}, {0.01}, {1.0, 0.0}, 100.0).valid);
}

TEST_CASE("mixing engines reject a negative stdev", "[process_parameter_validation]") {
    const auto r = validate_process_parameters("geometric", {0.0}, {-0.01}, {1.0}, 100.0);
    CHECK_FALSE(r.valid);
}

TEST_CASE("mixing engines reject all-zero weights", "[process_parameter_validation]") {
    const auto r =
        validate_process_parameters("geometric", {0.0, 0.0}, {0.01, 0.01}, {0.0, 0.0}, 100.0);
    CHECK_FALSE(r.valid);
}

TEST_CASE("mixing engines accept a single negative weight as long as the sum is positive",
          "[process_parameter_validation]") {
    // Only the *sum* is checked (matches std::discrete_distribution's own
    // requirement) — an individual negative weight is not itself an error.
    const auto r =
        validate_process_parameters("geometric", {0.0, 0.0}, {0.01, 0.01}, {-0.5, 2.0}, 100.0);
    CHECK(r.valid);
}

TEST_CASE("ou rejects missing kappa/sigma channels", "[process_parameter_validation]") {
    CHECK_FALSE(validate_process_parameters("ou", {}, {}, {0.3}, 100.0).valid);
    CHECK_FALSE(validate_process_parameters("ou", {}, {1.0}, {}, 100.0).valid);
}

TEST_CASE("ou rejects a negative sigma", "[process_parameter_validation]") {
    const auto r = validate_process_parameters("ou", {}, {-1.0}, {0.3}, 100.0);
    CHECK_FALSE(r.valid);
}

TEST_CASE("ou accepts a non-positive kappa (degenerate driftless walk is valid)",
          "[process_parameter_validation]") {
    CHECK(validate_process_parameters("ou", {}, {1.0}, {0.0}, 100.0).valid);
    CHECK(validate_process_parameters("ou", {}, {1.0}, {-0.5}, 100.0).valid);
}

TEST_CASE("valid vasicek/hull_white parameters pass", "[process_parameter_validation]") {
    CHECK(validate_yield_curve_process_parameters("vasicek", 0.3, {0.03}, 0.01, 0.03).valid);
    CHECK(validate_yield_curve_process_parameters("hull_white", 0.3, {0.03, 0.04, 0.05}, 0.01, 0.03)
              .valid);
}

TEST_CASE("vasicek/hull_white accept a non-positive kappa (degenerate driftless case)",
          "[process_parameter_validation]") {
    CHECK(validate_yield_curve_process_parameters("vasicek", 0.0, {0.03}, 0.01, 0.03).valid);
    CHECK(validate_yield_curve_process_parameters("hull_white", -0.1, {0.03}, 0.01, 0.03).valid);
}

TEST_CASE("vasicek/hull_white accept a negative initial or mean-reversion rate",
          "[process_parameter_validation]") {
    // Gaussian models: a negative short rate is unusual but not invalid.
    CHECK(validate_yield_curve_process_parameters("vasicek", 0.3, {-0.01}, 0.01, -0.005).valid);
}

TEST_CASE("yield curve engines reject an empty theta_path", "[process_parameter_validation]") {
    CHECK_FALSE(validate_yield_curve_process_parameters("vasicek", 0.3, {}, 0.01, 0.03).valid);
    CHECK_FALSE(validate_yield_curve_process_parameters("hull_white", 0.3, {}, 0.01, 0.03).valid);
    CHECK_FALSE(validate_yield_curve_process_parameters("cir", 0.3, {}, 0.01, 0.03).valid);
}

TEST_CASE("yield curve engines reject a negative sigma", "[process_parameter_validation]") {
    CHECK_FALSE(validate_yield_curve_process_parameters("vasicek", 0.3, {0.03}, -0.01, 0.03).valid);
    CHECK_FALSE(validate_yield_curve_process_parameters("cir", 0.3, {0.03}, -0.01, 0.03).valid);
}

TEST_CASE("cir requires strictly positive kappa and theta", "[process_parameter_validation]") {
    CHECK_FALSE(validate_yield_curve_process_parameters("cir", 0.0, {0.03}, 0.01, 0.03).valid);
    CHECK_FALSE(validate_yield_curve_process_parameters("cir", -0.1, {0.03}, 0.01, 0.03).valid);
    CHECK_FALSE(validate_yield_curve_process_parameters("cir", 0.3, {0.0}, 0.01, 0.03).valid);
    CHECK_FALSE(validate_yield_curve_process_parameters("cir", 0.3, {-0.01}, 0.01, 0.03).valid);
}

TEST_CASE("cir rejects a negative initial rate", "[process_parameter_validation]") {
    CHECK_FALSE(validate_yield_curve_process_parameters("cir", 0.3, {0.03}, 0.01, -0.001).valid);
}

TEST_CASE("cir accepts a zero initial rate", "[process_parameter_validation]") {
    CHECK(validate_yield_curve_process_parameters("cir", 0.3, {0.03}, 0.01, 0.0).valid);
}
