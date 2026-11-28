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
#include "ores.analytics.quant/math/ir_vol_derivation.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <vector>

using namespace ores::analytics::quant::math;

namespace {

/// A rate series whose changes alternate by one basis point, and its changes.
std::vector<double> alternating_changes(int count) {
    std::vector<double> out;
    out.reserve(static_cast<std::size_t>(count));
    for (int i = 0; i < count; ++i)
        out.push_back(i % 2 == 0 ? 0.0001 : -0.0001);
    return out;
}

short_rate_projection reversion_of(double mean_reversion) {
    return {.model = short_rate_model::hull_white, .mean_reversion = mean_reversion};
}

}

TEST_CASE("a model's name and the model round trip, and a process with no closed form has none",
          "[ir_vol_derivation]") {
    CHECK(name_of(short_rate_model::vasicek) == "vasicek");
    CHECK(name_of(short_rate_model::hull_white) == "hull_white");
    CHECK(short_rate_model_named("vasicek") == short_rate_model::vasicek);
    CHECK(short_rate_model_named("hull_white") == short_rate_model::hull_white);
    CHECK_FALSE(short_rate_model_named("cox_ingersoll_ross"));
    CHECK_FALSE(short_rate_model_named("black_karasinski"));
    CHECK_FALSE(short_rate_model_named("two_factor_gaussian"));
    CHECK_FALSE(short_rate_model_named(""));
}

TEST_CASE("the projection carries a level over the expiry and averages it over the term",
          "[ir_vol_derivation]") {
    // (1 - exp(-2 * 0.05 * 1)) / 0.1, rooted, times (1 - exp(-0.05 * 1)) / 0.05.
    CHECK(*projection_factor({.expiry_years = 1.0, .term_years = 1.0}, reversion_of(0.05)) ==
          Catch::Approx(0.9515267115656557).epsilon(1e-14));
    // A longer swap averages the same rate over more of its life, so it swings less.
    CHECK(*projection_factor({.expiry_years = 1.0, .term_years = 5.0}, reversion_of(0.05)) ==
          Catch::Approx(0.8631309404501314).epsilon(1e-14));
    // A later expiry has more time to accumulate variance than the term can damp.
    CHECK(*projection_factor({.expiry_years = 5.0, .term_years = 10.0}, reversion_of(0.05)) ==
          Catch::Approx(1.5609764152990506).epsilon(1e-14));
}

TEST_CASE("a model with no mean reversion is read through its limit, not refused",
          "[ir_vol_derivation]") {
    CHECK(*projection_factor({.expiry_years = 2.0, .term_years = 3.0}, reversion_of(0.0)) ==
          Catch::Approx(1.4142135623730951).epsilon(1e-15));
    CHECK(*projection_factor({.expiry_years = 2.0, .term_years = 3.0}, reversion_of(1e-12)) ==
          Catch::Approx(std::sqrt(2.0)).epsilon(1e-5));
}

TEST_CASE("a pillar with no length, or a negative mean reversion, is refused",
          "[ir_vol_derivation]") {
    CHECK_FALSE(projection_factor({.expiry_years = 0.0, .term_years = 1.0}, reversion_of(0.05)));
    CHECK_FALSE(projection_factor({.expiry_years = 1.0, .term_years = 0.0}, reversion_of(0.05)));
    CHECK_FALSE(projection_factor({.expiry_years = -1.0, .term_years = 1.0}, reversion_of(0.05)));
    CHECK_FALSE(projection_factor({.expiry_years = 1.0, .term_years = 1.0}, reversion_of(-0.05)));
    CHECK_FALSE(
        projection_factor({.expiry_years = 1.0, .term_years = 1.0}, reversion_of(std::nan(""))));
}

TEST_CASE("a pillar's ATM volatility is its window's level carried by the projection",
          "[ir_vol_derivation]") {
    const auto vol = atm_vol_of(
        0.001590609895806464, 252, {.expiry_years = 1.0, .term_years = 1.0}, reversion_of(0.05));
    REQUIRE(vol);
    CHECK(vol->window == 252);
    CHECK(vol->level == Catch::Approx(0.001590609895806464).epsilon(1e-15));
    CHECK(vol->projection == Catch::Approx(0.9515267115656557).epsilon(1e-14));
    CHECK(vol->atm == Catch::Approx(0.0015135078035405148).epsilon(1e-13));

    CHECK_FALSE(
        atm_vol_of(-0.001, 252, {.expiry_years = 1.0, .term_years = 1.0}, reversion_of(0.05)));
    CHECK_FALSE(
        atm_vol_of(0.001, 252, {.expiry_years = 0.0, .term_years = 1.0}, reversion_of(0.05)));
}

TEST_CASE("the smile is the level at the money, with the skewness as its slope",
          "[ir_vol_derivation]") {
    const normal_skew_smile smile = smile_of(0.0016, {.skewness = -1.0, .excess_kurtosis = 0.5});
    CHECK(smile.alpha == 0.0016);
    CHECK(smile.skewness == -1.0);

    const double forward = 0.04;
    const double root = smile.alpha * std::sqrt(1.0);
    // On the money the standardised strike is zero, so the volatility is the level.
    CHECK(smile_at(smile, forward, forward, 1.0) == Catch::Approx(0.0016).epsilon(1e-15));
    // One standard deviation below the money: a negative skewness raises the volatility there.
    CHECK(smile_at(smile, forward, forward - root, 1.0) ==
          Catch::Approx(0.0016 * (1.0 + 1.0 / 6.0)).epsilon(1e-15));
    // And lowers it above the money.
    CHECK(smile_at(smile, forward, forward + root, 1.0) ==
          Catch::Approx(0.0016 * (1.0 - 1.0 / 6.0)).epsilon(1e-15));
    // The upper wing is where a negative skewness drives the slope to zero, and a wing far
    // enough out never states a negative volatility.
    CHECK(smile_at(smile, forward, forward + 40.0 * root, 1.0) == 0.0);
}

TEST_CASE("a term's matrix reads each expiry over its own window and projects through the model",
          "[ir_vol_derivation]") {
    const auto changes = alternating_changes(400);
    const std::vector<double> expiries{1.0, 0.25, 3.0};
    const auto matrix = derive_term_matrix(changes, 1.0, expiries, {}, reversion_of(0.05));
    REQUIRE(matrix);
    REQUIRE(matrix->cells.size() == 3);
    // The whole sample's shape is read once; the level of each cell is read over its own window.
    CHECK(matrix->series.moments.count == 400);
    CHECK(matrix->cells[0]->window == 252);
    CHECK(matrix->cells[1]->window == 63);
    CHECK(matrix->cells[0]->atm == Catch::Approx(0.0015135078035405148).epsilon(1e-13));
    // A 63-change window of alternating basis points is an odd count, so its mean is not zero:
    // its sample stdev is sqrt(63 * (1e-4)^2 / 62), carried by the projection at a quarter year.
    CHECK(matrix->cells[1]->atm == Catch::Approx(0.0007754774567857565).epsilon(1e-13));
    // Three years of returns from a sample of four hundred: the window is not covered, and only
    // that cell is refused.
    CHECK_FALSE(matrix->cells[2]);
    CHECK(matrix->cells[2].error() == "the sample is shorter than the window");

    CHECK_FALSE(
        derive_term_matrix(changes, 1.0, expiries, {.periods_per_year = 0.0}, reversion_of(0.05)));
    CHECK_FALSE(derive_term_matrix(
        std::vector<double>{0.0001, -0.0001, 0.0002}, 1.0, expiries, {}, reversion_of(0.05)));
}
