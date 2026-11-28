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
#include <string>
#include <vector>

using namespace ores::analytics::quant::math;

namespace {

using std::chrono::January;
using std::chrono::sys_days;
using std::chrono::year_month_day;

year_month_day rate_observation_day(int day) {
    return year_month_day{sys_days{std::chrono::year{2026} / January / day}};
}

/// A rate at four percent whose change alternates by one basis point, as dated levels.
std::vector<dated_level> alternating_rate(int count) {
    std::vector<dated_level> out;
    out.reserve(static_cast<std::size_t>(count));
    double rate = 0.04;
    for (int i = 0; i < count; ++i) {
        if (i > 0)
            rate += i % 2 == 0 ? 0.0001 : -0.0001;
        out.push_back({.date = rate_observation_day(i + 1), .level = rate});
    }
    return out;
}

short_rate_projection reversion_of(double mean_reversion) {
    return {.model = short_rate_model::hull_white, .mean_reversion = mean_reversion};
}

return_shape unit_shape() {
    return {.skewness = -1.0, .excess_kurtosis = 0.5};
}

}

TEST_CASE("a model's name round trips, and the models without a closed form say so",
          "[ir_vol_derivation]") {
    CHECK(name_of(short_rate_model::vasicek) == "vasicek");
    CHECK(name_of(short_rate_model::hull_white) == "hull_white");
    CHECK(short_rate_model_named("vasicek") == short_rate_model::vasicek);
    CHECK(short_rate_model_named("hull_white") == short_rate_model::hull_white);
    CHECK(short_rate_model_named("cox_ingersoll_ross") == short_rate_model::cox_ingersoll_ross);
    CHECK_FALSE(short_rate_model_named("black"));
    CHECK_FALSE(short_rate_model_named(""));

    CHECK(has_closed_form(short_rate_model::vasicek));
    CHECK(has_closed_form(short_rate_model::hull_white));
    CHECK_FALSE(has_closed_form(short_rate_model::cox_ingersoll_ross));
    CHECK_FALSE(has_closed_form(short_rate_model::black_karasinski));
    CHECK_FALSE(has_closed_form(short_rate_model::two_factor_gaussian));
}

TEST_CASE("a model the projection is not closed form for is refused by name, not approximated",
          "[ir_vol_derivation]") {
    const swaption_pillar pillar{.expiry_years = 1.0, .term_years = 1.0};
    const auto refused = projection_factor(
        pillar, {.model = short_rate_model::cox_ingersoll_ross, .mean_reversion = 0.05});
    REQUIRE_FALSE(refused);
    CHECK(refused.error().find("cox_ingersoll_ross") != std::string::npos);
    CHECK_FALSE(projection_factor(
        pillar, {.model = short_rate_model::black_karasinski, .mean_reversion = 0.05}));
    CHECK_FALSE(projection_factor(
        pillar, {.model = short_rate_model::two_factor_gaussian, .mean_reversion = 0.05}));
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
    const auto vol = atm_vol_of(0.001590609895806464,
                                252,
                                {.expiry_years = 1.0, .term_years = 1.0},
                                reversion_of(0.05),
                                unit_shape());
    REQUIRE(vol);
    CHECK(vol->window == 252);
    CHECK(vol->level == Catch::Approx(0.001590609895806464).epsilon(1e-15));
    CHECK(vol->factor == Catch::Approx(0.9515267115656557).epsilon(1e-14));
    CHECK(vol->atm == Catch::Approx(0.0015135078035405148).epsilon(1e-13));
    // The smile states the level at the money, and the shape it was handed.
    CHECK(vol->smile.alpha == vol->atm);
    CHECK(vol->smile.skewness == -1.0);

    CHECK_FALSE(atm_vol_of(
        -0.001, 252, {.expiry_years = 1.0, .term_years = 1.0}, reversion_of(0.05), unit_shape()));
    CHECK_FALSE(atm_vol_of(
        0.001, 252, {.expiry_years = 0.0, .term_years = 1.0}, reversion_of(0.05), unit_shape()));
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

TEST_CASE("a term's matrix applies the difference, reads each expiry over its own window and "
          "projects through the model",
          "[ir_vol_derivation]") {
    const auto levels = alternating_rate(400);
    const std::vector<double> expiries{1.0, 0.25, 3.0};
    const auto matrix = derive_term_matrix(levels, 1.0, expiries, {}, reversion_of(0.05));
    REQUIRE(matrix);
    REQUIRE(matrix->cells.size() == 3);
    // Four hundred levels are three hundred and ninety-nine changes; the shape is read once over
    // all of them, and each cell's level over its own window.
    CHECK(matrix->series.moments.count == 399);
    CHECK(matrix->cells[0]->window == 252);
    CHECK(matrix->cells[1]->window == 63);
    CHECK(matrix->cells[0]->atm == Catch::Approx(0.0015135078035405582).epsilon(1e-13));
    CHECK(matrix->cells[1]->atm == Catch::Approx(0.0007754774567857786).epsilon(1e-13));
    // Each cell's smile is the shape of a return over that cell's own life: the daily skewness
    // falls as one over the root of the periods the expiry holds.
    CHECK(matrix->cells[1]->smile.skewness ==
          Catch::Approx(matrix->series.shape.skewness / std::sqrt(63.0)).epsilon(1e-12));
    // Three years of returns from four hundred levels: the window is not covered, and only that
    // cell is refused, in the words that name what it needs.
    REQUIRE_FALSE(matrix->cells[2]);
    CHECK(matrix->cells[2].error() ==
          "the pillar of 3 years needs 756 returns; the sample holds 399");

    CHECK_FALSE(
        derive_term_matrix(levels, 1.0, expiries, {.periods_per_year = 0.0}, reversion_of(0.05)));
    CHECK_FALSE(derive_term_matrix({}, 1.0, expiries, {}, reversion_of(0.05)));
}
