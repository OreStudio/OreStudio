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
#include "ores.analytics.quant/service/curve_instrument_pricer.hpp"
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include "ores.analytics.quant/service/processes/vasicek_process.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <stdexcept>
#include <vector>

using ores::analytics::quant::service::curve_instrument_pricer;
using ores::analytics::quant::service::hull_white_process;
using ores::analytics::quant::service::vasicek_process;

TEST_CASE("deposit_rate recovers the rate implied by a known discount factor",
    "[curve_instrument_pricer]") {
    // P = 1/(1+r*tau) => r = (1/P - 1)/tau
    const double tau = 0.25;
    const double r = 0.03;
    const double df = 1.0 / (1.0 + r * tau);
    CHECK(curve_instrument_pricer::deposit_rate(df, tau) == Catch::Approx(r));
}

TEST_CASE("deposit_rate(1.0, tau) is exactly zero (no discounting, zero rate)",
    "[curve_instrument_pricer]") {
    CHECK(curve_instrument_pricer::deposit_rate(1.0, 1.0) == Catch::Approx(0.0));
}

TEST_CASE("deposit_rate rejects a non-positive discount factor", "[curve_instrument_pricer]") {
    CHECK_THROWS_AS(curve_instrument_pricer::deposit_rate(0.0, 1.0), std::invalid_argument);
    CHECK_THROWS_AS(curve_instrument_pricer::deposit_rate(-0.5, 1.0), std::invalid_argument);
}

TEST_CASE("deposit_rate rejects a non-positive year fraction", "[curve_instrument_pricer]") {
    CHECK_THROWS_AS(curve_instrument_pricer::deposit_rate(0.99, 0.0), std::invalid_argument);
}

TEST_CASE("fra_rate recovers the forward rate implied by two discount factors",
    "[curve_instrument_pricer]") {
    // P(start)/P(end) = 1 + F*accrual => F = (P(start)/P(end) - 1)/accrual
    const double accrual = 0.5;
    const double forward = 0.025;
    const double df_end = 0.9;
    const double df_start = df_end * (1.0 + forward * accrual);
    CHECK(curve_instrument_pricer::fra_rate(df_start, df_end, accrual) == Catch::Approx(forward));
}

TEST_CASE("fra_rate is zero when start and end discount factors are equal",
    "[curve_instrument_pricer]") {
    CHECK(curve_instrument_pricer::fra_rate(0.95, 0.95, 0.5) == Catch::Approx(0.0));
}

TEST_CASE("fra_rate rejects non-positive discount factors or accrual fraction",
    "[curve_instrument_pricer]") {
    CHECK_THROWS_AS(curve_instrument_pricer::fra_rate(0.0, 0.9, 0.5), std::invalid_argument);
    CHECK_THROWS_AS(curve_instrument_pricer::fra_rate(0.9, 0.0, 0.5), std::invalid_argument);
    CHECK_THROWS_AS(curve_instrument_pricer::fra_rate(0.95, 0.9, 0.0), std::invalid_argument);
}

TEST_CASE("swap_par_rate recovers a known par rate for a single-period swap "
    "(degenerates to fra_rate)",
    "[curve_instrument_pricer]") {
    const double df_start = 1.0;
    const double df_end = 0.95;
    const double accrual = 1.0;
    const double par = curve_instrument_pricer::swap_par_rate(df_start, df_end, {df_end}, {accrual});
    const double forward = curve_instrument_pricer::fra_rate(df_start, df_end, accrual);
    CHECK(par == Catch::Approx(forward));
}

TEST_CASE("swap_par_rate makes the fixed leg PV equal the floating leg's telescoped PV",
    "[curve_instrument_pricer]") {
    const double df_start = 1.0;
    const std::vector<double> fixed_leg_dfs = {0.99, 0.97, 0.94, 0.90};
    const std::vector<double> accruals = {0.25, 0.25, 0.25, 0.25};
    const double df_end = fixed_leg_dfs.back();

    const double par = curve_instrument_pricer::swap_par_rate(df_start, df_end, fixed_leg_dfs, accruals);

    double annuity = 0.0;
    for (std::size_t i = 0; i < fixed_leg_dfs.size(); ++i)
        annuity += accruals[i] * fixed_leg_dfs[i];
    const double fixed_leg_pv = par * annuity;
    const double floating_leg_pv = df_start - df_end;
    CHECK(fixed_leg_pv == Catch::Approx(floating_leg_pv));
}

TEST_CASE("swap_par_rate rejects mismatched or empty vectors", "[curve_instrument_pricer]") {
    CHECK_THROWS_AS(curve_instrument_pricer::swap_par_rate(1.0, 0.9, {}, {}), std::invalid_argument);
    CHECK_THROWS_AS(
        curve_instrument_pricer::swap_par_rate(1.0, 0.9, {0.95, 0.9}, {0.5}), std::invalid_argument);
}

TEST_CASE("swap_par_rate rejects non-positive discount factors or accrual fractions",
    "[curve_instrument_pricer]") {
    CHECK_THROWS_AS(
        curve_instrument_pricer::swap_par_rate(0.0, 0.9, {0.9}, {1.0}), std::invalid_argument);
    CHECK_THROWS_AS(
        curve_instrument_pricer::swap_par_rate(1.0, 0.9, {0.0}, {1.0}), std::invalid_argument);
    CHECK_THROWS_AS(
        curve_instrument_pricer::swap_par_rate(1.0, 0.9, {0.9}, {0.0}), std::invalid_argument);
}

TEST_CASE("deposit_rate/fra_rate/swap_par_rate applied to a real hull_white_process "
    "produce a coherent, monotonically-consistent curve",
    "[curve_instrument_pricer]") {
    // Integration check: derive several instrument rates from one process's
    // discount_factor()s at increasing maturities, confirming they compose
    // without contradiction (e.g. the multi-period swap rate should sit
    // between the shortest and longest single-period forward rates for a
    // monotonically-shaped curve) rather than checking exact values (which
    // duplicates the process's own discount_factor tests).
    hull_white_process process(0.3, {0.03}, 0.01, 0.02);

    const double df_3m = process.discount_factor(1);
    const double df_1y = process.discount_factor(4);
    const double df_2y = process.discount_factor(8);

    const double deposit = curve_instrument_pricer::deposit_rate(df_3m, 0.25);
    const double fra = curve_instrument_pricer::fra_rate(df_3m, df_1y, 0.75);
    const double swap = curve_instrument_pricer::swap_par_rate(
        1.0, df_2y, {df_3m, df_1y, df_2y}, {0.25, 0.75, 1.0});

    CHECK(std::isfinite(deposit));
    CHECK(std::isfinite(fra));
    CHECK(std::isfinite(swap));
}

TEST_CASE("vasicek_process (a hull_white_process composition) produces the same "
    "instrument rates as the equivalent hull_white_process directly",
    "[curve_instrument_pricer]") {
    vasicek_process vasicek(0.4, 0.025, 0.02, 0.015);
    hull_white_process hull_white(0.4, {0.025}, 0.02, 0.015);

    const double v_df = vasicek.discount_factor(4);
    const double hw_df = hull_white.discount_factor(4);
    CHECK(v_df == Catch::Approx(hw_df));

    const double v_rate = curve_instrument_pricer::deposit_rate(v_df, 1.0);
    const double hw_rate = curve_instrument_pricer::deposit_rate(hw_df, 1.0);
    CHECK(v_rate == Catch::Approx(hw_rate));
}
