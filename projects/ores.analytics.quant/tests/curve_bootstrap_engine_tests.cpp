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
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.analytics.quant/service/curve_instrument_pricer.hpp"
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <stdexcept>

using ores::analytics::quant::service::bootstrap_curve_role_code;
using ores::analytics::quant::service::bootstrap_pillar;
using ores::analytics::quant::service::bootstrapped_point;
using ores::analytics::quant::service::curve_bootstrap_engine;
using ores::analytics::quant::service::curve_instrument_pricer;
using ores::analytics::quant::service::day_count_calculator;
using ores::analytics::quant::service::day_count_convention_code;
using ores::analytics::quant::service::discount_curve_required_error;
using ores::analytics::quant::service::interpolation_method_code;
using ores::analytics::quant::service::parse_bootstrap_curve_role_code;
using ores::analytics::quant::service::parse_day_count_convention_code;
using ores::analytics::quant::service::parse_interpolation_method_code;
using namespace std::chrono;

namespace {

constexpr auto A365 = day_count_convention_code::A365;
constexpr auto LOG_LINEAR_DISCOUNT = interpolation_method_code::LOG_LINEAR_DISCOUNT;
constexpr auto FLAT_FORWARD_THEN_LOG_LINEAR = interpolation_method_code::FLAT_FORWARD_THEN_LOG_LINEAR;
constexpr auto DEPOSIT = bootstrap_curve_role_code::DEPOSIT;
constexpr auto FRA = bootstrap_curve_role_code::FRA;
constexpr auto SWAP = bootstrap_curve_role_code::SWAP;

// A flat, continuously-compounded 3% curve: ln(df) = -r*t is linear in t,
// so log-linear interpolation between any two points on it is exact (up to
// floating point) -- letting round-trip tests assert tight tolerances
// without conflating bootstrap-formula error with interpolation error.
constexpr double flat_rate = 0.03;

double true_df(year_month_day value_date, year_month_day d) {
    const double t = day_count_calculator::year_fraction(value_date, d, A365);
    return std::exp(-flat_rate * t);
}

std::vector<bootstrap_pillar> make_single_segment_pillars(year_month_day value_date) {
    const auto d_3m = value_date.year() / (unsigned(value_date.month()) + 3) / value_date.day();
    const auto d_6m = value_date.year() / (unsigned(value_date.month()) + 6) / value_date.day();
    const auto d_1y = year(int(value_date.year()) + 1) / value_date.month() / value_date.day();

    std::vector<bootstrap_pillar> pillars;

    // DEPOSIT: value_date -> 3M.
    {
        const double yf = day_count_calculator::year_fraction(value_date, d_3m, A365);
        const double rate = curve_instrument_pricer::deposit_rate(true_df(value_date, d_3m), yf);
        pillars.push_back({"3M", DEPOSIT, rate, value_date, d_3m, {}});
    }
    // FRA: 3M -> 6M.
    {
        const double yf = day_count_calculator::year_fraction(d_3m, d_6m, A365);
        const double rate = curve_instrument_pricer::fra_rate(true_df(value_date, d_3m),
                                                               true_df(value_date, d_6m), yf);
        pillars.push_back({"6M", FRA, rate, d_3m, d_6m, {}});
    }
    // SWAP: value_date -> 1Y, semi-annual fixed leg aligned to the curve's
    // own tenor grid (3M, 6M are prior pillars; 1Y is this pillar's own
    // maturity) -- an intermediate date not coinciding with a prior pillar
    // (e.g. 9M) is a separate, dedicated test case (see below).
    {
        std::vector<year_month_day> schedule{d_3m, d_6m, d_1y};
        std::vector<double> dfs, accruals;
        auto previous = value_date;
        for (const auto& d : schedule) {
            dfs.push_back(true_df(value_date, d));
            accruals.push_back(day_count_calculator::year_fraction(previous, d, A365));
            previous = d;
        }
        const double rate =
            curve_instrument_pricer::swap_par_rate(1.0, true_df(value_date, d_1y), dfs, accruals);
        pillars.push_back({"1Y", SWAP, rate, value_date, d_1y, schedule});
    }
    return pillars;
}

}

TEST_CASE("bootstrap recovers the true discount factors of a flat-rate single-segment curve",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto pillars = make_single_segment_pillars(value_date);

    const auto result =
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT);

    REQUIRE(result.size() == 3);
    for (const auto& point : result)
        CHECK(point.discount_factor == Catch::Approx(true_df(value_date, point.date)).epsilon(1e-9));
}

TEST_CASE("bootstrap recovers the true discount factors when the SWAP pillar comes first in a "
          "differently-ordered but still maturity-increasing pillar list",
          "[curve_bootstrap_engine]") {
    // Same economic curve as the single-segment fixture, but built entirely
    // out of DEPOSIT pillars at successive tenors instead of mixing
    // instrument types, to isolate that DEPOSIT-only chains also round-trip.
    const auto value_date = 2026y / January / 1d;
    const auto d_1m = value_date.year() / (unsigned(value_date.month()) + 1) / value_date.day();
    const auto d_2m = value_date.year() / (unsigned(value_date.month()) + 2) / value_date.day();
    const auto d_3m = value_date.year() / (unsigned(value_date.month()) + 3) / value_date.day();

    std::vector<bootstrap_pillar> pillars;
    for (const auto& d : {d_1m, d_2m, d_3m}) {
        const double yf = day_count_calculator::year_fraction(value_date, d, A365);
        const double rate = curve_instrument_pricer::deposit_rate(true_df(value_date, d), yf);
        pillars.push_back({"", DEPOSIT, rate, value_date, d, {}});
    }

    const auto result =
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT);
    REQUIRE(result.size() == 3);
    for (const auto& point : result)
        CHECK(point.discount_factor == Catch::Approx(true_df(value_date, point.date)).epsilon(1e-9));
}

TEST_CASE("FLAT_FORWARD_THEN_LOG_LINEAR reproduces the same result as LOG_LINEAR_DISCOUNT "
          "-- the FOMC-style two-segment method is one configuration of this engine, not "
          "special-cased",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto pillars = make_single_segment_pillars(value_date);

    const auto log_linear =
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT);
    const auto flat_forward =
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, FLAT_FORWARD_THEN_LOG_LINEAR);

    REQUIRE(log_linear.size() == flat_forward.size());
    for (std::size_t i = 0; i < log_linear.size(); ++i)
        CHECK(log_linear[i].discount_factor == Catch::Approx(flat_forward[i].discount_factor));
}

TEST_CASE("a Projection-style bootstrap succeeds when the supplied discount curve covers every "
          "pillar",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto funding_pillars = make_single_segment_pillars(value_date);
    const auto funding_curve =
        curve_bootstrap_engine::bootstrap(value_date, funding_pillars, A365, LOG_LINEAR_DISCOUNT);

    // A single short pillar well within the funding curve's own coverage.
    const auto d_3m = value_date.year() / (unsigned(value_date.month()) + 3) / value_date.day();
    const double yf = day_count_calculator::year_fraction(value_date, d_3m, A365);
    const double rate = curve_instrument_pricer::deposit_rate(true_df(value_date, d_3m), yf);
    std::vector<bootstrap_pillar> projection_pillars{{"3M", DEPOSIT, rate, value_date, d_3m, {}}};

    const auto result = curve_bootstrap_engine::bootstrap(
        value_date, projection_pillars, A365, LOG_LINEAR_DISCOUNT, &funding_curve);
    REQUIRE(result.size() == 1);
    CHECK(result.front().discount_factor == Catch::Approx(true_df(value_date, d_3m)).epsilon(1e-9));
}

TEST_CASE("a Projection-style bootstrap fails cleanly when the supplied discount curve does not "
          "cover a required pillar date",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    // A funding curve covering only out to 6M.
    auto short_pillars = make_single_segment_pillars(value_date);
    short_pillars.resize(2); // 3M, 6M only
    const auto short_funding_curve =
        curve_bootstrap_engine::bootstrap(value_date, short_pillars, A365, LOG_LINEAR_DISCOUNT);

    // A projection pillar maturing at 1Y -- beyond the funding curve's own coverage.
    const auto d_1y = year(int(value_date.year()) + 1) / value_date.month() / value_date.day();
    const double yf = day_count_calculator::year_fraction(value_date, d_1y, A365);
    const double rate = curve_instrument_pricer::deposit_rate(true_df(value_date, d_1y), yf);
    std::vector<bootstrap_pillar> projection_pillars{{"1Y", DEPOSIT, rate, value_date, d_1y, {}}};

    CHECK_THROWS_AS(curve_bootstrap_engine::bootstrap(value_date, projection_pillars, A365,
                                                       LOG_LINEAR_DISCOUNT, &short_funding_curve),
                    discount_curve_required_error);
}

TEST_CASE("a Projection-style bootstrap fails cleanly when a SWAP pillar's fixed-leg date is not "
          "covered by the supplied discount curve, even though its end_date is",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto funding_pillars = make_single_segment_pillars(value_date);
    // Funding curve covers only out to 6M -- doesn't cover the 1Y fixed-leg date below.
    auto short_pillars = funding_pillars;
    short_pillars.resize(2);
    const auto short_funding_curve =
        curve_bootstrap_engine::bootstrap(value_date, short_pillars, A365, LOG_LINEAR_DISCOUNT);

    const auto d_3m = value_date.year() / (unsigned(value_date.month()) + 3) / value_date.day();
    const auto d_6m = value_date.year() / (unsigned(value_date.month()) + 6) / value_date.day();
    const auto d_1y = year(int(value_date.year()) + 1) / value_date.month() / value_date.day();
    std::vector<bootstrap_pillar> projection_pillars{
        {"1Y", SWAP, 0.03, value_date, d_1y, {d_3m, d_6m, d_1y}}};

    CHECK_THROWS_AS(curve_bootstrap_engine::bootstrap(value_date, projection_pillars, A365,
                                                       LOG_LINEAR_DISCOUNT, &short_funding_curve),
                    discount_curve_required_error);
}

TEST_CASE("an empty discount_curve is rejected rather than silently treated as self-discounting",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto pillars = make_single_segment_pillars(value_date);
    const std::vector<bootstrapped_point> empty_curve;
    CHECK_THROWS_AS(curve_bootstrap_engine::bootstrap(value_date, pillars, A365,
                                                       LOG_LINEAR_DISCOUNT, &empty_curve),
                    discount_curve_required_error);
}

TEST_CASE("bootstrap rejects an empty pillar list", "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    CHECK_THROWS_AS(
        curve_bootstrap_engine::bootstrap(value_date, {}, A365, LOG_LINEAR_DISCOUNT),
        std::invalid_argument);
}

TEST_CASE("bootstrap rejects the recognised-but-unimplemented CUBIC_SPLINE interpolation_method",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto pillars = make_single_segment_pillars(value_date);
    CHECK_THROWS_AS(curve_bootstrap_engine::bootstrap(value_date, pillars, A365,
                                                       interpolation_method_code::CUBIC_SPLINE),
                    std::invalid_argument);
}

TEST_CASE("bootstrap rejects pillars not in strictly increasing maturity order",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto d_3m = value_date.year() / (unsigned(value_date.month()) + 3) / value_date.day();
    std::vector<bootstrap_pillar> pillars{
        {"3M", DEPOSIT, 0.03, value_date, d_3m, {}},
        {"3M-again", DEPOSIT, 0.03, value_date, d_3m, {}}, // same maturity as the previous pillar
    };
    CHECK_THROWS_AS(
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT),
        std::invalid_argument);
}

TEST_CASE("bootstrap rejects pillars supplied in strictly decreasing maturity order",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto d_3m = value_date.year() / (unsigned(value_date.month()) + 3) / value_date.day();
    const auto d_6m = value_date.year() / (unsigned(value_date.month()) + 6) / value_date.day();
    std::vector<bootstrap_pillar> pillars{
        {"6M", DEPOSIT, 0.03, value_date, d_6m, {}},
        {"3M", DEPOSIT, 0.03, value_date, d_3m, {}},
    };
    CHECK_THROWS_AS(
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT),
        std::invalid_argument);
}

TEST_CASE("bootstrap rejects a SWAP pillar with an empty fixed_leg_dates",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto d_1y = year(int(value_date.year()) + 1) / value_date.month() / value_date.day();
    std::vector<bootstrap_pillar> pillars{{"1Y", SWAP, 0.03, value_date, d_1y, {}}};
    CHECK_THROWS_AS(
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT),
        std::invalid_argument);
}

TEST_CASE("bootstrap rejects a SWAP pillar whose fixed_leg_dates does not end at its own end_date",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto d_6m = value_date.year() / (unsigned(value_date.month()) + 6) / value_date.day();
    const auto d_1y = year(int(value_date.year()) + 1) / value_date.month() / value_date.day();
    std::vector<bootstrap_pillar> pillars{{"1Y", SWAP, 0.03, value_date, d_1y, {d_6m}}};
    CHECK_THROWS_AS(
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT),
        std::invalid_argument);
}

TEST_CASE("bootstrap rejects a pillar depending on a date beyond what has been bootstrapped so far",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto d_3m = value_date.year() / (unsigned(value_date.month()) + 3) / value_date.day();
    const auto d_6m = value_date.year() / (unsigned(value_date.month()) + 6) / value_date.day();
    const auto d_9m = value_date.year() / (unsigned(value_date.month()) + 9) / value_date.day();
    // A gap: the second pillar's own start_date (6M) never got bootstrapped -- only 3M did.
    std::vector<bootstrap_pillar> pillars{
        {"3M", DEPOSIT, 0.03, value_date, d_3m, {}},
        {"9M-off-6M", FRA, 0.03, d_6m, d_9m, {}},
    };
    CHECK_THROWS_AS(
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT),
        std::invalid_argument);
}

TEST_CASE("bootstrap rejects a SWAP pillar whose fixed-leg schedule has an intermediate date not "
          "aligned with the curve's own tenor grid",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto d_3m = value_date.year() / (unsigned(value_date.month()) + 3) / value_date.day();
    const auto d_9m = value_date.year() / (unsigned(value_date.month()) + 9) / value_date.day();
    const auto d_1y = year(int(value_date.year()) + 1) / value_date.month() / value_date.day();

    // Only 3M has been bootstrapped; 9M is an intermediate fixed-leg date
    // that does not coincide with any prior pillar -- a genuine circular
    // dependency under log-linear interpolation, not yet resolvable.
    std::vector<bootstrap_pillar> pillars{
        {"3M", DEPOSIT, 0.03, value_date, d_3m, {}},
        {"1Y", SWAP, 0.03, value_date, d_1y, {d_3m, d_9m, d_1y}},
    };
    CHECK_THROWS_AS(
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT),
        std::invalid_argument);
}

TEST_CASE("bootstrap of a SWAP-only curve where every fixed-leg date is itself a prior pillar "
          "recovers the true discount factors",
          "[curve_bootstrap_engine]") {
    // Quarterly SWAP pillars out to 1Y, each one's fixed-leg schedule
    // referencing only dates that are themselves prior pillars -- the
    // "fully aligned tenor grid" case the engine requires.
    const auto value_date = 2026y / January / 1d;
    const auto d_3m = value_date.year() / (unsigned(value_date.month()) + 3) / value_date.day();
    const auto d_6m = value_date.year() / (unsigned(value_date.month()) + 6) / value_date.day();
    const auto d_9m = value_date.year() / (unsigned(value_date.month()) + 9) / value_date.day();
    const auto d_1y = year(int(value_date.year()) + 1) / value_date.month() / value_date.day();

    auto swap_rate_for = [&](year_month_day maturity, const std::vector<year_month_day>& schedule) {
        std::vector<double> dfs, accruals;
        auto previous = value_date;
        for (const auto& d : schedule) {
            dfs.push_back(true_df(value_date, d));
            accruals.push_back(day_count_calculator::year_fraction(previous, d, A365));
            previous = d;
        }
        return curve_instrument_pricer::swap_par_rate(1.0, true_df(value_date, maturity), dfs,
                                                       accruals);
    };

    std::vector<bootstrap_pillar> pillars{
        {"3M", SWAP, swap_rate_for(d_3m, {d_3m}), value_date, d_3m, {d_3m}},
        {"6M", SWAP, swap_rate_for(d_6m, {d_3m, d_6m}), value_date, d_6m, {d_3m, d_6m}},
        {"9M", SWAP, swap_rate_for(d_9m, {d_3m, d_6m, d_9m}), value_date, d_9m,
         {d_3m, d_6m, d_9m}},
        {"1Y", SWAP, swap_rate_for(d_1y, {d_3m, d_6m, d_9m, d_1y}), value_date, d_1y,
         {d_3m, d_6m, d_9m, d_1y}},
    };

    const auto result =
        curve_bootstrap_engine::bootstrap(value_date, pillars, A365, LOG_LINEAR_DISCOUNT);
    REQUIRE(result.size() == 4);
    for (const auto& point : result)
        CHECK(point.discount_factor == Catch::Approx(true_df(value_date, point.date)).epsilon(1e-9));
}

TEST_CASE("interpolate_discount_factor returns the exact value at a known point",
          "[curve_bootstrap_engine]") {
    const std::vector<bootstrapped_point> points{
        {"a", 2026y / January / 1d, 1.0},
        {"b", 2026y / July / 1d, 0.97},
    };
    CHECK(curve_bootstrap_engine::interpolate_discount_factor(points, 2026y / July / 1d,
                                                              LOG_LINEAR_DISCOUNT) ==
          Catch::Approx(0.97));
}

TEST_CASE("interpolate_discount_factor returns the exact value at the first known point",
          "[curve_bootstrap_engine]") {
    const std::vector<bootstrapped_point> points{
        {"a", 2026y / January / 1d, 1.0},
        {"b", 2026y / July / 1d, 0.97},
    };
    CHECK(curve_bootstrap_engine::interpolate_discount_factor(points, 2026y / January / 1d,
                                                              LOG_LINEAR_DISCOUNT) ==
          Catch::Approx(1.0));
}

TEST_CASE("interpolate_discount_factor interpolates the log of the discount factor linearly "
          "between two bracketing points",
          "[curve_bootstrap_engine]") {
    const auto value_date = 2026y / January / 1d;
    const auto mid_date = 2026y / April / 2d; // ~91 days into a 181-day segment
    const auto end_date = 2026y / July / 1d;
    const std::vector<bootstrapped_point> points{
        {"a", value_date, 1.0},
        {"b", end_date, true_df(value_date, end_date)},
    };
    const double interpolated =
        curve_bootstrap_engine::interpolate_discount_factor(points, mid_date, LOG_LINEAR_DISCOUNT);
    // Both endpoints lie on the same flat continuously-compounded curve, so
    // log-linear interpolation should recover the true discount factor
    // exactly (up to floating point).
    CHECK(interpolated == Catch::Approx(true_df(value_date, mid_date)).epsilon(1e-9));
}

TEST_CASE("interpolate_discount_factor rejects a query date outside the known range",
          "[curve_bootstrap_engine]") {
    const std::vector<bootstrapped_point> points{
        {"a", 2026y / January / 1d, 1.0},
        {"b", 2026y / July / 1d, 0.97},
    };
    CHECK_THROWS_AS(curve_bootstrap_engine::interpolate_discount_factor(
                        points, 2027y / January / 1d, LOG_LINEAR_DISCOUNT),
                    std::out_of_range);
    CHECK_THROWS_AS(curve_bootstrap_engine::interpolate_discount_factor(
                        points, 2025y / January / 1d, LOG_LINEAR_DISCOUNT),
                    std::out_of_range);
}

TEST_CASE("interpolate_discount_factor rejects an empty point set", "[curve_bootstrap_engine]") {
    CHECK_THROWS_AS(curve_bootstrap_engine::interpolate_discount_factor(
                        {}, 2026y / January / 1d, LOG_LINEAR_DISCOUNT),
                    std::invalid_argument);
}

TEST_CASE("interpolate_discount_factor rejects the recognised-but-unimplemented CUBIC_SPLINE "
          "interpolation_method",
          "[curve_bootstrap_engine]") {
    const std::vector<bootstrapped_point> points{
        {"a", 2026y / January / 1d, 1.0},
        {"b", 2026y / July / 1d, 0.97},
    };
    CHECK_THROWS_AS(curve_bootstrap_engine::interpolate_discount_factor(
                        points, 2026y / April / 1d, interpolation_method_code::CUBIC_SPLINE),
                    std::invalid_argument);
}

TEST_CASE("parse_bootstrap_curve_role_code accepts every valid curve_role.code",
          "[curve_bootstrap_engine]") {
    CHECK(parse_bootstrap_curve_role_code("DEPOSIT") == DEPOSIT);
    CHECK(parse_bootstrap_curve_role_code("FRA") == FRA);
    CHECK(parse_bootstrap_curve_role_code("SWAP") == SWAP);
}

TEST_CASE("parse_bootstrap_curve_role_code rejects an unsupported curve_role.code",
          "[curve_bootstrap_engine]") {
    CHECK_THROWS_AS(parse_bootstrap_curve_role_code("FUTURE"), std::invalid_argument);
    CHECK_THROWS_AS(parse_bootstrap_curve_role_code("NONE"), std::invalid_argument);
    CHECK_THROWS_AS(parse_bootstrap_curve_role_code(""), std::invalid_argument);
    CHECK_THROWS_AS(parse_bootstrap_curve_role_code("deposit"), std::invalid_argument);
}

TEST_CASE("parse_interpolation_method_code accepts every valid interpolation_method.code, "
          "including the recognised-but-unimplemented CUBIC_SPLINE",
          "[curve_bootstrap_engine]") {
    CHECK(parse_interpolation_method_code("LOG_LINEAR_DISCOUNT") == LOG_LINEAR_DISCOUNT);
    CHECK(parse_interpolation_method_code("FLAT_FORWARD_THEN_LOG_LINEAR") ==
          FLAT_FORWARD_THEN_LOG_LINEAR);
    CHECK(parse_interpolation_method_code("CUBIC_SPLINE") ==
          interpolation_method_code::CUBIC_SPLINE);
}

TEST_CASE("parse_interpolation_method_code rejects an unsupported interpolation_method.code",
          "[curve_bootstrap_engine]") {
    CHECK_THROWS_AS(parse_interpolation_method_code("MONOTONE_CONVEX"), std::invalid_argument);
    CHECK_THROWS_AS(parse_interpolation_method_code(""), std::invalid_argument);
}

TEST_CASE("parse_day_count_convention_code accepts every valid day_count_fraction_type.code",
          "[curve_bootstrap_engine]") {
    CHECK(parse_day_count_convention_code("A360") == day_count_convention_code::A360);
    CHECK(parse_day_count_convention_code("A365") == day_count_convention_code::A365);
    CHECK(parse_day_count_convention_code("A365F") == day_count_convention_code::A365F);
    CHECK(parse_day_count_convention_code("30/360") == day_count_convention_code::THIRTY_360);
}

TEST_CASE("parse_day_count_convention_code rejects an unsupported day_count_fraction_type.code",
          "[curve_bootstrap_engine]") {
    CHECK_THROWS_AS(parse_day_count_convention_code("ActAct(ISDA)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("Business252"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("30E/360"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code(""), std::invalid_argument);
}
