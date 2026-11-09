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
#include "ores.analytics.quant/service/fx_forward_calculator.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

using namespace ores::analytics::quant::service;
using namespace std::chrono;
using Catch::Approx;
using Catch::Matchers::ContainsSubstring;

namespace {

const year_month_day today = 2024y / March / 15d;
const year_month_day six_months = 2024y / September / 15d;

year_month_day after(int days) {
    return year_month_day{sys_days(today) + std::chrono::days{days}};
}

double flat_discount(double rate, int days) {
    return std::exp(-rate * days / 365.0);
}

std::vector<bootstrapped_point> flat_curve(double rate) {
    return {{"", today, 1.0}, {"LONG", after(400), flat_discount(rate, 400)}};
}

/// The fixture of QuantLib's test-suite/fxforward.cpp: 1 USD = 1.35 SGD, USD at 5%, SGD at 3.5%.
fx_forward_request usd_sgd(year_month_day spot_date = today) {
    return {.base_currency = "USD",
            .quote_currency = "SGD",
            .spot = 1.35,
            .spot_date = spot_date,
            .pip_factor = 0.0001,
            .base_curve = flat_curve(0.05),
            .quote_curve = flat_curve(0.035),
            .tenors = {{.tenor_id = "6M", .value_date = six_months}}};
}

fx_forward_point only_point(const fx_forward_request& request) {
    const auto result = calculate_fx_forwards(request);
    REQUIRE(result.has_value());
    REQUIRE(result->size() == 1);
    return result->front();
}

}

TEST_CASE("fair forward follows covered interest parity on the QuantLib fixture",
          "[fx_forward_calculator]") {
    // QuantLib's fairForwardRate is S * D_target / D_source, the inverse of parity for a spot
    // quoted target per source, and 1.36 here. This value is parity, by hand.
    CHECK(only_point(usd_sgd()).forward_rate == Approx(1.3398302791826484).epsilon(1e-12));
}

TEST_CASE("a forward at the fair rate has zero NPV, as QuantLib's testAtTheMoney",
          "[fx_forward_calculator]") {
    const auto settlement = after(2);
    const auto point = only_point(usd_sgd(settlement));
    constexpr double usd_nominal = 1000000.0;
    const double sgd_nominal = usd_nominal * point.forward_rate;

    const double df_usd = point.base_discount / flat_discount(0.05, 2);
    const double df_sgd = point.quote_discount / flat_discount(0.035, 2);
    const double npv = -usd_nominal * df_usd + sgd_nominal * df_sgd / 1.35;

    CHECK(npv == Approx(0.0).margin(1e-4));
    CHECK(point.forward_rate == Approx(1.3399404067450218).epsilon(1e-12));
}

TEST_CASE("a covered round trip through the quote currency earns the base currency return",
          "[fx_forward_calculator]") {
    const auto point = only_point(usd_sgd());
    const double direct = 1.0 / point.base_discount;
    const double covered = 1.35 / point.quote_discount / point.forward_rate;
    CHECK(covered == Approx(direct).epsilon(1e-12));
}

TEST_CASE("the forward moves with each curve and scales with spot, as QuantLib's sensitivities",
          "[fx_forward_calculator]") {
    const double base_forward = only_point(usd_sgd()).forward_rate;

    auto usd_up = usd_sgd();
    usd_up.base_curve = flat_curve(0.051);
    CHECK(only_point(usd_up).forward_rate < base_forward);

    auto sgd_up = usd_sgd();
    sgd_up.quote_curve = flat_curve(0.036);
    CHECK(only_point(sgd_up).forward_rate > base_forward);

    auto spot_up = usd_sgd();
    spot_up.spot = 1.40;
    CHECK(only_point(spot_up).forward_rate == Approx(base_forward * 1.40 / 1.35).epsilon(1e-12));
}

TEST_CASE("forward points scale by the pair's pip factor", "[fx_forward_calculator]") {
    const auto one_year = 2026y / January / 1d;
    const auto points_of = [&](double spot, double pip, double base_rate, double quote_rate) {
        const std::vector<bootstrapped_point> anchor{{"", 2025y / January / 1d, 1.0}};
        auto base = anchor;
        base.push_back({"1Y", one_year, std::exp(-base_rate)});
        auto quote = anchor;
        quote.push_back({"1Y", one_year, std::exp(-quote_rate)});
        return only_point({.base_currency = "BBB",
                           .quote_currency = "QQQ",
                           .spot = spot,
                           .spot_date = 2025y / January / 1d,
                           .pip_factor = pip,
                           .base_curve = base,
                           .quote_curve = quote,
                           .tenors = {{.tenor_id = "1Y", .value_date = one_year}}});
    };

    SECTION("EUR/USD") {
        const auto point = points_of(1.10, 0.0001, 0.02, 0.04);
        CHECK(point.forward_rate == Approx(1.1222214740294316).epsilon(1e-12));
        CHECK(point.forward_points == Approx(222.2147402943153).epsilon(1e-10));
    }
    SECTION("USD/JPY") {
        const auto point = points_of(150.0, 0.01, 0.04, 0.01);
        CHECK(point.forward_rate == Approx(145.56683003227621).epsilon(1e-12));
        CHECK(point.forward_points == Approx(-443.3169967723785).epsilon(1e-10));
    }
}

TEST_CASE("a spot-start swap quotes the forward points", "[fx_forward_calculator]") {
    const auto point = only_point(usd_sgd(after(2)));
    CHECK(point.near_date == after(2));
    CHECK(point.swap_points == point.forward_points);
    CHECK(point.forward_points != 0.0);
}

TEST_CASE("the spot date itself has a forward of spot and no points", "[fx_forward_calculator]") {
    auto request = usd_sgd(after(2));
    request.tenors = {{.tenor_id = "SP", .value_date = after(2)}};
    const auto point = only_point(request);
    CHECK(point.forward_rate == 1.35);
    CHECK(point.forward_points == 0.0);
    CHECK(point.swap_points == 0.0);
}

TEST_CASE("ON and TN quote their own swaps and chain to spot", "[fx_forward_calculator]") {
    auto request = usd_sgd(after(2));
    request.tenors = {{.tenor_id = "ON", .value_date = after(1), .near_date = today},
                      {.tenor_id = "TN", .value_date = after(2), .near_date = after(1)}};
    const auto result = calculate_fx_forwards(request);
    REQUIRE(result.has_value());
    const auto& on = (*result)[0];
    const auto& tn = (*result)[1];

    CHECK(on.swap_points != Approx(on.forward_points));
    CHECK(tn.forward_points == 0.0);
    CHECK(tn.swap_points != 0.0);

    auto value_today = usd_sgd(after(2));
    value_today.tenors = {{.tenor_id = "TD", .value_date = today}};
    const double points_today = only_point(value_today).forward_points;
    CHECK(on.swap_points + tn.swap_points == Approx(-points_today).epsilon(1e-9));
}

TEST_CASE("a forward-forward swap is the difference of its two forwards",
          "[fx_forward_calculator]") {
    auto request = usd_sgd();
    request.tenors = {{.tenor_id = "3M", .value_date = after(92)},
                      {.tenor_id = "3Mx6M", .value_date = six_months, .near_date = after(92)}};
    const auto result = calculate_fx_forwards(request);
    REQUIRE(result.has_value());
    CHECK((*result)[1].near_date == after(92));
    CHECK((*result)[1].swap_points ==
          Approx((*result)[1].forward_points - (*result)[0].forward_points).epsilon(1e-9));
}

TEST_CASE("the result states each curve's own discount factors", "[fx_forward_calculator]") {
    const auto point = only_point(usd_sgd());
    CHECK(point.base_discount == Approx(flat_discount(0.05, 184)).epsilon(1e-12));
    CHECK(point.quote_discount == Approx(flat_discount(0.035, 184)).epsilon(1e-12));
}

TEST_CASE("the forward depends only on discount ratios from the spot date",
          "[fx_forward_calculator]") {
    const auto spot_date = after(2);
    const double before = only_point(usd_sgd(spot_date)).forward_rate;

    auto rebased = usd_sgd(spot_date);
    for (auto& p : rebased.base_curve)
        p.discount_factor *= 0.9;
    for (auto& p : rebased.quote_curve)
        p.discount_factor *= 1.1;
    CHECK(only_point(rebased).forward_rate == Approx(before).epsilon(1e-12));
}

TEST_CASE("each result keeps the order and the identity of its tenor", "[fx_forward_calculator]") {
    auto request = usd_sgd();
    request.tenors = {{.tenor_id = "6M", .value_date = six_months},
                      {.tenor_id = "1M", .value_date = after(31)}};
    const auto result = calculate_fx_forwards(request);
    REQUIRE(result.has_value());
    REQUIRE(result->size() == 2);
    CHECK((*result)[0].tenor_id == "6M");
    CHECK((*result)[1].tenor_id == "1M");
    CHECK((*result)[1].value_date == after(31));
}

TEST_CASE("an invalid request is refused and names what failed", "[fx_forward_calculator]") {
    const auto refused = [](const fx_forward_request& request) {
        const auto result = calculate_fx_forwards(request);
        REQUIRE_FALSE(result.has_value());
        return result.error();
    };
    auto request = usd_sgd();

    SECTION("zero spot") {
        request.spot = 0.0;
        CHECK_THAT(refused(request), ContainsSubstring("spot of USD/SGD"));
    }
    SECTION("NaN spot") {
        request.spot = std::numeric_limits<double>::quiet_NaN();
        CHECK_THAT(refused(request), ContainsSubstring("spot of USD/SGD"));
    }
    SECTION("infinite spot") {
        request.spot = std::numeric_limits<double>::infinity();
        CHECK_THAT(refused(request), ContainsSubstring("spot of USD/SGD"));
    }
    SECTION("zero pip factor") {
        request.pip_factor = 0.0;
        CHECK_THAT(refused(request), ContainsSubstring("pip factor of USD/SGD"));
    }
    SECTION("empty base curve") {
        request.base_curve.clear();
        CHECK_THAT(refused(request), ContainsSubstring("USD curve cannot state the spot date"));
    }
    SECTION("empty quote curve") {
        request.quote_curve.clear();
        CHECK_THAT(refused(request), ContainsSubstring("SGD curve cannot state the spot date"));
    }
    SECTION("a tenor beyond the quote curve") {
        request.tenors = {{.tenor_id = "2Y", .value_date = after(730)}};
        request.base_curve.push_back({"2Y", after(800), 0.9});
        CHECK_THAT(refused(request), ContainsSubstring("SGD curve cannot state 2Y"));
    }
    SECTION("a near leg before the curve") {
        request.tenors.front().near_date = year_month_day{sys_days(today) - days{1}};
        CHECK_THAT(refused(request), ContainsSubstring("USD curve cannot state 6M near leg"));
    }
    SECTION("a spline the curve engine does not implement") {
        request.interpolation = interpolation_method_code::CUBIC_SPLINE;
        CHECK_THAT(refused(request), ContainsSubstring("CUBIC_SPLINE"));
    }
    SECTION("a discount factor that is not positive") {
        request.quote_curve.back().discount_factor = 0.0;
        request.tenors = {{.tenor_id = "LONG", .value_date = after(400)}};
        CHECK_THAT(refused(request), ContainsSubstring("SGD curve holds no positive"));
    }
}

TEST_CASE("forward points match the money market formula on discrete rates with separate day bases",
          "[fx_forward_calculator]") {
    // P = S * [(1 + R2/100 * N/B2) / (1 + R1/100 * N/B1) - 1], with USD as the base currency.
    constexpr double spot = 150.0;
    constexpr double usd_rate = 5.0;
    constexpr double jpy_rate = 0.5;
    constexpr double usd_basis = 360.0;
    constexpr double jpy_basis = 365.0;
    constexpr int tenor_days = 90;
    const double usd_growth = 1.0 + usd_rate / 100.0 * tenor_days / usd_basis;
    const double jpy_growth = 1.0 + jpy_rate / 100.0 * tenor_days / jpy_basis;
    const double expected = spot * (jpy_growth / usd_growth - 1.0);

    const auto point =
        only_point({.base_currency = "USD",
                    .quote_currency = "JPY",
                    .spot = spot,
                    .spot_date = today,
                    .pip_factor = 0.01,
                    .base_curve = {{"", today, 1.0}, {"3M", after(tenor_days), 1.0 / usd_growth}},
                    .quote_curve = {{"", today, 1.0}, {"3M", after(tenor_days), 1.0 / jpy_growth}},
                    .tenors = {{.tenor_id = "3M", .value_date = after(tenor_days)}}});
    CHECK(point.forward_points * 0.01 == Approx(expected).epsilon(1e-12));

    const double backed_out_rate =
        100.0 * jpy_basis / tenor_days * ((expected / spot + 1.0) * usd_growth - 1.0);
    CHECK(backed_out_rate == Approx(jpy_rate).epsilon(1e-12));
}

TEST_CASE("the higher rate in the quote currency makes a premium, the lower a discount",
          "[fx_forward_calculator]") {
    auto premium = usd_sgd();
    premium.base_curve = flat_curve(0.01);
    CHECK(only_point(premium).forward_points > 0.0);
    CHECK(only_point(usd_sgd()).forward_points < 0.0);
}

TEST_CASE("a discount factor above one is valid and the forward stays positive",
          "[fx_forward_calculator]") {
    auto request = usd_sgd();
    request.quote_curve = flat_curve(-0.01);
    const auto point = only_point(request);
    CHECK(point.quote_discount > 1.0);
    CHECK(point.forward_rate > 0.0);
}

TEST_CASE("outright values before spot chain from the short-date swaps",
          "[fx_forward_calculator]") {
    constexpr double pip = 0.0001;
    const auto outright = [](const fx_forward_request& request, year_month_day date) {
        auto dated = request;
        dated.tenors = {{.tenor_id = "D", .value_date = date}};
        return only_point(dated).forward_rate;
    };

    SECTION("spot at t+2") {
        auto request = usd_sgd(after(2));
        request.tenors = {{.tenor_id = "ON", .value_date = after(1), .near_date = today},
                          {.tenor_id = "TN", .value_date = after(2), .near_date = after(1)}};
        const auto result = calculate_fx_forwards(request);
        REQUIRE(result.has_value());
        const double on = (*result)[0].swap_points;
        const double tn = (*result)[1].swap_points;
        CHECK(outright(request, today) == Approx(1.35 - tn * pip - on * pip).epsilon(1e-12));
        CHECK(outright(request, after(1)) == Approx(1.35 - tn * pip).epsilon(1e-12));
    }
    SECTION("spot at t+1") {
        auto request = usd_sgd(after(1));
        request.tenors = {{.tenor_id = "ON", .value_date = after(1), .near_date = today}};
        const auto result = calculate_fx_forwards(request);
        REQUIRE(result.has_value());
        const double on = (*result)[0].swap_points;
        CHECK(outright(request, today) == Approx(1.35 - on * pip).epsilon(1e-12));
        CHECK(outright(request, after(1)) == 1.35);
    }
}
