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
#include "ores.analytics.quant/math/fx_pair_market.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <string>
#include <utility>
#include <vector>

using namespace ores::analytics::quant::math;

namespace {

constexpr std::chrono::year_month_day market_spot_date{
    std::chrono::year{2026}, std::chrono::November, std::chrono::day{16}};
constexpr double market_spot = 1.0842;

std::chrono::year_month_day day_after_spot(int days) {
    return std::chrono::year_month_day(std::chrono::sys_days(market_spot_date) +
                                       std::chrono::days(days));
}

/// A set built from two constant continuously compounded rates, so any day in it has one
/// answer: the discounts are exact and an interpolation on a straight rate recovers them.
std::vector<fx_discount_point> flat_rate_set(double base_rate, double quote_rate) {
    std::vector<fx_discount_point> rows;
    for (const int offset : {7, 14, 30, 90, 180, 365}) {
        const double years = static_cast<double>(offset) / 365.0;
        rows.push_back({.date = day_after_spot(offset),
                        .base_discount = std::exp(-base_rate * years),
                        .quote_discount = std::exp(-quote_rate * years)});
    }
    return rows;
}

}

TEST_CASE("a value date the set quotes takes that row's two discounts", "[fx_pair_market]") {
    const auto rows = flat_rate_set(0.03, 0.01);
    const auto market = pair_market_at(market_spot, market_spot_date, rows, rows[2].date, "3M");
    REQUIRE(market);
    CHECK(market->spot == market_spot);
    CHECK(market->foreign_discount == Catch::Approx(rows[2].base_discount).epsilon(1e-15));
    CHECK(market->domestic_discount == Catch::Approx(rows[2].quote_discount).epsilon(1e-15));
}

TEST_CASE("a day between two rows recovers a flat rate's own discount", "[fx_pair_market]") {
    // Days 45 and 120 sit between rows and are not on any of them. On a constant rate both
    // must come back exactly, which no other interpolation rule gives.
    const auto rows = flat_rate_set(0.03, 0.01);
    for (const int offset : {45, 120}) {
        const double years = static_cast<double>(offset) / 365.0;
        const auto market =
            pair_market_at(market_spot, market_spot_date, rows, day_after_spot(offset), "");
        INFO("offset " << offset);
        REQUIRE(market);
        CHECK(market->foreign_discount == Catch::Approx(std::exp(-0.03 * years)).epsilon(1e-14));
        CHECK(market->domestic_discount == Catch::Approx(std::exp(-0.01 * years)).epsilon(1e-14));
    }
}

TEST_CASE("the base discount is the foreign side and the quote discount the domestic",
          "[fx_pair_market]") {
    // EUR/USD: EUR is the base and the foreign side, USD the quote and the domestic side, so
    // the forward is the spot times base over quote. A set whose base discounts faster than
    // its quote therefore forwards above the spot, never below it.
    const std::vector<fx_discount_point> rows{
        {.date = day_after_spot(90), .base_discount = 0.99, .quote_discount = 0.98}};
    const auto market =
        pair_market_at(market_spot, market_spot_date, rows, rows.front().date, "3M");
    REQUIRE(market);
    CHECK(market->forward() > market->spot);
    CHECK(market->forward() / market->spot == Catch::Approx(0.99 / 0.98).epsilon(1e-15));
}

TEST_CASE("a value date on the spot date carries no rate differential", "[fx_pair_market]") {
    const auto rows = flat_rate_set(0.03, 0.01);
    const auto market = pair_market_at(market_spot, market_spot_date, rows, market_spot_date, "");
    REQUIRE(market);
    CHECK(market->domestic_discount == 1.0);
    CHECK(market->foreign_discount == 1.0);
    CHECK(market->forward() == Catch::Approx(market_spot).epsilon(1e-15));
}

TEST_CASE("a set that cannot answer a day says why", "[fx_pair_market]") {
    const auto rows = flat_rate_set(0.03, 0.01);

    SECTION("no rows") {
        const auto market =
            pair_market_at(market_spot, market_spot_date, {}, day_after_spot(7), "EUR/USD");
        REQUIRE_FALSE(market);
        CHECK(market.error().find("EUR/USD") != std::string::npos);
    }
    SECTION("a day past the last row") {
        const auto market =
            pair_market_at(market_spot, market_spot_date, rows, day_after_spot(400), "EUR/USD");
        REQUIRE_FALSE(market);
        CHECK(market.error().find("2027-11-16") != std::string::npos);
    }
    SECTION("a day before the spot date") {
        const auto market =
            pair_market_at(market_spot, market_spot_date, rows, day_after_spot(-1), "EUR/USD");
        REQUIRE_FALSE(market);
        CHECK(market.error().find("before the spot date") != std::string::npos);
    }
    SECTION("a discount that is not above zero") {
        auto broken = rows;
        broken[1].quote_discount = 0.0;
        const auto market =
            pair_market_at(market_spot, market_spot_date, broken, day_after_spot(7), "EUR/USD");
        REQUIRE_FALSE(market);
        CHECK(market.error().find("not above zero") != std::string::npos);
    }
    SECTION("rows out of date order") {
        auto broken = rows;
        std::swap(broken[1], broken[3]);
        const auto market =
            pair_market_at(market_spot, market_spot_date, broken, day_after_spot(60), "EUR/USD");
        REQUIRE_FALSE(market);
        CHECK(market.error().find("not in date order") != std::string::npos);
    }
    SECTION("a spot that is not above zero") {
        const auto market =
            pair_market_at(0.0, market_spot_date, rows, day_after_spot(7), "EUR/USD");
        REQUIRE_FALSE(market);
        CHECK(market.error().find("spot") != std::string::npos);
    }
}
