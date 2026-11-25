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
#include "ores.analytics.quant/service/term_rate_calculator.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>

using ores::analytics::quant::service::day_count_convention_code;
using ores::analytics::quant::service::term_rate_calculator;
using ores::analytics::quant::service::term_rate_rule;
using namespace std::chrono;

namespace {

/// A month of one-day periods, each fixed at its own rate, and the discount
/// factor the overnight schedule's compounding produces at the month's end.
struct overnight_month final {
    year_month_day start{year{2026}, January, day{2}};
    year_month_day end{year{2026}, February, day{2}};
    double discount_factor_end = 1.0;
    double compounded = 0.0;
    double last_rate = 0.0;
};

overnight_month build_overnight_month(double first_rate, double step) {
    overnight_month m;
    double product = 1.0;
    double rate = first_rate;
    for (auto d = sys_days{m.start}; d < sys_days{m.end}; d += days{1}) {
        product *= 1.0 + rate / 360.0;
        m.last_rate = rate;
        rate += step;
    }
    m.discount_factor_end = 1.0 / product;
    const double accrual = 31.0 / 360.0;
    m.compounded = (product - 1.0) / accrual;
    return m;
}

}

TEST_CASE("A compounded term rate equals the overnight curve's own compounding",
          "[term_rate_calculator]") {
    const auto m = build_overnight_month(0.05, 0.0001);

    const auto rate = term_rate_calculator::rate(0.0,
                                                 1.0,
                                                 m.discount_factor_end,
                                                 m.start,
                                                 m.end,
                                                 day_count_convention_code::A360,
                                                 term_rate_rule::compounded_overnight,
                                                 0.0);

    REQUIRE(rate == Catch::Approx(m.compounded).epsilon(1e-12));
}

TEST_CASE("A flat overnight curve compounds to just above its own rate", "[term_rate_calculator]") {
    const auto m = build_overnight_month(0.05, 0.0);

    const auto rate = term_rate_calculator::rate(0.0,
                                                 1.0,
                                                 m.discount_factor_end,
                                                 m.start,
                                                 m.end,
                                                 day_count_convention_code::A360,
                                                 term_rate_rule::compounded_overnight,
                                                 0.0);

    // Daily compounding of a flat 5% (ACT/360) sits a hair above 5%: the
    // cross terms of the product, worth about a basis point over a month.
    REQUIRE(rate > 0.05);
    REQUIRE(rate - 0.05 < 2e-4);
}

TEST_CASE("The compounded rule ignores a spread", "[term_rate_calculator]") {
    const auto m = build_overnight_month(0.05, 0.0001);

    const auto with = term_rate_calculator::rate(0.0,
                                                 1.0,
                                                 m.discount_factor_end,
                                                 m.start,
                                                 m.end,
                                                 day_count_convention_code::A360,
                                                 term_rate_rule::compounded_overnight,
                                                 500.0);
    const auto without = term_rate_calculator::rate(0.0,
                                                    1.0,
                                                    m.discount_factor_end,
                                                    m.start,
                                                    m.end,
                                                    day_count_convention_code::A360,
                                                    term_rate_rule::compounded_overnight,
                                                    0.0);

    REQUIRE(with == Catch::Approx(without));
}

TEST_CASE("A spread term rate is the overnight rate plus its basis points",
          "[term_rate_calculator]") {
    const auto m = build_overnight_month(0.05, 0.0001);

    const auto rate = term_rate_calculator::rate(m.last_rate,
                                                 1.0,
                                                 m.discount_factor_end,
                                                 m.start,
                                                 m.end,
                                                 day_count_convention_code::A360,
                                                 term_rate_rule::overnight_plus_spread,
                                                 125.0);

    REQUIRE(rate == Catch::Approx(m.last_rate + 0.0125).epsilon(1e-15));
}

TEST_CASE("A term rate refuses a curve that cannot state one", "[term_rate_calculator]") {
    const auto m = build_overnight_month(0.05, 0.0);

    REQUIRE_THROWS_AS(term_rate_calculator::rate(0.05,
                                                 1.0,
                                                 0.0,
                                                 m.start,
                                                 m.end,
                                                 day_count_convention_code::A360,
                                                 term_rate_rule::compounded_overnight,
                                                 0.0),
                      std::invalid_argument);

    // A term that ends where it starts has no year fraction to annualise by.
    REQUIRE_THROWS_AS(term_rate_calculator::rate(0.05,
                                                 1.0,
                                                 0.999,
                                                 m.start,
                                                 m.start,
                                                 day_count_convention_code::A360,
                                                 term_rate_rule::overnight_plus_spread,
                                                 0.0),
                      std::invalid_argument);
}
