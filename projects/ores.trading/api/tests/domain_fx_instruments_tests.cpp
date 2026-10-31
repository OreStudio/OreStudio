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
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/fx_instruments.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <string>

namespace {

using namespace ores::trading::domain;

std::chrono::year_month_day an_fx_date() {
    return std::chrono::year{2027} / std::chrono::month{3} / std::chrono::day{15};
}

decimal fx_money(const std::string& text) {
    return *decimal::from_string(text);
}

fx_legs_terms make_fx_legs() {
    fx_legs_terms legs;
    legs.bought_currency = "EUR";
    legs.bought_amount = fx_money("1000000");
    legs.sold_currency = "USD";
    legs.sold_amount = fx_money("1100000");
    return legs;
}

fx_barrier_option_terms make_barrier_terms() {
    fx_barrier_option_terms terms;
    terms.legs = make_fx_legs();
    terms.option_type = "Call";
    terms.expiry_date = an_fx_date();
    terms.settlement = "Cash";
    terms.barrier_type = "UpAndOut";
    terms.upper_barrier = fx_money("1.15");
    terms.underlying_code = "EUR/USD";
    return terms;
}

fx_digital_option_terms make_digital_terms() {
    fx_digital_option_terms terms;
    terms.foreign_currency = "EUR";
    terms.domestic_currency = "USD";
    terms.payoff_currency = "USD";
    terms.payoff_amount = fx_money("100000");
    terms.option_type = "Call";
    terms.expiry_date = an_fx_date();
    terms.long_short = "Long";
    terms.strike = fx_money("1.10");
    return terms;
}

}

using namespace ores::logging;

TEST_CASE("an_fx_forward_states_what_is_bought_and_sold", "[domain]") {
    auto lg(make_logger(test_suite));

    const auto sut = construct_fx_forward(fx_forward_terms{make_fx_legs(), an_fx_date(), "Cash"});
    BOOST_LOG_SEV(lg, info) << "Forward type: " << sut.identity.trade_type_code;

    CHECK(sut.identity.trade_type_code == "FxForward");
    CHECK(sut.bought_currency == "EUR");
    CHECK(sut.bought_amount.to_string() == "1000000");
    CHECK(sut.sold_currency == "USD");
}

TEST_CASE("an_fx_swap_refuses_two_legs_in_one_currency", "[domain]") {
    auto lg(make_logger(test_suite));

    auto legs = make_fx_legs();
    legs.sold_currency = legs.bought_currency;
    BOOST_LOG_SEV(lg, info) << "Swapping one currency against itself is refused";

    CHECK_THROWS_AS(construct_fx_swap(fx_forward_terms{legs, an_fx_date(), "Cash"}),
                    fx_product_error);
}

TEST_CASE("a_double_barrier_requires_both_barriers", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_barrier_terms();
    const auto single = construct_fx_barrier_option(terms);
    BOOST_LOG_SEV(lg, info) << "Single barrier upper: " << single.upper_barrier->to_string();

    CHECK(single.upper_barrier.has_value());
    CHECK_FALSE(single.lower_barrier.has_value());
    CHECK(single.identity.trade_type_code == "FxBarrierOption");
    CHECK_THROWS_AS(construct_fx_double_barrier_option(terms), fx_product_error);

    terms.lower_barrier = fx_money("1.05");
    const auto double_barrier = construct_fx_double_barrier_option(terms);
    CHECK(double_barrier.lower_barrier.has_value());
    CHECK(double_barrier.identity.trade_type_code == "FxDoubleBarrierOption");
}

TEST_CASE("a_barrier_type_that_names_an_upper_barrier_requires_one", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_barrier_terms();
    terms.upper_barrier.reset();
    terms.lower_barrier = fx_money("1.05");
    BOOST_LOG_SEV(lg, info) << "Refusing UpAndOut with only a lower barrier";

    CHECK_THROWS_AS(construct_fx_barrier_option(terms), fx_product_error);
}

TEST_CASE("a_digital_requires_a_strike_and_a_touch_requires_a_barrier", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_digital_terms();
    const auto digital = construct_fx_digital_option(terms);
    BOOST_LOG_SEV(lg, info) << "Digital payoff: " << digital.payoff_amount.to_string();

    CHECK(digital.identity.trade_type_code == "FxDigitalOption");
    CHECK(digital.strike.has_value());

    terms.strike.reset();
    CHECK_THROWS_AS(construct_fx_digital_option(terms), fx_product_error);
    CHECK_THROWS_AS(construct_fx_touch_option(terms), fx_product_error);

    terms.lower_barrier = fx_money("1.05");
    const auto touch = construct_fx_touch_option(terms);
    CHECK(touch.identity.trade_type_code == "FxTouchOption");
    CHECK(touch.lower_barrier.has_value());
}

TEST_CASE("a_double_touch_requires_two_barriers", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_digital_terms();
    terms.strike.reset();
    terms.lower_barrier = fx_money("1.05");
    BOOST_LOG_SEV(lg, info) << "Refusing a double touch with one barrier";

    CHECK_THROWS_AS(construct_fx_double_touch_option(terms), fx_product_error);

    terms.upper_barrier = fx_money("1.15");
    CHECK(construct_fx_double_touch_option(terms).identity.trade_type_code ==
          "FxDoubleTouchOption");
}

TEST_CASE("an_average_forward_requires_an_index_and_a_tarf_a_target", "[domain]") {
    auto lg(make_logger(test_suite));

    fx_asian_forward_terms terms;
    terms.reference_currency = "EUR";
    terms.settlement_currency = "USD";
    terms.currency = "USD";
    terms.long_short = "Long";
    BOOST_LOG_SEV(lg, info) << "Refusing an average forward with no index";

    CHECK_THROWS_AS(construct_fx_average_forward(terms), fx_product_error);

    terms.fx_index = "EUR/USD";
    CHECK(construct_fx_average_forward(terms).identity.trade_type_code == "FxAverageForward");

    fx_tarf_terms tarf;
    tarf.fx_index = terms.fx_index;
    tarf.reference_currency = terms.reference_currency;
    tarf.settlement_currency = terms.settlement_currency;
    tarf.currency = terms.currency;
    tarf.long_short = terms.long_short;
    CHECK_THROWS_AS(construct_fx_target_redemption_forward(tarf), fx_product_error);

    tarf.target_amount = fx_money("50000");
    CHECK(construct_fx_target_redemption_forward(tarf).identity.trade_type_code == "FxTaRF");
}

TEST_CASE("a_variance_swap_requires_an_end_after_its_start_and_a_positive_strike", "[domain]") {
    auto lg(make_logger(test_suite));

    fx_variance_swap_terms terms;
    terms.start_date = an_fx_date();
    terms.end_date = an_fx_date();
    terms.currency = "USD";
    terms.underlying_code = "EUR/USD";
    terms.long_short = "Long";
    terms.strike = 0.08;
    terms.notional = fx_money("1000000");
    terms.moment_type = "Variance";
    BOOST_LOG_SEV(lg, info) << "Refusing a variance swap whose end is not after its start";

    CHECK_THROWS_AS(construct_fx_variance_swap(terms), fx_product_error);
}

TEST_CASE("an_accumulator_states_the_range_its_fixings_accrue_inside", "[domain]") {
    auto lg(make_logger(test_suite));

    fx_accumulator_terms terms{"USD",
                               fx_money("10000"),
                               "EUR/USD",
                               "Long",
                               an_fx_date(),
                               fx_money("1.10"),
                               fx_money("1.15")};
    const auto sut = construct_fx_accumulator(terms);
    BOOST_LOG_SEV(lg, info) << "Accumulator bounds: " << sut.range_bounds.size();

    CHECK(sut.row.identity.trade_type_code == "FxAccumulator");
    REQUIRE(sut.range_bounds.size() == 1);
    CHECK(sut.range_bounds[0].sequence_number == 1);
    CHECK(sut.range_bounds[0].range_from->to_string() == "1.1");
    CHECK(sut.range_bounds[0].range_to->to_string() == "1.15");
}
