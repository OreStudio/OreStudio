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
#include "ores.trading.api/domain/equity_instruments.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

using namespace ores::trading::domain;
using ores::utility::decimal::decimal;

std::chrono::year_month_day a_date() {
    return std::chrono::year{2027} / std::chrono::month{3} / std::chrono::day{15};
}

decimal money(const std::string& text) {
    return *decimal::from_string(text);
}

equity_cliquet_option_terms make_cliquet_terms() {
    equity_cliquet_option_terms terms;
    terms.underlying_name = ".SPX";
    terms.currency = "USD";
    terms.notional = money("1000000");
    terms.option_type = "Call";
    terms.strike = money("4500");
    terms.expiry_date = a_date();
    terms.exercise_type = "European";
    terms.long_short = "Long";
    terms.settlement_type = "Cash";
    terms.frequency = "Quarterly";
    return terms;
}

equity_double_barrier_option_terms make_double_barrier_terms() {
    equity_double_barrier_option_terms terms;
    terms.underlying_name = ".SPX";
    terms.currency = "USD";
    terms.notional = money("1000000");
    terms.option_type = "Call";
    terms.strike = money("4500");
    terms.expiry_date = a_date();
    terms.exercise_type = "European";
    terms.long_short = "Long";
    terms.lower_barrier = money("4050");
    terms.lower_barrier_type = "DownAndOut";
    terms.upper_barrier = money("4950");
    terms.upper_barrier_type = "UpAndOut";
    return terms;
}

equity_tarf_terms make_tarf_terms() {
    equity_tarf_terms terms;
    terms.underlying_name = ".SPX";
    terms.currency = "USD";
    terms.strike = money("4500");
    terms.fixing_amount = money("1000000");
    terms.start_date = a_date();
    terms.expiry_date = a_date();
    terms.fixing_frequency = "Daily";
    terms.long_short = "Long";
    terms.payoff_type = "TargetFull";
    terms.target_amount = money("2000000");
    terms.target_type = "TargetFull";
    return terms;
}

}

using namespace ores::logging;

TEST_CASE("a_cliquet_is_constructed_as_the_cliquet_it_states", "[domain]") {
    auto lg(make_logger(test_suite));

    const auto sut = construct_equity_cliquet_option(make_cliquet_terms());
    BOOST_LOG_SEV(lg, info) << "Cliquet underlying: " << sut.underlying_name;

    CHECK(sut.identity.trade_type_code == "EquityCliquetOption");
    CHECK(sut.cliquet_frequency == "Quarterly");
    CHECK(sut.underlying_name == ".SPX");
    CHECK(sut.option_type == "Call");
    CHECK(sut.notional.to_string() == "1000000");
    CHECK(sut.strike.to_string() == "4500");
    CHECK(sut.settlement_type == "Cash");
}

TEST_CASE("a_cliquet_without_a_frequency_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_cliquet_terms();
    terms.frequency.clear();
    BOOST_LOG_SEV(lg, info) << "Refusing a cliquet with no frequency";

    CHECK_THROWS_AS(construct_equity_cliquet_option(terms), equity_product_error);
}

TEST_CASE("a_cliquet_with_a_notional_that_is_not_positive_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_cliquet_terms();
    terms.notional = decimal{};
    BOOST_LOG_SEV(lg, info) << "Refusing a cliquet with a zero notional";

    CHECK_THROWS_AS(construct_equity_cliquet_option(terms), equity_product_error);
}

TEST_CASE("a_cliquet_with_an_option_type_outside_the_closed_set_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_cliquet_terms();
    terms.option_type = "Straddle";
    BOOST_LOG_SEV(lg, info) << "Refusing a cliquet whose option type is not Call or Put";

    CHECK_THROWS_AS(construct_equity_cliquet_option(terms), equity_product_error);
}

TEST_CASE("a_cliquet_with_an_exercise_type_outside_the_closed_set_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_cliquet_terms();
    terms.exercise_type = "Perpetual";
    BOOST_LOG_SEV(lg, info) << "Refusing a cliquet whose exercise type is not ORE's";

    CHECK_THROWS_AS(construct_equity_cliquet_option(terms), equity_product_error);
}

TEST_CASE("a_digital_states_a_strike_and_a_touch_states_a_barrier", "[domain]") {
    auto lg(make_logger(test_suite));

    equity_digital_option_terms digital;
    digital.underlying_name = ".SPX";
    digital.currency = "USD";
    digital.notional = money("1000000");
    digital.option_type = "Put";
    digital.strike = money("4000");
    digital.expiry_date = a_date();
    digital.long_short = "Long";

    equity_touch_option_terms touch;
    touch.underlying_name = ".SPX";
    touch.currency = "USD";
    touch.notional = money("1000000");
    touch.barrier_level = money("4000");
    touch.barrier_type = "DownAndOut";
    touch.expiry_date = a_date();
    touch.long_short = "Long";

    const auto digital_row = construct_equity_digital_option(digital);
    const auto touch_row = construct_equity_touch_option(touch);
    BOOST_LOG_SEV(lg, info) << "Digital strike: " << digital_row.strike->to_string();

    CHECK(digital_row.identity.trade_type_code == "EquityDigitalOption");
    CHECK(digital_row.strike->to_string() == "4000");
    CHECK_FALSE(digital_row.barrier_level.has_value());
    CHECK(touch_row.identity.trade_type_code == "EquityTouchOption");
    CHECK(touch_row.barrier_level->to_string() == "4000");
    CHECK_FALSE(touch_row.strike.has_value());
}

TEST_CASE("a_double_barrier_below_its_lower_barrier_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_double_barrier_terms();
    terms.upper_barrier = terms.lower_barrier;
    BOOST_LOG_SEV(lg, info) << "Refusing a double barrier whose upper is not above its lower";

    CHECK_THROWS_AS(construct_equity_double_barrier_option(terms), equity_product_error);
}

TEST_CASE("an_accumulator_states_no_target_and_a_tarf_requires_one", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_tarf_terms();
    terms.target_amount = decimal{};
    BOOST_LOG_SEV(lg, info) << "Refusing a target redemption forward with no target";

    CHECK_THROWS_AS(construct_equity_target_redemption_forward(terms), equity_product_error);

    equity_accumulator_terms accumulator;
    accumulator.underlying_name = ".SPX";
    accumulator.currency = "USD";
    accumulator.strike = money("4500");
    accumulator.fixing_amount = money("1000000");
    accumulator.start_date = a_date();
    accumulator.expiry_date = a_date();
    accumulator.fixing_frequency = "Daily";
    accumulator.long_short = "Long";
    accumulator.payoff_type = "Accumulator";

    accumulator.range_bounds.push_back({money("4400"), std::nullopt, 1.0});
    accumulator.range_bounds.push_back({std::nullopt, money("4600"), 2.0});

    const auto sut = construct_equity_accumulator(accumulator);
    CHECK(sut.row.identity.trade_type_code == "EquityAccumulator");
    CHECK_FALSE(sut.row.target_amount.has_value());
    CHECK(sut.row.target_type.empty());
    REQUIRE(sut.range_bounds.size() == 2);
    CHECK(sut.range_bounds[0].sequence_number == 1);
    CHECK(sut.range_bounds[0].range_from->to_string() == "4400");
    CHECK(sut.range_bounds[1].sequence_number == 2);
    CHECK(sut.range_bounds[1].leverage.has_value());
}

TEST_CASE("a_forward_without_a_quantity_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    equity_forward_terms terms;
    terms.underlying_name = ".SPX";
    terms.currency = "USD";
    terms.quantity = 0.0;
    terms.expiry_date = a_date();
    terms.long_short = "Long";
    terms.settlement_type = "Cash";
    BOOST_LOG_SEV(lg, info) << "Refusing a forward with no quantity";

    CHECK_THROWS_AS(construct_equity_forward(terms), equity_product_error);
}

TEST_CASE("a_worst_of_basket_swap_without_a_basket_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    equity_basket_swap_terms terms;
    terms.currency = "USD";
    terms.notional = money("1000000");
    terms.return_type = "Price";
    terms.start_date = a_date();
    terms.maturity_date = a_date();
    terms.long_short = "Long";
    terms.payment_frequency_code = "Quarterly";
    BOOST_LOG_SEV(lg, info) << "Refusing a worst-of basket with no members";

    CHECK_THROWS_AS(construct_equity_worst_of_basket_swap(terms), equity_product_error);
}

TEST_CASE("a_worst_of_basket_swap_states_no_single_underlying", "[domain]") {
    auto lg(make_logger(test_suite));

    equity_basket_swap_terms terms;
    terms.currency = "USD";
    terms.notional = money("1000000");
    terms.return_type = "Price";
    terms.start_date = a_date();
    terms.maturity_date = a_date();
    terms.long_short = "Long";
    terms.payment_frequency_code = "Quarterly";

    terms.basket.push_back({".SPX", "equity", 0.5, money("4500")});
    terms.basket.push_back({".NDX", "equity", 0.5, money("15000")});
    terms.periods.push_back({a_date(), std::nullopt, std::nullopt});

    const auto sut = construct_equity_worst_of_basket_swap(terms);
    BOOST_LOG_SEV(lg, info) << "Basket swap type: " << sut.row.identity.trade_type_code;

    CHECK(sut.row.identity.trade_type_code == "EquityWorstOfBasketSwap");
    CHECK(sut.row.underlying_name.empty());
    CHECK(sut.row.return_type == "Price");
    REQUIRE(sut.constituents.size() == 2);
    CHECK(sut.constituents[0].sequence_number == 1);
    CHECK(sut.constituents[1].underlying_name == ".NDX");
    REQUIRE(sut.periods.size() == 1);
    CHECK(sut.periods[0].sequence_number == 1);
}
