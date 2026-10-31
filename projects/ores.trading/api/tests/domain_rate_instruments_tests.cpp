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
#include "ores.trading.api/domain/rate_instruments.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

using namespace ores::trading::domain;

std::chrono::year_month_day a_rate_date() {
    return std::chrono::year{2027} / std::chrono::month{3} / std::chrono::day{15};
}

decimal rate_money(const std::string& text) {
    return *decimal::from_string(text);
}

rate_common_terms make_rate_common() {
    rate_common_terms common;
    common.start_date = a_rate_date();
    common.maturity_date = a_rate_date();
    common.fixed.leg_type_code = "Fixed";
    common.fixed.day_count_fraction_code = "Act360";
    common.fixed.business_day_convention_code = "ModifiedFollowing";
    common.fixed.payment_frequency_code = "Annual";
    common.fixed.currency = "USD";
    common.fixed.rate_role = "fixed";
    common.fixed.value = rate_money("0.03");
    common.fixed.notional = rate_money("1000000");
    common.fixed.payer = true;
    common.floating.leg_type_code = "Floating";
    common.floating.day_count_fraction_code = "ActAct(ISDA)";
    common.floating.business_day_convention_code = "ModifiedFollowing";
    common.floating.payment_frequency_code = "Semiannual";
    common.floating.floating_index_code = "USD-SOFR";
    common.floating.currency = "USD";
    common.floating.rate_role = "spread";
    common.floating.value = rate_money("0.001");
    common.floating.notional = rate_money("1000000");
    common.floating.payer = false;
    return common;
}

}

using namespace ores::logging;

TEST_CASE("a_vanilla_swap_states_two_legs_with_their_amounts_and_rates", "[domain]") {
    auto lg(make_logger(test_suite));

    const auto sut = construct_rate_swap(rate_swap_terms{make_rate_common(), 2});
    BOOST_LOG_SEV(lg, info) << "Legs: " << sut.legs.size();

    REQUIRE(sut.legs.size() == 2);
    CHECK(sut.legs[0].identity.leg_number == 1);
    CHECK(sut.legs[0].leg_type_code == "Fixed");
    CHECK(sut.legs[0].payer.value_or(false));
    CHECK(sut.legs[1].identity.leg_number == 2);
    CHECK(sut.legs[1].leg_type_code == "Floating");
    CHECK_FALSE(sut.legs[1].payer.value_or(true));
    REQUIRE(sut.amounts.size() == 2);
    CHECK(sut.amounts[0].amount.to_string() == "1000000");
    REQUIRE(sut.rates.size() == 2);
    CHECK(sut.rates[0].rate_role == "fixed");
    CHECK(sut.rates[1].rate_role == "spread");
    CHECK(std::get<vanilla_swap_instrument>(sut.fact).settlement_lag == 2);
}

TEST_CASE("a_cross_currency_swap_states_two_different_leg_currencies", "[domain]") {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Refusing a cross currency swap with one currency";
    CHECK_THROWS_AS(construct_rate_cross_currency_swap(rate_swap_terms{make_rate_common(), 2}),
                    rate_product_error);

    auto common = make_rate_common();
    common.floating.currency = "EUR";
    const auto sut = construct_rate_cross_currency_swap(rate_swap_terms{common, 2});
    CHECK(std::get<vanilla_swap_instrument>(sut.fact).settlement_lag == 2);
}

TEST_CASE("a_fra_states_its_notional_index_and_direction", "[domain]") {
    auto lg(make_logger(test_suite));

    const auto sut = construct_rate_fra(rate_fra_terms{make_rate_common(), "Long"});
    BOOST_LOG_SEV(lg, info) << "FRA index: " << std::get<fra_instrument>(sut.fact).rate_index;

    CHECK(std::get<fra_instrument>(sut.fact).currency == "USD");
    CHECK(std::get<fra_instrument>(sut.fact).rate_index == "USD-SOFR");
    CHECK(std::get<fra_instrument>(sut.fact).notional.to_string() == "1000000");

    CHECK_THROWS_AS(construct_rate_fra(rate_fra_terms{make_rate_common(), "Either"}),
                    rate_product_error);
}

TEST_CASE("a_cap_states_a_strike_and_refuses_a_zero_one", "[domain]") {
    auto lg(make_logger(test_suite));

    const auto sut =
        construct_rate_cap_floor(rate_cap_floor_terms{make_rate_common(), rate_money("0.035")});
    BOOST_LOG_SEV(lg, info) << "Strike rows: " << sut.strikes.size();

    REQUIRE(sut.strikes.size() == 1);
    CHECK(sut.strikes[0].bare_value->to_string() == "0.035");
    CHECK(sut.strikes[0].bare_currency == "USD");
    CHECK(std::holds_alternative<cap_floor_instrument>(sut.fact));

    CHECK_THROWS_AS(construct_rate_cap_floor(rate_cap_floor_terms{make_rate_common(), decimal{}}),
                    rate_product_error);
}

TEST_CASE("a_swaption_requires_an_expiry", "[domain]") {
    auto lg(make_logger(test_suite));

    CHECK_THROWS_AS(
        construct_rate_swaption(rate_swaption_terms{
            make_rate_common(), std::chrono::year_month_day{}, "Long", "European", "Cash"}),
        rate_product_error);

    const auto sut = construct_rate_swaption(
        rate_swaption_terms{make_rate_common(), a_rate_date(), "Long", "European", "Cash"});
    CHECK(std::get<swaption_instrument>(sut.fact).exercise_type == "European");
    CHECK(std::get<swaption_instrument>(sut.fact).long_short == "Long");
}

TEST_CASE("the_simple_fact_products_state_their_own_row", "[domain]") {
    auto lg(make_logger(test_suite));

    const auto callable =
        construct_rate_callable_swap(rate_callable_swap_terms{make_rate_common()});
    const auto knock_out = construct_rate_knock_out_swap(
        rate_knock_out_swap_terms{make_rate_common(), a_rate_date(), "UpAndOut"});
    const auto flexi =
        construct_rate_flexi_swap(rate_flexi_swap_terms{make_rate_common(), "Short"});
    const auto balance = construct_rate_balance_guaranteed_swap(
        rate_balance_guaranteed_swap_terms{make_rate_common(), "GB0005405286"});
    BOOST_LOG_SEV(lg, info) << "Constructing the four simple rate products";

    CHECK(std::holds_alternative<callable_swap_instrument>(callable.fact));
    CHECK(std::get<knock_out_swap_instrument>(knock_out.fact).barrier_type == "UpAndOut");
    CHECK(std::get<flexi_swap_instrument>(flexi.fact).option_long_short == "Short");
    CHECK(std::get<balance_guaranteed_swap_instrument>(balance.fact).reference_security ==
          "GB0005405286");
}

TEST_CASE("a_knock_out_swap_refuses_a_barrier_type_outside_the_closed_set", "[domain]") {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Refusing a knock out swap with an unknown barrier type";
    CHECK_THROWS_AS(construct_rate_knock_out_swap(
                        rate_knock_out_swap_terms{make_rate_common(), a_rate_date(), "Sideways"}),
                    rate_product_error);
}

TEST_CASE("a_rate_product_refuses_a_leg_without_a_currency", "[domain]") {
    auto lg(make_logger(test_suite));

    auto common = make_rate_common();
    common.floating.currency.clear();
    BOOST_LOG_SEV(lg, info) << "Refusing a swap whose floating leg states no currency";

    CHECK_THROWS_AS(construct_rate_swap(rate_swap_terms{common, 2}), rate_product_error);
}
