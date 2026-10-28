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
#include "ores.trading.api/domain/commodity_instruments.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <string>

namespace {

using namespace ores::trading::domain;

std::chrono::year_month_day a_commodity_date() {
    return std::chrono::year{2027} / std::chrono::month{3} / std::chrono::day{15};
}

decimal commodity_money(const std::string& text) {
    return *decimal::from_string(text);
}

commodity_common_terms make_common() {
    commodity_common_terms common;
    common.commodity_code = "BRENT";
    common.currency = "USD";
    common.quantity = 1000.0;
    common.unit = "BBL";
    common.start_date = a_commodity_date();
    common.maturity_date = a_commodity_date();
    return common;
}

commodity_option_terms make_option_terms() {
    commodity_option_terms terms{make_common(), "Call", commodity_money("80"), "European"};
    return terms;
}

}

using namespace ores::logging;

TEST_CASE("a_commodity_forward_carries_no_option_terms", "[domain]") {
    auto lg(make_logger(test_suite));

    commodity_forward_terms terms{make_common(), commodity_money("75")};
    const auto sut = construct_commodity_forward(terms);
    BOOST_LOG_SEV(lg, info) << "Forward type: " << sut.identity.trade_type_code;

    CHECK(sut.identity.trade_type_code == "CommodityForward");
    CHECK(sut.fixed_price->to_string() == "75");
    CHECK(sut.option_type.empty());
    CHECK_FALSE(sut.strike_price.has_value());
    CHECK(sut.barrier_type.empty());
}

TEST_CASE("the_four_forward_shaped_products_state_their_own_type", "[domain]") {
    auto lg(make_logger(test_suite));

    const commodity_forward_terms terms{make_common(), commodity_money("75")};
    BOOST_LOG_SEV(lg, info) << "Constructing the four forward-shaped products";

    CHECK(construct_commodity_forward(terms).identity.trade_type_code == "CommodityForward");
    CHECK(construct_intraday_power_forward(terms).identity.trade_type_code ==
          "IntradayPowerForward");
    CHECK(construct_commodity_forward_volatility_agreement(terms).identity.trade_type_code ==
          "CommodityForwardVolatilityAgreement");
    CHECK(construct_commodity_position(terms).identity.trade_type_code == "CommodityPosition");
}

TEST_CASE("the_seven_option_shaped_products_state_their_own_type", "[domain]") {
    auto lg(make_logger(test_suite));

    const auto terms = make_option_terms();
    BOOST_LOG_SEV(lg, info) << "Constructing the seven option-shaped products";

    CHECK(construct_commodity_option(terms).identity.trade_type_code == "CommodityOption");
    CHECK(construct_commodity_digital_option(terms).identity.trade_type_code ==
          "CommodityDigitalOption");
    CHECK(construct_commodity_asian_option(terms).identity.trade_type_code ==
          "CommodityAsianOption");
    CHECK(construct_commodity_strike_resettable_option(terms).identity.trade_type_code ==
          "CommodityStrikeResettableOption");
    CHECK(construct_commodity_basket_option(terms).identity.trade_type_code ==
          "CommodityBasketOption");
    CHECK(construct_commodity_rainbow_option(terms).identity.trade_type_code ==
          "CommodityRainbowOption");
    CHECK(construct_commodity_best_entry_option(terms).identity.trade_type_code ==
          "CommodityBestEntryOption");
}

TEST_CASE("an_option_type_outside_the_closed_set_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_option_terms();
    terms.option_type = "Straddle";
    BOOST_LOG_SEV(lg, info) << "Refusing a commodity option whose type is not Call or Put";

    CHECK_THROWS_AS(construct_commodity_option(terms), commodity_product_error);
}

TEST_CASE("an_exercise_type_outside_the_closed_set_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_option_terms();
    terms.exercise_type = "Bermudan";
    BOOST_LOG_SEV(lg, info) << "Refusing a commodity option whose exercise type is not ORE's";

    CHECK_THROWS_AS(construct_commodity_option(terms), commodity_product_error);
}

TEST_CASE("an_accumulator_and_a_target_redemption_forward_differ_only_by_type", "[domain]") {
    auto lg(make_logger(test_suite));

    commodity_accumulation_terms terms{make_common(), "Call", commodity_money("50000"), commodity_money("88")};
    const auto accumulator = construct_commodity_accumulator(terms);
    const auto tarf = construct_commodity_target_redemption_forward(terms);
    BOOST_LOG_SEV(lg, info) << "Accumulator: " << accumulator.identity.trade_type_code;

    CHECK(accumulator.identity.trade_type_code == "CommodityAccumulator");
    CHECK(tarf.identity.trade_type_code == "CommodityTaRF");
    CHECK(accumulator.accumulation_amount->to_string() ==
          tarf.accumulation_amount->to_string());
}

TEST_CASE("a_variance_swap_with_a_non_finite_strike_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    commodity_variance_swap_terms terms{make_common(), 0.0};
    terms.variance_strike = std::nan("");
    BOOST_LOG_SEV(lg, info) << "Refusing a variance swap with a NaN strike";

    CHECK_THROWS_AS(construct_commodity_variance_swap(terms), commodity_product_error);
}

TEST_CASE("a_swaption_without_an_expiry_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    commodity_swaption_terms terms{make_common(), "Call", commodity_money("80"), std::nullopt};
    BOOST_LOG_SEV(lg, info) << "Refusing a swaption with no expiry";

    CHECK_THROWS_AS(construct_commodity_swaption(terms), commodity_product_error);
}

TEST_CASE("a_zero_quantity_is_refused_for_every_shape", "[domain]") {
    auto lg(make_logger(test_suite));

    auto common = make_common();
    common.quantity = 0.0;
    BOOST_LOG_SEV(lg, info) << "Refusing a commodity product with no quantity";

    CHECK_THROWS_AS(construct_commodity_forward({common, commodity_money("75")}), commodity_product_error);
    CHECK_THROWS_AS(construct_commodity_option({common, "Call", commodity_money("80"), "European"}),
                    commodity_product_error);
    CHECK_THROWS_AS(construct_commodity_variance_swap({common, 0.05}), commodity_product_error);
}
