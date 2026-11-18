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
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.trading.api/builders/basket_option_builders.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <variant>
#include <vector>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace md = ores::marketdata::datum;
namespace domain = ores::trading::domain;
using ores::utility::decimal::decimal;

namespace {

const auto basket_tags("[builders][basket]");

decimal basket_amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference basket_equity(const char* code) {
    auto index = md::market_index::make(md::index_family::equity, code, {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

basket_option_terms spx_and_ndx(std::string payoff_type) {
    return {.currency = std::string("USD"),
            .notional = basket_amount("1"),
            .strike = basket_amount("20000"),
            .members = std::vector<basket_option_member>{{.underlying = basket_equity(".SPX"),
                                                          .currency = std::nullopt,
                                                          .weight = basket_amount("1")},
                                                         {.underlying = basket_equity(".NDX"),
                                                          .currency = std::nullopt,
                                                          .weight = basket_amount("0.5")}},
            .payoff_type = std::move(payoff_type),
            .kind = option_kind::call,
            .position = option_position::long_option,
            .expiry = 2027y / 1 / 5,
            .settlement = 2027y / 1 / 7};
}

}

TEST_CASE("a_basket_option_states_its_terms_on_the_equity_row", basket_tags) {
    const auto built =
        basket_option_builder<basket_product::equity_basket>(spx_and_ndx("Vanilla")).build();
    REQUIRE(built.has_value());
    const auto* row = std::get_if<domain::equity_basket_option_instrument>(&built->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->identity.trade_type_code == "EquityBasketOption");
    CHECK(row->currency == "USD");
    CHECK(row->notional == basket_amount("1"));
    CHECK(row->strike == basket_amount("20000"));
    CHECK(row->settlement_date == std::optional(2027y / 1 / 7));
}

TEST_CASE("a_basket_option_states_its_members_its_option_block_and_its_exercise_date",
          basket_tags) {
    const auto built =
        basket_option_builder<basket_product::equity_basket>(spx_and_ndx("Vanilla")).build();
    REQUIRE(built.has_value());
    REQUIRE(built->instrument_underlyings.size() == 2);
    CHECK(built->instrument_underlyings[0].underlying_type == "Equity");
    CHECK(built->instrument_underlyings[0].underlying_role == "underlying");
    CHECK(built->instrument_underlyings[0].sequence_number == 1);
    CHECK(built->instrument_underlyings[1].sequence_number == 2);
    CHECK(built->instrument_underlyings[1].weight == 0.5);
    REQUIRE(built->options.size() == 1);
    CHECK(built->options[0].payoff_type == "Vanilla");
    CHECK(built->options[0].option_type == "Call");
    REQUIRE(built->schedule_dates.size() == 1);
    CHECK(built->schedule_dates[0].schedule_role == "exercise_dates");
    CHECK(built->schedule_dates[0].schedule_date == 2027y / 1 / 5);
}

TEST_CASE("a_rainbow_on_a_best_of_payoff_states_no_option_type", basket_tags) {
    const auto built =
        basket_option_builder<basket_product::fx_rainbow>(spx_and_ndx("BestOfAssetOrCash")).build();
    REQUIRE(built.has_value());
    REQUIRE(built->basket_options.size() == 1);
    CHECK(built->basket_options[0].identity.trade_type_code == "FxRainbowOption");
    CHECK(built->basket_options[0].strike == basket_amount("20000"));
    CHECK(built->instrument_underlyings[0].underlying_type == "FX");
    REQUIRE(built->options.size() == 1);
    CHECK_FALSE(built->options[0].option_type.has_value());
}

TEST_CASE("a_basket_option_refuses_a_payoff_of_the_other_product", basket_tags) {
    const auto basket =
        basket_option_builder<basket_product::equity_basket>(spx_and_ndx("MaxRainbow")).build();
    REQUIRE_FALSE(basket.has_value());
    CHECK(basket.error().term == "payoff_type");
    const auto rainbow =
        basket_option_builder<basket_product::equity_rainbow>(spx_and_ndx("Vanilla")).build();
    REQUIRE_FALSE(rainbow.has_value());
    CHECK(rainbow.error().term == "payoff_type");
}

TEST_CASE("a_basket_option_refuses_one_member_and_a_member_named_twice", basket_tags) {
    auto terms = spx_and_ndx("Vanilla");
    auto members = terms.members.get();
    members.pop_back();
    terms.members = members;
    auto built = basket_option_builder<basket_product::equity_basket>(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "members");

    terms = spx_and_ndx("Vanilla");
    members = terms.members.get();
    members[1].underlying = basket_equity(".SPX");
    terms.members = members;
    built = basket_option_builder<basket_product::equity_basket>(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "members");
}
