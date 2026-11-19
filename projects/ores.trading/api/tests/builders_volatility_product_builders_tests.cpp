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
#include "ores.trading.api/builders/volatility_product_builders.hpp"
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

const auto volatility_tags("[builders][volatility]");

decimal volatility_amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference volatility_equity(const char* code) {
    auto index = md::market_index::make(md::index_family::equity, code, {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

basket_variance_swap_terms spx_and_ndx_variance() {
    return {.members = std::vector<basket_option_member>{{.underlying = volatility_equity(".SPX"),
                                                          .currency = std::nullopt,
                                                          .weight = volatility_amount("0.5")},
                                                         {.underlying = volatility_equity(".NDX"),
                                                          .currency = std::nullopt,
                                                          .weight = volatility_amount("0.5")}},
            .currency = std::string("USD"),
            .notional = volatility_amount("1000"),
            .strike = volatility_amount("0.2"),
            .squared_payoff = false,
            .start = 2026y / 1 / 5,
            .end = 2027y / 1 / 5,
            .settlement = 2027y / 1 / 7,
            .position = option_position::short_option};
}

forward_volatility_agreement_terms spx_fva() {
    return {.underlying = volatility_equity(".SPX"),
            .pay_currency = std::string("USD"),
            .quantity = volatility_amount("1000"),
            .implied_vol_strike = volatility_amount("0.2"),
            .fva_date = 2026y / 7 / 6,
            .option_expiry = 2027y / 1 / 5,
            .premium_date = 2026y / 7 / 8,
            .settlement = 2027y / 1 / 7,
            .position = option_position::long_option};
}

}

TEST_CASE("a_basket_variance_swap_states_its_terms_on_the_equity_row", volatility_tags) {
    const auto built = equity_basket_variance_swap_builder(spx_and_ndx_variance()).build();
    REQUIRE(built.has_value());
    const auto* row =
        std::get_if<domain::equity_basket_variance_swap_instrument>(&built->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->identity.trade_type_code == "EquityBasketVarianceSwap");
    CHECK(row->long_short == "Short");
    CHECK(row->strike == volatility_amount("0.2"));
    CHECK(row->notional == volatility_amount("1000"));
    CHECK_FALSE(row->squared_payoff);
    CHECK(row->settlement_date == 2027y / 1 / 7);
    CHECK(row->currency == "USD");
    CHECK_FALSE(row->cap.has_value());
}

TEST_CASE("a_basket_variance_swap_states_its_weighted_members_and_its_valuation_schedule",
          volatility_tags) {
    const auto built = fx_basket_variance_swap_builder(spx_and_ndx_variance()).build();
    REQUIRE(built.has_value());
    REQUIRE(built->basket_variance_swaps.size() == 1);
    CHECK(built->basket_variance_swaps[0].identity.trade_type_code == "FxBasketVarianceSwap");
    REQUIRE(built->instrument_underlyings.size() == 2);
    CHECK(built->instrument_underlyings[0].underlying_type == "FX");
    CHECK(built->instrument_underlyings[1].weight == 0.5);
    REQUIRE(built->schedules.size() == 1);
    CHECK(built->schedules[0].schedule_role == "valuation_schedule");
    CHECK(built->schedules[0].tenor == "1D");
}

TEST_CASE("a_basket_variance_swap_refuses_a_member_named_twice", volatility_tags) {
    auto terms = spx_and_ndx_variance();
    auto members = terms.members.get();
    members[1].underlying = volatility_equity(".SPX");
    terms.members = members;
    const auto built = equity_basket_variance_swap_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "members");
}

TEST_CASE("a_forward_volatility_agreement_states_its_dates_and_its_strike", volatility_tags) {
    const auto built = equity_forward_volatility_agreement_builder(spx_fva()).build();
    REQUIRE(built.has_value());
    const auto* row =
        std::get_if<domain::equity_forward_volatility_agreement_instrument>(&built->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->identity.trade_type_code == "EquityForwardVolatilityAgreement");
    CHECK(row->fva_date == 2026y / 7 / 6);
    CHECK(row->option_expiry == 2027y / 1 / 5);
    CHECK(row->premium_date == 2026y / 7 / 8);
    CHECK(row->long_short == "Long");
    CHECK(row->implied_vol_strike == 0.2);
    CHECK(row->quantity == 1000.0);
    CHECK(row->pay_currency == "USD");
    REQUIRE(built->instrument_underlyings.size() == 1);
    CHECK(built->instrument_underlyings[0].underlying_role == "underlying");
}

TEST_CASE("a_forward_volatility_agreement_refuses_an_expiry_before_its_fva_date", volatility_tags) {
    auto terms = spx_fva();
    terms.option_expiry = 2026y / 7 / 1;
    const auto built = fx_forward_volatility_agreement_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "option_expiry");
}
