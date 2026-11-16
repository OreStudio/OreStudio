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
#include "ores.marketdata.api/conventions/convention_lookup.hpp"
#include "ores.trading.api/builders/composite_builders.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace conv = ores::marketdata::conventions;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto composite_tags("[builders][composite]");
constexpr auto composite_euribor_6m =
    "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M";

conv::convention_set composite_conventions() {
    ores::refdata::domain::ibor_index_convention index;
    index.id = "EUR-EURIBOR";
    index.fixing_calendar = "TARGET";
    index.settlement_days = 2;
    index.day_count_fraction = "A360";
    index.business_day_convention = "MF";
    index.oresmd_uri = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR";
    ores::refdata::domain::swap_convention swap;
    swap.id = "EUR-6M-SWAP";
    swap.fixed_calendar = "TARGET";
    swap.fixed_frequency = "Annual";
    swap.fixed_convention = "MF";
    swap.fixed_day_count_fraction = "30/360";
    swap.index = "EUR-EURIBOR-6M";
    swap.oresmd_uri = composite_euribor_6m;
    return {.ibor_indices = {index}, .swaps = {swap}};
}

total_return_terms returns(side who = side::receive) {
    return {.return_side = who,
            .currency = std::string("EUR"),
            .start = 2026y / 1 / 5,
            .end = 2027y / 1 / 5,
            .initial_price = *decimal::from_string("4200")};
}

md::market_reference composite_index() {
    auto parsed = md::market_reference::parse(composite_euribor_6m);
    REQUIRE(parsed);
    return *parsed;
}

}

TEST_CASE("a_composite_trade_states_its_currency_and_notional_calculation", composite_tags) {
    const auto trade = composite_trade_builder({.currency = std::string("USD"),
                                                .notional_calculation = std::string("Mean")})
                           .build();
    REQUIRE(trade.has_value());
    CHECK(trade->instrument.identity.trade_type_code == "CompositeTrade");
    CHECK(trade->instrument.currency == std::optional<std::string>("USD"));
    CHECK(trade->instrument.notional_calculation == std::optional<std::string>("Mean"));
    CHECK(trade->legs.empty());
}

TEST_CASE("a_contract_for_difference_states_its_return_leg_alone", composite_tags) {
    const auto cfd = contract_for_difference_builder(returns(side::pay)).build();
    REQUIRE(cfd.has_value());
    CHECK(cfd->instrument.identity.trade_type_code == "ContractForDifference");
    CHECK(cfd->instrument.return_payer == std::optional<std::string>("true"));
    CHECK(cfd->instrument.return_initial_price == *decimal::from_string("4200"));
    REQUIRE(cfd->schedules.size() == 1);
    CHECK(cfd->schedules[0].owner_role == "trs");
    CHECK(cfd->schedules[0].schedule_role == "return_schedule");
    CHECK(cfd->legs.empty());
}

TEST_CASE("a_total_return_swap_funds_the_return_on_the_other_side", composite_tags) {
    conv::convention_lookup lookup(composite_conventions());
    const auto trs =
        total_return_swap_builder(lookup,
                                  {.return_leg = returns(side::receive),
                                   .funding_notional = *decimal::from_string("4200000"),
                                   .funding_index = composite_index()})
            .build();
    REQUIRE(trs.has_value());
    CHECK(trs->instrument.return_payer == std::optional<std::string>("false"));
    REQUIRE(trs->legs.size() == 1);
    CHECK(trs->legs[0].identity.leg_number == 1);
    CHECK(trs->legs[0].leg_type_code == "Floating");
    CHECK(trs->legs[0].payer == true);
    CHECK(trs->legs[0].floating_index_code == composite_euribor_6m);
    REQUIRE(trs->schedules.size() == 2);
    CHECK(trs->schedules[1].owner_role == "swap");
    CHECK(trs->schedules[1].owner_number == 1);
}

TEST_CASE("a_return_leg_that_ends_before_it_starts_fails", composite_tags) {
    auto terms = returns();
    terms.end = 2025y / 1 / 5;
    const auto cfd = contract_for_difference_builder(terms).build();
    REQUIRE_FALSE(cfd.has_value());
    CHECK(cfd.error().term == "end");
}

TEST_CASE("a_return_leg_with_no_initial_price_fails", composite_tags) {
    auto terms = returns();
    terms.initial_price = decimal{};
    const auto cfd = contract_for_difference_builder(terms).build();
    REQUIRE_FALSE(cfd.has_value());
    CHECK(cfd.error().term == "initial_price");
}
