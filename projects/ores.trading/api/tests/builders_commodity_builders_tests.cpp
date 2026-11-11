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
#include "ores.trading.api/builders/builder_registry.hpp"
#include "ores.trading.api/builders/commodity_builders.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto commodity_tags("[builders][commodity]");

decimal amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference commodity(const char* code) {
    auto index = md::market_index::make(md::index_family::commodity, code, {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

commodity_swap_terms crude_swap() {
    return {.underlying = commodity("NYMEX:CL"),
            .currency = std::string("USD"),
            .quantity = amount("1000"),
            .price = amount("75.5"),
            .start = 2026y / 1 / 5,
            .maturity = 2027y / 1 / 5,
            .fixed_side = side::pay};
}

commodity_option_terms crude_call() {
    return {.underlying = commodity("NYMEX:CL"),
            .currency = std::string("USD"),
            .quantity = amount("1000"),
            .kind = option_kind::call,
            .strike = amount("80"),
            .expiry = 2027y / 1 / 5,
            .position = option_position::short_option};
}

}

TEST_CASE("a_commodity_forward_names_its_commodity_by_address", commodity_tags) {
    const auto forward = commodity_forward_builder({.underlying = commodity("NYMEX:CL"),
                                                    .currency = std::string("USD"),
                                                    .quantity = amount("1000"),
                                                    .price = amount("75.5"),
                                                    .maturity = 2027y / 1 / 5,
                                                    .position = option_position::short_option})
                             .build();
    REQUIRE(forward.has_value());
    CHECK(forward->instrument.commodity_code == commodity("NYMEX:CL").uri());
    CHECK(forward->instrument.position == "Short");
    CHECK(forward->instrument.fixed_price == amount("75.5"));
}

TEST_CASE("a_commodity_option_settles_in_cash", commodity_tags) {
    const auto option = commodity_option_builder(crude_call()).build();
    REQUIRE(option.has_value());
    REQUIRE(option->options.size() == 1);
    CHECK(option->options[0].settlement == "Cash");
    CHECK(option->options[0].long_short == "Short");
}

TEST_CASE("a_commodity_option_with_no_strike_fails_and_names_the_term", commodity_tags) {
    auto terms = crude_call();
    terms.strike = decimal{};
    const auto option = commodity_option_builder(terms).build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "strike");
}

TEST_CASE("a_commodity_swap_states_a_fixed_and_a_floating_leg", commodity_tags) {
    const auto swap = commodity_swap_builder(crude_swap()).build();
    REQUIRE(swap.has_value());
    REQUIRE(swap->legs.size() == 2);
    CHECK(swap->legs[0].leg_type_code == "CommodityFixed");
    CHECK(swap->legs[0].payer);
    CHECK(swap->legs[1].leg_type_code == "CommodityFloating");
    CHECK(swap->legs[1].floating_index_code == commodity("NYMEX:CL").uri());
    // Each leg rolls monthly to maturity, so ORE has an end to its schedule.
    REQUIRE(swap->schedules.size() == 2);
    for (const auto& schedule : swap->schedules)
        CHECK(schedule.end_date == 2027y / 1 / 5);
    REQUIRE(swap->leg_rates.size() == 1);
    CHECK(swap->leg_rates[0].rate_role == "price");
}

TEST_CASE("a_commodity_swaption_expiring_after_the_swap_starts_fails", commodity_tags) {
    const auto swaption = commodity_swaption_builder({.swap = crude_swap(),
                                                      .expiry = 2026y / 2 / 5,
                                                      .position = option_position::long_option})
                              .build();
    REQUIRE_FALSE(swaption.has_value());
    CHECK(swaption.error().term == "expiry");
}

TEST_CASE("a_commodity_swaption_settles_in_cash", commodity_tags) {
    const auto swaption = commodity_swaption_builder({.swap = crude_swap(),
                                                      .expiry = 2025y / 12 / 22,
                                                      .position = option_position::long_option})
                              .build();
    REQUIRE(swaption.has_value());
    REQUIRE(swaption->options.size() == 1);
    CHECK(swaption->options[0].settlement == "Cash");
    CHECK(swaption->legs.size() == 2);
}

TEST_CASE("a_commodity_option_strip_states_its_strike_on_its_side", commodity_tags) {
    const auto strip = commodity_option_strip_builder({.underlying = commodity("NYMEX:NG"),
                                                       .currency = std::string("USD"),
                                                       .quantity = amount("10000"),
                                                       .kind = option_kind::put,
                                                       .strike = amount("3"),
                                                       .start = 2026y / 1 / 5,
                                                       .maturity = 2027y / 1 / 5,
                                                       .roll_tenor = std::string("1M"),
                                                       .position = option_position::long_option})
                           .build();
    REQUIRE(strip.has_value());
    REQUIRE(strip->strikes.size() == 1);
    CHECK(strip->strikes[0].strike_role == "put");
    CHECK(strip->strikes[0].long_short == "Long");
    REQUIRE(strip->schedules.size() == 1);
    CHECK(strip->schedules[0].end_date == 2027y / 1 / 5);
}

TEST_CASE("a_commodity_average_price_option_averages_to_its_expiry", commodity_tags) {
    const auto option = commodity_average_price_option_builder(
                            {.option = crude_call(), .averaging_start = 2026y / 1 / 5})
                            .build();
    REQUIRE(option.has_value());
    CHECK(option->instrument.average_type == "Arithmetic");
    CHECK(option->instrument.averaging_end_date == 2027y / 1 / 5);
}

TEST_CASE("a_commodity_variance_swap_with_no_strike_fails", commodity_tags) {
    const auto swap = commodity_variance_swap_builder({.underlying = commodity("NYMEX:CL"),
                                                       .currency = std::string("USD"),
                                                       .notional = amount("100000"),
                                                       .variance_strike = 0.0,
                                                       .start = 2026y / 1 / 5,
                                                       .maturity = 2027y / 1 / 5})
                          .build();
    REQUIRE_FALSE(swap.has_value());
    CHECK(swap.error().term == "variance_strike");
}

TEST_CASE("a_commodity_basket_option_pays_a_vanilla_payoff", commodity_tags) {
    const auto option =
        commodity_basket_option_builder(
            {.basket = std::vector<commodity_basket_member>{{commodity("NYMEX:CL"), 0.5},
                                                            {commodity("NYMEX:NG"), 0.5}},
             .currency = std::string("USD"),
             .notional = amount("1000"),
             .kind = option_kind::call,
             .strike = amount("50"),
             .expiry = 2027y / 1 / 5,
             .position = option_position::long_option})
            .build();
    REQUIRE(option.has_value());
    REQUIRE(option->constituents.size() == 2);
    CHECK(option->constituents[1].underlying_code == commodity("NYMEX:NG").uri());
    REQUIRE(option->options.size() == 1);
    CHECK(option->options[0].payoff_type == "Vanilla");
}

TEST_CASE("a_commodity_basket_holding_one_commodity_twice_fails", commodity_tags) {
    const auto option =
        commodity_basket_option_builder(
            {.basket = std::vector<commodity_basket_member>{{commodity("NYMEX:CL"), 0.5},
                                                            {commodity("NYMEX:CL"), 0.5}},
             .currency = std::string("USD"),
             .notional = amount("1000"),
             .kind = option_kind::call,
             .strike = amount("50"),
             .expiry = 2027y / 1 / 5,
             .position = option_position::long_option})
            .build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "basket");
}

TEST_CASE("the_registry_names_every_commodity_builder", commodity_tags) {
    const auto types = built_trade_types();
    for (const auto* type : {"CommodityForward",
                             "CommodityOption",
                             "CommodityAveragePriceOption",
                             "CommodityOptionStrip",
                             "CommoditySwap",
                             "CommoditySwaption",
                             "CommodityVarianceSwap",
                             "CommodityBasketOption"})
        CHECK(types.contains(type));
}
