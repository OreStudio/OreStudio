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
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.trading.api/builders/builder_registry.hpp"
#include "ores.trading.api/builders/equity_builders.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <variant>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace conv = ores::marketdata::conventions;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto equity_tags("[builders][equity]");

decimal equity_amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference spx() {
    auto index = md::market_index::make(md::index_family::equity, ".SPX", {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

equity_option_terms spx_call() {
    return {.underlying = spx(),
            .currency = std::string("USD"),
            .quantity = equity_amount("1000"),
            .kind = option_kind::call,
            .strike = equity_amount("5000"),
            .expiry = 2027y / 1 / 5,
            .position = option_position::long_option};
}

}

TEST_CASE("an_equity_option_states_its_style_on_the_option_row", equity_tags) {
    const auto option = equity_option_builder(spx_call()).build();
    REQUIRE(option.has_value());
    REQUIRE(option->options.size() == 1);
    CHECK(option->options[0].style == "European");
    const auto* row =
        std::get_if<ores::trading::domain::equity_option_instrument>(&option->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->underlying_name == spx().uri());
}

TEST_CASE("an_equity_digital_states_its_style", equity_tags) {
    const auto digital =
        equity_digital_option_builder({.option = spx_call(), .payout = equity_amount("100000")})
            .build();
    REQUIRE(digital.has_value());
    REQUIRE(digital->options.size() == 1);
    CHECK(digital->options[0].style == "European");
}

TEST_CASE("an_equity_asian_option_observes_monthly_and_states_its_payoff", equity_tags) {
    const auto asian =
        equity_asian_option_builder({.option = spx_call(), .averaging_start = 2026y / 1 / 5})
            .build();
    REQUIRE(asian.has_value());
    REQUIRE(asian->schedules.size() == 1);
    CHECK(asian->schedules[0].schedule_role == "observation_schedule");
    CHECK(asian->options[0].payoff_type == "Asian");
}

TEST_CASE("an_equity_cliquet_resets_on_its_schedule", equity_tags) {
    const auto cliquet =
        equity_cliquet_option_builder(
            {.option = spx_call(), .start = 2026y / 1 / 5, .reset_tenor = std::string("3M")})
            .build();
    REQUIRE(cliquet.has_value());
    REQUIRE(cliquet->schedules.size() == 1);
    CHECK(cliquet->schedules[0].tenor == "3M");
}

TEST_CASE("an_equity_touch_watches_the_equity_index", equity_tags) {
    const auto touch = equity_touch_option_builder({.underlying = spx(),
                                                    .currency = std::string("USD"),
                                                    .payout = equity_amount("100000"),
                                                    .barrier_type = std::string("UpAndIn"),
                                                    .level = equity_amount("5500"),
                                                    .expiry = 2027y / 1 / 5,
                                                    .position = option_position::long_option})
                           .build();
    REQUIRE(touch.has_value());
    const auto* row =
        std::get_if<ores::trading::domain::equity_digital_option_instrument>(&touch->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->barrier_fixing_index == spx().uri());
}

TEST_CASE("a_double_barrier_with_one_level_fails_and_names_the_term", equity_tags) {
    const auto option = equity_barrier_option_builder<equity_barrier_product::double_barrier>(
                            {.option = spx_call(),
                             .barrier_type = std::string("KnockOut"),
                             .levels = std::vector<decimal>{equity_amount("4500")}})
                            .build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "barrier");
}

TEST_CASE("an_equity_accumulator_states_its_payoff_bounds_and_knock_out", equity_tags) {
    const auto accumulator = equity_accumulator_builder({.underlying = spx(),
                                                         .currency = std::string("USD"),
                                                         .fixing_amount = equity_amount("10"),
                                                         .strike = equity_amount("5000"),
                                                         .knock_out = equity_amount("5500"),
                                                         .start = 2026y / 1 / 5,
                                                         .expiry = 2027y / 1 / 5,
                                                         .position = option_position::long_option})
                                 .build();
    REQUIRE(accumulator.has_value());
    CHECK(accumulator->options[0].payoff_type == "Accumulator");
    CHECK(accumulator->range_bounds.size() == 2);
    REQUIRE(accumulator->barriers.size() == 1);
    CHECK(accumulator->barriers[0].barrier_type == "UpAndOut");
}

TEST_CASE("an_equity_tarf_states_its_range_bounds_and_its_target", equity_tags) {
    const auto tarf = equity_tarf_builder({.underlying = spx(),
                                           .currency = std::string("USD"),
                                           .fixing_amount = equity_amount("10"),
                                           .strike = equity_amount("5000"),
                                           .target_amount = equity_amount("10000"),
                                           .start = 2026y / 1 / 5,
                                           .expiry = 2027y / 1 / 5,
                                           .position = option_position::long_option})
                          .build();
    REQUIRE(tarf.has_value());
    CHECK(tarf->range_bounds.size() == 2);
    REQUIRE(tarf->barriers.size() == 1);
    CHECK(tarf->barriers[0].barrier_type == "CumulatedProfitCap");
}

TEST_CASE("a_worst_of_basket_states_its_members_as_equities", equity_tags) {
    auto other = md::market_index::make(md::index_family::equity, ".NDX", {});
    REQUIRE(other);
    auto ndx = md::market_reference::of(std::move(*other));
    REQUIRE(ndx);
    const auto basket = equity_worst_of_basket_swap_builder(
                            {.basket = std::vector<md::market_reference>{spx(), *ndx},
                             .currency = std::string("USD"),
                             .notional = equity_amount("1000000"),
                             .start = 2026y / 1 / 5,
                             .maturity = 2027y / 1 / 5,
                             .position = option_position::long_option})
                            .build();
    REQUIRE(basket.has_value());
    REQUIRE(basket->basket_constituents.size() == 2);
    for (const auto& member : basket->basket_constituents)
        CHECK(member.underlying_kind == "Equity");
}

TEST_CASE("an_equity_swap_pays_the_equity_against_a_floating_leg", equity_tags) {
    ores::refdata::domain::ibor_index_convention libor;
    libor.id = "USD-LIBOR";
    libor.fixing_calendar = "US";
    libor.settlement_days = 2;
    libor.day_count_fraction = "A360";
    libor.business_day_convention = "MF";
    libor.oresmd_uri = "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR";
    ores::refdata::domain::swap_convention swap;
    swap.id = "USD-3M-SWAP";
    swap.fixed_calendar = "US";
    swap.fixed_frequency = "Semiannual";
    swap.fixed_convention = "MF";
    swap.fixed_day_count_fraction = "30/360";
    swap.index = "USD-LIBOR-3M";
    swap.oresmd_uri = "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M";
    const conv::convention_lookup lookup(
        conv::convention_set{.ibor_indices = {libor}, .swaps = {swap}});
    auto index = md::market_reference::parse(*swap.oresmd_uri);
    REQUIRE(index);
    const auto built = equity_swap_builder(lookup,
                                           {.underlying = spx(),
                                            .quantity = 200.0,
                                            .funding = {.notional = equity_amount("1000000"),
                                                        .currency = std::string("USD"),
                                                        .start = 2026y / 1 / 5,
                                                        .maturity = 2027y / 1 / 5,
                                                        .fixed_rate = decimal{},
                                                        .index = *index,
                                                        .fixed_side = side::pay}})
                           .build();
    REQUIRE(built.has_value());
    REQUIRE(built->legs.size() == 2);
    CHECK(built->legs[0].leg_type_code == "Floating");
    CHECK(built->legs[1].leg_type_code == "Equity");
    CHECK(built->legs[1].equity_name == spx().uri());
    CHECK(built->schedules.size() == 2);
}

TEST_CASE("the_registry_names_every_equity_builder", equity_tags) {
    const auto types = built_trade_types();
    for (const auto* type : {"EquityOption",
                             "EquityCliquetOption",
                             "EquityAsianOption",
                             "EquityBarrierOption",
                             "EquityEuropeanBarrierOption",
                             "EquityDoubleBarrierOption",
                             "EquityDigitalOption",
                             "EquityTouchOption",
                             "EquityForward",
                             "EquitySwap",
                             "EquityWorstOfBasketSwap",
                             "EquityVarianceSwap",
                             "EquityAccumulator",
                             "EquityTaRF",
                             "EquityPosition",
                             "EquityOptionPosition"})
        CHECK(types.contains(type));
}
