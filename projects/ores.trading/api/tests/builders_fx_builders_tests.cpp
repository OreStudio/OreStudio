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
#include "ores.trading.api/builders/fx_builders.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto fx_tags("[builders][fx]");

decimal fx_amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference eur_usd() {
    auto index =
        md::market_index::make(md::index_family::fx, "EUR", {{"source", "TR20H"}, {"ccy", "USD"}});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

fx_option_terms fx_call() {
    return {.exchange = {.bought_currency = std::string("EUR"),
                         .bought_amount = fx_amount("1000000"),
                         .sold_currency = std::string("USD"),
                         .sold_amount = fx_amount("1100000")},
            .kind = option_kind::call,
            .expiry = 2027y / 1 / 5,
            .position = option_position::long_option};
}

template <fx_barrier_product Product>
fx_result barrier_option(std::vector<fx_barrier> barriers) {
    return fx_barrier_option_builder<Product>(
               {.option = fx_call(), .barriers = std::move(barriers), .fx_index = eur_usd()})
        .build();
}

}

TEST_CASE("an_fx_index_is_written_by_ore_as_its_source_and_pair", fx_tags) {
    CHECK(eur_usd().uri().starts_with("oresmd://fx/EUR?"));
}

TEST_CASE("an_fx_swap_reverses_on_its_far_date", fx_tags) {
    const auto swap = fx_swap_builder({.near_leg = fx_call().exchange,
                                       .near_date = 2026y / 1 / 5,
                                       .far_date = 2027y / 1 / 5,
                                       .far_bought_amount = fx_amount("1110000"),
                                       .far_sold_amount = fx_amount("1000000")})
                          .build();
    REQUIRE(swap.has_value());
    REQUIRE(swap->forwards.size() == 1);
    CHECK(swap->forwards[0].far_date == 2027y / 1 / 5);
    CHECK(swap->forwards[0].far_bought_amount == fx_amount("1110000"));
}

TEST_CASE("an_fx_option_states_its_style_on_the_option_row", fx_tags) {
    const auto option = fx_option_builder(fx_call()).build();
    REQUIRE(option.has_value());
    REQUIRE(option->options.size() == 1);
    CHECK(option->options[0].style == "European");
}

TEST_CASE("a_single_barrier_option_states_one_level", fx_tags) {
    const auto option =
        barrier_option<fx_barrier_product::barrier>({{"UpAndOut", {fx_amount("1.2")}}});
    REQUIRE(option.has_value());
    REQUIRE(option->barrier_levels.size() == 1);
    CHECK(option->barrier_options[0].upper_barrier == fx_amount("1.2"));
    CHECK(option->barrier_options[0].barrier_fixing_index == eur_usd().uri());
    CHECK(option->options[0].style == "European");

    const auto two = barrier_option<fx_barrier_product::barrier>(
        {{"UpAndOut", {fx_amount("1.2"), fx_amount("1.3")}}});
    REQUIRE_FALSE(two.has_value());
    CHECK(two.error().term == "barriers");
}

TEST_CASE("a_double_barrier_states_one_barrier_of_two_levels", fx_tags) {
    const auto option = barrier_option<fx_barrier_product::double_barrier>(
        {{"KnockOut", {fx_amount("0.9"), fx_amount("1.2")}}});
    REQUIRE(option.has_value());
    CHECK(option->barriers.size() == 1);
    CHECK(option->barrier_levels.size() == 2);
}

TEST_CASE("a_kiko_states_a_knock_in_and_a_knock_out_barrier", fx_tags) {
    const auto option = barrier_option<fx_barrier_product::kiko>(
        {{"DownAndIn", {fx_amount("0.9")}}, {"UpAndOut", {fx_amount("1.2")}}});
    REQUIRE(option.has_value());
    CHECK(option->barriers.size() == 2);

    const auto both_out = barrier_option<fx_barrier_product::kiko>(
        {{"DownAndOut", {fx_amount("0.9")}}, {"UpAndOut", {fx_amount("1.2")}}});
    REQUIRE_FALSE(both_out.has_value());
    CHECK(both_out.error().term == "barriers");
}

TEST_CASE("a_generic_barrier_observes_its_pair_as_an_underlying", fx_tags) {
    const auto option =
        fx_generic_barrier_option_builder({.option = fx_call(),
                                           .barrier = fx_barrier{"UpAndOut", {fx_amount("1.2")}},
                                           .fx_index = eur_usd(),
                                           .monitoring_start = 2026y / 1 / 5})
            .build();
    REQUIRE(option.has_value());
    REQUIRE(option->schedules.size() == 1);
    CHECK(option->schedules[0].schedule_role == "barrier_schedule");
    CHECK(option->schedules[0].end_date == 2027y / 1 / 5);
    REQUIRE(option->instrument_underlyings.size() == 1);
    CHECK(option->instrument_underlyings[0].underlying_type == "FX");
    CHECK(option->instrument_underlyings[0].name == eur_usd().uri());
    CHECK(option->options[0].payoff_type == "Vanilla");
}

TEST_CASE("a_touch_pays_when_touched_and_states_no_style", fx_tags) {
    const auto touch = fx_digital_option_builder<fx_digital_product::touch>(
                           {.foreign_currency = std::string("EUR"),
                            .domestic_currency = std::string("USD"),
                            .payoff_amount = fx_amount("100000"),
                            .kind = option_kind::call,
                            .expiry = 2027y / 1 / 5,
                            .position = option_position::long_option,
                            .strike = std::nullopt,
                            .barriers = {{"UpAndIn", {fx_amount("1.2")}}},
                            .fx_index = eur_usd()})
                           .build();
    REQUIRE(touch.has_value());
    CHECK_FALSE(touch->options[0].style.has_value());
    CHECK(touch->options[0].pay_off_at_expiry == "true");
}

TEST_CASE("a_digital_without_a_strike_fails_and_names_the_term", fx_tags) {
    const auto digital = fx_digital_option_builder<fx_digital_product::digital>(
                             {.foreign_currency = std::string("EUR"),
                              .domestic_currency = std::string("USD"),
                              .payoff_amount = fx_amount("100000"),
                              .kind = option_kind::call,
                              .expiry = 2027y / 1 / 5,
                              .position = option_position::long_option,
                              .strike = std::nullopt,
                              .barriers = {},
                              .fx_index = eur_usd()})
                             .build();
    REQUIRE_FALSE(digital.has_value());
    CHECK(digital.error().term == "strike");
}

TEST_CASE("an_average_forward_observes_monthly_until_it_pays", fx_tags) {
    const auto forward = fx_average_forward_builder({.fx_index = eur_usd(),
                                                     .reference_currency = std::string("EUR"),
                                                     .reference_notional = fx_amount("1000000"),
                                                     .settlement_currency = std::string("USD"),
                                                     .settlement_notional = fx_amount("1100000"),
                                                     .start = 2026y / 1 / 5,
                                                     .payment_date = 2027y / 1 / 5})
                             .build();
    REQUIRE(forward.has_value());
    REQUIRE(forward->schedules.size() == 1);
    CHECK(forward->schedules[0].schedule_role == "observation_schedule");
    CHECK(forward->schedules[0].end_date == 2027y / 1 / 5);
    CHECK(forward->asian_forwards[0].fx_index == eur_usd().uri());
}

TEST_CASE("a_tarf_states_its_range_bounds_and_its_target", fx_tags) {
    const auto tarf = fx_tarf_builder({.fx_index = eur_usd(),
                                       .currency = std::string("USD"),
                                       .fixing_amount = fx_amount("100000"),
                                       .strike = fx_amount("1.1"),
                                       .target_amount = fx_amount("10000"),
                                       .start = 2026y / 1 / 5,
                                       .end = 2027y / 1 / 5,
                                       .position = option_position::long_option})
                          .build();
    REQUIRE(tarf.has_value());
    CHECK(tarf->range_bounds.size() == 2);
    REQUIRE(tarf->barriers.size() == 1);
    CHECK(tarf->barriers[0].barrier_type == "CumulatedProfitCap");
    CHECK(tarf->options[0].payoff_type == "TargetExact");
}

TEST_CASE("an_accumulator_knocks_out_above_its_strike", fx_tags) {
    const auto accumulator = fx_accumulator_builder({.fx_index = eur_usd(),
                                                     .currency = std::string("USD"),
                                                     .fixing_amount = fx_amount("100000"),
                                                     .strike = fx_amount("1.1"),
                                                     .knock_out = fx_amount("1.0"),
                                                     .start = 2026y / 1 / 5,
                                                     .end = 2027y / 1 / 5,
                                                     .position = option_position::long_option})
                                 .build();
    REQUIRE_FALSE(accumulator.has_value());
    CHECK(accumulator.error().term == "knock_out");
}

TEST_CASE("the_registry_names_every_fx_builder", fx_tags) {
    const auto types = built_trade_types();
    for (const auto* type : {"FxForward",
                             "FxSwap",
                             "FxOption",
                             "FxBarrierOption",
                             "FxDoubleBarrierOption",
                             "FxEuropeanBarrierOption",
                             "FxKIKOBarrierOption",
                             "FxGenericBarrierOption",
                             "FxDigitalOption",
                             "FxTouchOption",
                             "FxDoubleTouchOption",
                             "FxDigitalBarrierOption",
                             "FxAverageForward",
                             "FxTaRF",
                             "FxVarianceSwap",
                             "FxAccumulator"})
        CHECK(types.contains(type));
}

TEST_CASE("a_touch_with_a_strike_fails_and_names_the_term", fx_tags) {
    const auto touch = fx_digital_option_builder<fx_digital_product::touch>(
                           {.foreign_currency = std::string("EUR"),
                            .domestic_currency = std::string("USD"),
                            .payoff_amount = fx_amount("100000"),
                            .kind = option_kind::call,
                            .expiry = 2027y / 1 / 5,
                            .position = option_position::long_option,
                            .strike = fx_amount("1.1"),
                            .barriers = {{"UpAndIn", {fx_amount("1.2")}}},
                            .fx_index = eur_usd()})
                           .build();
    REQUIRE_FALSE(touch.has_value());
    CHECK(touch.error().term == "strike");
}
