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

md::market_reference libor_3m() {
    auto parsed =
        md::market_reference::parse("oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M");
    REQUIRE(parsed);
    return *parsed;
}

/// Conventions defining the LIBOR index a basket funds on; static, as the builder keeps a
/// reference.
const conv::convention_lookup& basket_conventions() {
    static const conv::convention_lookup lookup = [] {
        ores::refdata::domain::ibor_index_convention libor;
        libor.id = "USD-LIBOR";
        libor.fixing_calendar = "US";
        libor.settlement_days = 2;
        libor.day_count_fraction = "A360";
        libor.business_day_convention = "MF";
        libor.oresmd_uri = "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR";
        return conv::convention_lookup(conv::convention_set{.ibor_indices = {libor}});
    }();
    return lookup;
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

namespace {

md::market_reference ndx() {
    auto index = md::market_index::make(md::index_family::equity, ".NDX", {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

equity_outperformance_option_terms spx_over_ndx() {
    return {.underlying_1 = spx(),
            .underlying_2 = ndx(),
            .currency = std::string("USD"),
            .notional = equity_amount("500000"),
            .kind = option_kind::call,
            .expiry = 2027y / 1 / 5,
            .position = option_position::long_option,
            .initial_price_1 = equity_amount("5000"),
            .initial_price_2 = equity_amount("17000"),
            .strike_return = equity_amount("0.01"),
            .knock_in_price = equity_amount("5200"),
            .knock_out_price = std::nullopt};
}

}

TEST_CASE("an_outperformance_option_states_each_equity_in_its_role_and_its_prices", equity_tags) {
    const auto built = equity_outperformance_option_builder(spx_over_ndx()).build();
    REQUIRE(built.has_value());
    const auto* row =
        std::get_if<ores::trading::domain::equity_option_instrument>(&built->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->identity.trade_type_code == "EquityOutperformanceOption");
    CHECK(row->underlying_name == spx().uri());
    CHECK(row->strike == equity_amount("0.01"));
    CHECK(row->initial_price_1 == equity_amount("5000"));
    CHECK(row->initial_price_2 == equity_amount("17000"));
    CHECK(row->knock_in_price == equity_amount("5200"));
    CHECK_FALSE(row->knock_out_price.has_value());
    REQUIRE(built->instrument_underlyings.size() == 2);
    CHECK(built->instrument_underlyings[0].underlying_role == "underlying1");
    CHECK(built->instrument_underlyings[0].name == spx().uri());
    CHECK(built->instrument_underlyings[1].underlying_role == "underlying2");
    CHECK(built->instrument_underlyings[1].name == ndx().uri());
}

TEST_CASE("an_outperformance_option_refuses_an_equity_against_itself", equity_tags) {
    auto terms = spx_over_ndx();
    terms.underlying_2 = spx();
    const auto built = equity_outperformance_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "underlying_2");
}

TEST_CASE("an_outperformance_option_refuses_a_price_that_is_not_positive", equity_tags) {
    auto terms = spx_over_ndx();
    terms.initial_price_2 = equity_amount("0");
    auto built = equity_outperformance_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "initial_price");

    terms = spx_over_ndx();
    terms.knock_in_price = equity_amount("-1");
    built = equity_outperformance_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "knock_price");
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
    const auto basket =
        equity_worst_of_basket_swap_builder(
            basket_conventions(),
            {.basket = std::vector<equity_basket_member>{{spx(), equity_amount("5000")},
                                                         {*ndx, equity_amount("18000")}},
             .observation_dates = std::vector<year_month_day>{2026y / 4 / 5},
             .funding_index = libor_3m(),
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
    // The store resolves a member by its catalogue code.
    CHECK(basket->basket_constituents[0].underlying_name == ".SPX");
    CHECK(basket->periods.size() == 1);
}

TEST_CASE("a_worst_of_basket_schedule_holds_one_more_date_than_its_periods", equity_tags) {
    auto other = md::market_index::make(md::index_family::equity, ".NDX", {});
    REQUIRE(other);
    auto ndx = md::market_reference::of(std::move(*other));
    REQUIRE(ndx);
    const auto basket =
        equity_worst_of_basket_swap_builder(
            basket_conventions(),
            {.basket = std::vector<equity_basket_member>{{spx(), equity_amount("5000")},
                                                         {*ndx, equity_amount("18000")}},
             .observation_dates = std::vector<year_month_day>{2026y / 7 / 5, 2027y / 1 / 5},
             .funding_index = libor_3m(),
             .currency = std::string("USD"),
             .notional = equity_amount("1000000"),
             .start = 2026y / 1 / 5,
             .maturity = 2027y / 1 / 5,
             .position = option_position::long_option})
            .build();
    REQUIRE(basket.has_value());
    CHECK(basket->periods.size() == 2);
    for (const auto* role : {"floating_period_schedule", "floating_pay_schedule"}) {
        std::vector<year_month_day> dates;
        for (const auto& date : basket->schedule_dates)
            if (date.schedule_role == role)
                dates.push_back(date.schedule_date);
        CHECK(dates == std::vector<year_month_day>{2026y / 1 / 5, 2026y / 7 / 5, 2027y / 1 / 5});
    }
}

std::expected<ores::trading::domain::equity_instrument_data, build_error>
basket_observing(std::vector<year_month_day> observations, md::market_reference funding) {
    auto other = md::market_index::make(md::index_family::equity, ".NDX", {});
    REQUIRE(other);
    auto ndx = md::market_reference::of(std::move(*other));
    REQUIRE(ndx);
    return equity_worst_of_basket_swap_builder(
               basket_conventions(),
               {.basket = std::vector<equity_basket_member>{{spx(), equity_amount("5000")},
                                                            {*ndx, equity_amount("18000")}},
                .observation_dates = std::move(observations),
                .funding_index = std::move(funding),
                .currency = std::string("USD"),
                .notional = equity_amount("1000000"),
                .start = 2026y / 1 / 5,
                .maturity = 2027y / 1 / 5,
                .position = option_position::long_option})
        .build();
}

TEST_CASE("a_worst_of_basket_observing_on_its_start_fails", equity_tags) {
    const auto basket = basket_observing({2026y / 1 / 5, 2027y / 1 / 5}, libor_3m());
    REQUIRE_FALSE(basket.has_value());
    CHECK(basket.error().term == "observation_dates");
}

TEST_CASE("a_worst_of_basket_observing_out_of_order_fails", equity_tags) {
    const auto basket = basket_observing({2026y / 7 / 5, 2026y / 4 / 5}, libor_3m());
    REQUIRE_FALSE(basket.has_value());
    CHECK(basket.error().term == "observation_dates");
}

TEST_CASE("a_worst_of_basket_funding_in_another_currency_fails", equity_tags) {
    auto euribor =
        md::market_reference::parse("oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=3M");
    REQUIRE(euribor);
    const auto basket = basket_observing({2027y / 1 / 5}, *euribor);
    REQUIRE_FALSE(basket.has_value());
    CHECK(basket.error().term == "funding_index");
}

TEST_CASE("a_worst_of_basket_accrues_as_its_funding_index_defines", equity_tags) {
    const auto basket = basket_observing({2027y / 1 / 5}, libor_3m());
    REQUIRE(basket.has_value());
    const auto* row =
        std::get_if<ores::trading::domain::equity_swap_instrument>(&basket->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->floating_day_count_fraction == "A360");
    for (const auto& schedule : basket->schedules)
        CHECK(schedule.calendar == "US");
}

TEST_CASE("a_worst_of_basket_observing_after_maturity_fails", equity_tags) {
    auto other = md::market_index::make(md::index_family::equity, ".NDX", {});
    REQUIRE(other);
    auto ndx = md::market_reference::of(std::move(*other));
    REQUIRE(ndx);
    const auto basket =
        equity_worst_of_basket_swap_builder(
            basket_conventions(),
            {.basket = std::vector<equity_basket_member>{{spx(), equity_amount("5000")},
                                                         {*ndx, equity_amount("18000")}},
             .observation_dates = std::vector<year_month_day>{2027y / 4 / 5},
             .funding_index = libor_3m(),
             .currency = std::string("USD"),
             .notional = equity_amount("1000000"),
             .start = 2026y / 1 / 5,
             .maturity = 2027y / 1 / 5,
             .position = option_position::long_option})
            .build();
    REQUIRE_FALSE(basket.has_value());
    CHECK(basket.error().term == "observation_dates");
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
                                            .position = option_position::long_option,
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
    // A long swap receives the equity's return and pays the funding.
    CHECK(built->legs[0].payer == true);
    CHECK(built->legs[1].payer == false);
    CHECK(std::get<ores::trading::domain::equity_swap_instrument>(built->instrument).long_short ==
          "Long");
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

TEST_CASE("a_basket_holding_one_equity_twice_fails_and_names_the_term", equity_tags) {
    const auto basket =
        equity_worst_of_basket_swap_builder(
            basket_conventions(),
            {.basket = std::vector<equity_basket_member>{{spx(), equity_amount("5000")},
                                                         {spx(), equity_amount("5000")}},
             .observation_dates = std::vector<year_month_day>{2026y / 4 / 5},
             .funding_index = libor_3m(),
             .currency = std::string("USD"),
             .notional = equity_amount("1000000"),
             .start = 2026y / 1 / 5,
             .maturity = 2027y / 1 / 5,
             .position = option_position::long_option})
            .build();
    REQUIRE_FALSE(basket.has_value());
    CHECK(basket.error().term == "basket");
}

TEST_CASE("a_double_barrier_with_its_levels_reversed_fails", equity_tags) {
    const auto option =
        equity_barrier_option_builder<equity_barrier_product::double_barrier>(
            {.option = spx_call(),
             .barrier_type = std::string("KnockOut"),
             .levels = std::vector<decimal>{equity_amount("5500"), equity_amount("4500")}})
            .build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "barrier");
}

TEST_CASE("a_barrier_at_zero_fails", equity_tags) {
    const auto option = equity_barrier_option_builder<equity_barrier_product::barrier>(
                            {.option = spx_call(),
                             .barrier_type = std::string("DownAndOut"),
                             .levels = std::vector<decimal>{decimal{}}})
                            .build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "barrier");
}

TEST_CASE("an_equity_double_touch_states_both_levels_as_one_barrier", equity_tags) {
    const auto touch =
        equity_double_touch_option_builder({.underlying = spx(),
                                            .currency = std::string("USD"),
                                            .payout = equity_amount("100000"),
                                            .barrier_type = std::string("KnockIn"),
                                            .lower = equity_amount("4500"),
                                            .upper = equity_amount("5500"),
                                            .expiry = 2027y / 1 / 5,
                                            .position = option_position::long_option})
            .build();
    REQUIRE(touch.has_value());
    REQUIRE(touch->barriers.size() == 1);
    CHECK(touch->barriers[0].barrier_type == "KnockIn");
    REQUIRE(touch->barrier_levels.size() == 2);
    CHECK(touch->barrier_levels[0].level == equity_amount("4500"));
    CHECK(touch->barrier_levels[1].level == equity_amount("5500"));
}

TEST_CASE("an_equity_double_touch_refuses_levels_out_of_order", equity_tags) {
    const auto touch =
        equity_double_touch_option_builder({.underlying = spx(),
                                            .currency = std::string("USD"),
                                            .payout = equity_amount("100000"),
                                            .barrier_type = std::string("KnockOut"),
                                            .lower = equity_amount("5500"),
                                            .upper = equity_amount("4500"),
                                            .expiry = 2027y / 1 / 5,
                                            .position = option_position::long_option})
            .build();
    REQUIRE_FALSE(touch.has_value());
    CHECK(touch.error().term == "barrier");
}

TEST_CASE("an_auto_delta_hedged_option_states_its_hedge_and_one_row_a_batch", equity_tags) {
    const auto batch = [](option_kind kind) {
        return delta_hedged_batch{.underlying = spx(),
                                  .currency = "USD",
                                  .strike = equity_amount("4000"),
                                  .quantity = equity_amount("100"),
                                  .kind = kind,
                                  .position = option_position::long_option,
                                  .expiry = 2027y / 1 / 5,
                                  .premium = equity_amount("9.36"),
                                  .premium_date = 2026y / 1 / 5};
    };
    const auto hedged = equity_auto_delta_hedged_option_builder(
                            {.volatility = equity_amount("0.2"),
                             .drift_rate = equity_amount("0"),
                             .observation_start = 2026y / 1 / 5,
                             .payment_date = 2027y / 1 / 12,
                             .batches = std::vector<delta_hedged_batch>{batch(option_kind::call),
                                                                        batch(option_kind::put)}})
                            .build();
    REQUIRE(hedged.has_value());
    const auto* row =
        std::get_if<ores::trading::domain::equity_position_instrument>(&hedged->instrument);
    REQUIRE(row != nullptr);
    REQUIRE(row->volatility.has_value());
    CHECK(*row->volatility == 0.2);
    CHECK(row->payment_date == std::optional(2027y / 1 / 12));
    REQUIRE(hedged->underlyings.size() == 2);
    CHECK(hedged->underlyings[1].option_type == "Put");
    CHECK(hedged->underlyings[1].quantity == 100.0);
    CHECK(hedged->underlyings[1].currency == std::optional<std::string>("USD"));
    CHECK(hedged->underlyings[1].premium_amount == equity_amount("9.36"));
}

TEST_CASE("an_auto_delta_hedged_option_refuses_a_batch_that_expires_after_the_payment",
          equity_tags) {
    const auto hedged =
        equity_auto_delta_hedged_option_builder(
            {.volatility = equity_amount("0.2"),
             .drift_rate = equity_amount("0"),
             .observation_start = 2026y / 1 / 5,
             .payment_date = 2026y / 6 / 1,
             .batches = std::vector<delta_hedged_batch>{{.underlying = spx(),
                                                         .currency = "USD",
                                                         .strike = equity_amount("4000"),
                                                         .quantity = equity_amount("100"),
                                                         .kind = option_kind::call,
                                                         .position = option_position::long_option,
                                                         .expiry = 2027y / 1 / 5,
                                                         .premium = equity_amount("9.36"),
                                                         .premium_date = 2026y / 1 / 5}}})
            .build();
    REQUIRE_FALSE(hedged.has_value());
    CHECK(hedged.error().term == "batches");
}

TEST_CASE("an_equity_future_option_is_the_option_row_with_the_future_expiry", equity_tags) {
    const equity_option_terms option{.underlying = spx(),
                                     .currency = std::string("USD"),
                                     .quantity = equity_amount("17000"),
                                     .kind = option_kind::call,
                                     .strike = equity_amount("2147.56"),
                                     .expiry = 2027y / 1 / 5,
                                     .position = option_position::long_option};
    const auto built =
        equity_future_option_builder({.option = option, .future_expiry = 2027y / 3 / 19}).build();
    REQUIRE(built.has_value());
    const auto* row =
        std::get_if<ores::trading::domain::equity_option_instrument>(&built->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->identity.trade_type_code == "EquityFutureOption");
    CHECK(row->future_expiry_date == std::optional(2027y / 3 / 19));

    const auto early =
        equity_future_option_builder({.option = option, .future_expiry = 2026y / 12 / 18}).build();
    REQUIRE_FALSE(early.has_value());
    CHECK(early.error().term == "future_expiry");
}

TEST_CASE("a_cash_position_is_its_row_long_or_short_and_refuses_none", equity_tags) {
    const auto short_cash =
        cash_position_builder({.currency = std::string("EUR"), .amount = equity_amount("-1000000")})
            .build();
    REQUIRE(short_cash.has_value());
    const auto* row =
        std::get_if<ores::trading::domain::cash_position_instrument>(&short_cash->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->identity.trade_type_code == "CashPosition");
    CHECK(row->currency == "EUR");
    CHECK(row->amount == equity_amount("-1000000"));

    const auto none =
        cash_position_builder({.currency = std::string("EUR"), .amount = equity_amount("0")})
            .build();
    REQUIRE_FALSE(none.has_value());
    CHECK(none.error().term == "amount");

    const auto unnamed =
        cash_position_builder({.currency = std::string(), .amount = equity_amount("100")}).build();
    REQUIRE_FALSE(unnamed.has_value());
    CHECK(unnamed.error().term == "currency");
}

TEST_CASE("an_equity_pairwise_variance_swap_states_both_members_and_a_daily_schedule",
          equity_tags) {
    pairwise_variance_swap_terms terms{.first = pairwise_member{.underlying = spx(),
                                                                .strike = equity_amount("0.34"),
                                                                .notional = equity_amount("577")},
                                       .second = pairwise_member{.underlying = ndx(),
                                                                 .strike = equity_amount("0.39"),
                                                                 .notional = equity_amount("603")},
                                       .basket_notional = equity_amount("1339"),
                                       .basket_strike = equity_amount("0.32"),
                                       .pay_currency = std::string("USD"),
                                       .start = 2026y / 1 / 5,
                                       .end = 2027y / 1 / 5,
                                       .settlement = 2027y / 1 / 7,
                                       .position = option_position::long_option};
    const auto built = equity_pairwise_variance_swap_builder(terms).build();
    REQUIRE(built.has_value());
    const auto* row = std::get_if<ores::trading::domain::equity_pairwise_variance_swap_instrument>(
        &built->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->first_underlying == spx().uri());
    CHECK(row->second_underlying == ndx().uri());
    CHECK(row->second_strike == equity_amount("0.39"));
    CHECK(row->basket_notional == equity_amount("1339"));
    CHECK(row->long_short == "Long");
    CHECK(row->settlement_date == 2027y / 1 / 7);
    REQUIRE(built->schedules.size() == 1);
    CHECK(built->schedules[0].schedule_role == "valuation_schedule");
    CHECK(built->schedules[0].tenor == "1D");

    auto twice = terms;
    auto second = twice.second.get();
    second.underlying = spx();
    twice.second = second;
    const auto refused = equity_pairwise_variance_swap_builder(twice).build();
    REQUIRE_FALSE(refused.has_value());
    CHECK(refused.error().term == "second");
}
