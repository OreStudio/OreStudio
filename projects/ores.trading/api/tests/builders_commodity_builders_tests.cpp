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

commodity_digital_option_terms crude_digital() {
    return {.underlying = commodity("NYMEX:CL"),
            .currency = std::string("USD"),
            .payoff = amount("5000"),
            .kind = option_kind::call,
            .strike = amount("80"),
            .expiry = 2027y / 1 / 5,
            .position = option_position::long_option};
}

commodity_spread_option_terms crude_gas_spread() {
    return {.long_underlying = commodity("NYMEX:CL"),
            .short_underlying = commodity("NYMEX:NG"),
            .currency = std::string("USD"),
            .quantity = amount("1000"),
            .kind = option_kind::call,
            .spread_strike = amount("-2.5"),
            .start = 2026y / 1 / 5,
            .maturity = 2027y / 1 / 5,
            .position = option_position::long_option};
}

intraday_power_forward_terms power_hour() {
    return {.underlying = commodity("PJM_WH_RT_PK"),
            .currency = std::string("USD"),
            .quantity = amount("50"),
            .price = amount("32.9"),
            .maturity = 2027y / 1 / 5,
            .delivery_date = 2027y / 1 / 12,
            .delivery_start = 3600,
            .delivery_end = 7200,
            .position = option_position::short_option};
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
    CHECK(swap->legs[0].payer == true);
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

TEST_CASE("a_short_commodity_option_strip_is_short_on_its_strike_not_its_leg", commodity_tags) {
    const auto strip = commodity_option_strip_builder({.underlying = commodity("NYMEX:NG"),
                                                       .currency = std::string("USD"),
                                                       .quantity = amount("10000"),
                                                       .kind = option_kind::call,
                                                       .strike = amount("3"),
                                                       .start = 2026y / 1 / 5,
                                                       .maturity = 2027y / 1 / 5,
                                                       .roll_tenor = std::string("1M"),
                                                       .position = option_position::short_option})
                           .build();
    REQUIRE(strip.has_value());
    // ORE reads a strip's direction from its strikes and receives on the leg itself.
    CHECK(strip->strikes[0].long_short == "Short");
    REQUIRE(strip->legs.size() == 1);
    CHECK(strip->legs[0].payer == false);
}

TEST_CASE("a_commodity_basket_option_states_no_unit", commodity_tags) {
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
    CHECK(option->instrument.unit.empty());
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
                             "CommodityBasketOption",
                             "CommodityDigitalOption",
                             "CommodityDigitalAveragePriceOption",
                             "CommodityPosition",
                             "CommoditySpreadOption",
                             "IntradayPowerForward",
                             "CommodityAsianOption",
                             "CommodityAccumulator",
                             "CommodityTaRF",
                             "CommodityGenericBarrierOption",
                             "CommodityStrikeResettableOption",
                             "CommodityForwardVolatilityAgreement"})
        CHECK(types.contains(type));
}

TEST_CASE("a_commodity_digital_option_keeps_its_payoff_as_its_quantity", commodity_tags) {
    const auto option = commodity_digital_option_builder(crude_digital()).build();
    REQUIRE(option.has_value());
    CHECK(option->instrument.quantity == 5000.0);
    CHECK(option->instrument.unit.empty());
    CHECK(option->instrument.strike_price == amount("80"));
    REQUIRE(option->options.size() == 1);
    CHECK(option->options[0].long_short == "Long");
}

TEST_CASE("a_commodity_digital_option_with_no_payoff_fails_and_names_the_term", commodity_tags) {
    auto terms = crude_digital();
    terms.payoff = amount("0");
    const auto option = commodity_digital_option_builder(terms).build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "payoff");
}

TEST_CASE("a_commodity_digital_average_price_option_averages_to_its_expiry", commodity_tags) {
    const auto option = commodity_digital_average_price_option_builder(
                            {.option = crude_digital(), .averaging_start = 2026y / 7 / 5})
                            .build();
    REQUIRE(option.has_value());
    CHECK(option->instrument.averaging_start_date == 2026y / 7 / 5);
    CHECK(option->instrument.averaging_end_date == 2027y / 1 / 5);
    CHECK(option->instrument.quantity == 5000.0);
}

TEST_CASE("a_commodity_digital_average_price_option_averaging_past_its_expiry_fails",
          commodity_tags) {
    const auto option = commodity_digital_average_price_option_builder(
                            {.option = crude_digital(), .averaging_start = 2027y / 2 / 5})
                            .build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "end");
}

TEST_CASE("a_commodity_position_holds_its_commodity_as_one_underlying", commodity_tags) {
    const auto position = commodity_position_builder({.underlying = commodity("NYMEX:CL"),
                                                      .currency = std::string("USD"),
                                                      .quantity = amount("1000"),
                                                      .unit = "bbl"})
                              .build();
    REQUIRE(position.has_value());
    REQUIRE(position->instrument_underlyings.size() == 1);
    const auto& underlying = position->instrument_underlyings[0];
    CHECK(underlying.underlying_type == "Commodity");
    CHECK(underlying.name == commodity("NYMEX:CL").uri());
    CHECK(underlying.weight == 1.0);
    CHECK(position->instrument.quantity == 1000.0);
}

TEST_CASE("a_commodity_spread_option_receives_its_long_leg_and_pays_its_short_leg",
          commodity_tags) {
    const auto option = commodity_spread_option_builder(crude_gas_spread()).build();
    REQUIRE(option.has_value());
    REQUIRE(option->legs.size() == 2);
    CHECK(option->legs[0].floating_index_code == commodity("NYMEX:CL").uri());
    CHECK(option->legs[0].payer == false);
    CHECK(option->legs[1].floating_index_code == commodity("NYMEX:NG").uri());
    CHECK(option->legs[1].payer == true);
    CHECK(option->instrument.spread_commodity_code == commodity("NYMEX:NG").uri());
    // A spread may be struck below zero.
    CHECK(option->instrument.strike_price == amount("-2.5"));
    REQUIRE(option->schedules.size() == 2);
    CHECK(option->schedules[0].tenor == "1M");
    CHECK(option->schedules[1].tenor == "1M");
}

TEST_CASE("a_commodity_spread_option_on_one_commodity_fails", commodity_tags) {
    auto terms = crude_gas_spread();
    terms.short_underlying = commodity("NYMEX:CL");
    const auto option = commodity_spread_option_builder(terms).build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "short_underlying");
}

TEST_CASE("an_intraday_power_forward_keeps_its_delivery_hours", commodity_tags) {
    const auto forward = intraday_power_forward_builder(power_hour()).build();
    REQUIRE(forward.has_value());
    CHECK(forward->instrument.delivery_date == 2027y / 1 / 12);
    CHECK(forward->instrument.delivery_start == 3600);
    CHECK(forward->instrument.delivery_end == 7200);
    CHECK(forward->instrument.position == "Short");
}

TEST_CASE("an_intraday_power_forward_ending_before_it_starts_fails", commodity_tags) {
    auto terms = power_hour();
    terms.delivery_end = 3600;
    const auto forward = intraday_power_forward_builder(terms).build();
    REQUIRE_FALSE(forward.has_value());
    CHECK(forward.error().term == "delivery_end");
}

TEST_CASE("an_intraday_power_forward_delivering_past_the_day_fails", commodity_tags) {
    auto terms = power_hour();
    terms.delivery_end = 90000;
    const auto forward = intraday_power_forward_builder(terms).build();
    REQUIRE_FALSE(forward.has_value());
    CHECK(forward.error().term == "delivery");
}

TEST_CASE("an_intraday_power_forward_delivering_before_it_matures_fails", commodity_tags) {
    auto terms = power_hour();
    terms.delivery_date = 2027y / 1 / 4;
    const auto forward = intraday_power_forward_builder(terms).build();
    REQUIRE_FALSE(forward.has_value());
    CHECK(forward.error().term == "delivery_date");
}

namespace {

commodity_accumulator_terms crude_accumulator() {
    return {.underlying = commodity("NYMEX:CL"),
            .currency = std::string("USD"),
            .fixing_amount = amount("100"),
            .strike = amount("70"),
            .knock_out = amount("90"),
            .start = 2026y / 1 / 5,
            .expiry = 2027y / 1 / 5,
            .position = option_position::long_option};
}

commodity_tarf_terms crude_tarf() {
    return {.underlying = commodity("NYMEX:CL"),
            .currency = std::string("USD"),
            .fixing_amount = amount("100"),
            .strike = amount("70"),
            .target_amount = amount("5000"),
            .start = 2026y / 1 / 5,
            .expiry = 2027y / 1 / 5,
            .position = option_position::long_option};
}

commodity_strike_resettable_option_terms crude_resettable() {
    return {.option = crude_call(),
            .reset_strike = amount("75"),
            .trigger = trigger_direction::up,
            .trigger_price = amount("90"),
            .observation_start = 2026y / 1 / 5,
            .settlement = 2027y / 1 / 7,
            .premium = amount("1200"),
            .premium_date = 2026y / 1 / 7};
}

commodity_forward_volatility_agreement_terms crude_fva() {
    return {.underlying = commodity("NYMEX:CL"),
            .currency = std::string("USD"),
            .quantity = amount("1000"),
            .fva_date = 2026y / 7 / 6,
            .expiry = 2027y / 1 / 5,
            .settlement = 2027y / 1 / 7,
            .premium_date = 2026y / 7 / 8,
            .implied_vol_strike = amount("0.3"),
            .position = option_position::long_option};
}

}

TEST_CASE("a_commodity_asian_option_observes_monthly_to_its_expiry", commodity_tags) {
    const auto option =
        commodity_asian_option_builder({.option = crude_call(), .averaging_start = 2026y / 1 / 5})
            .build();
    REQUIRE(option.has_value());
    REQUIRE(option->options.size() == 1);
    CHECK(option->options[0].payoff_type == "Asian");
    REQUIRE(option->schedules.size() == 1);
    CHECK(option->schedules[0].schedule_role == "observation_schedule");
    CHECK(option->schedules[0].start_date == 2026y / 1 / 5);
    CHECK(option->schedules[0].end_date == 2027y / 1 / 5);
    REQUIRE(option->instrument_underlyings.size() == 1);
    CHECK(option->instrument_underlyings[0].underlying_type == "Commodity");
}

TEST_CASE("a_commodity_asian_option_averaging_past_its_expiry_fails", commodity_tags) {
    const auto option =
        commodity_asian_option_builder({.option = crude_call(), .averaging_start = 2027y / 2 / 5})
            .build();
    REQUIRE_FALSE(option.has_value());
}

TEST_CASE("a_commodity_accumulator_keeps_its_fixing_amount_rather_than_a_quantity",
          commodity_tags) {
    const auto accumulator = commodity_accumulator_builder(crude_accumulator()).build();
    REQUIRE(accumulator.has_value());
    CHECK(accumulator->instrument.accumulation_amount == amount("100"));
    CHECK(accumulator->instrument.quantity == 0.0);
    REQUIRE(accumulator->range_bounds.size() == 2);
    CHECK(accumulator->range_bounds[0].range_to == amount("70"));
    CHECK(accumulator->range_bounds[0].leverage == 2.0);
    CHECK(accumulator->range_bounds[1].range_from == amount("70"));
    REQUIRE(accumulator->barriers.size() == 1);
    CHECK(accumulator->barriers[0].barrier_type == "UpAndOut");
    REQUIRE(accumulator->barrier_levels.size() == 1);
    CHECK(accumulator->barrier_levels[0].level == amount("90"));
}

TEST_CASE("a_commodity_accumulator_knocking_out_below_its_strike_fails", commodity_tags) {
    auto terms = crude_accumulator();
    terms.knock_out = amount("60");
    const auto accumulator = commodity_accumulator_builder(terms).build();
    REQUIRE_FALSE(accumulator.has_value());
    CHECK(accumulator.error().term == "knock_out");
}

TEST_CASE("a_commodity_tarf_caps_its_gains_at_its_target", commodity_tags) {
    const auto tarf = commodity_tarf_builder(crude_tarf()).build();
    REQUIRE(tarf.has_value());
    CHECK(tarf->instrument.target_amount == amount("5000"));
    REQUIRE(tarf->barriers.size() == 1);
    CHECK(tarf->barriers[0].barrier_type == "CumulatedProfitCap");
    REQUIRE(tarf->barrier_levels.size() == 1);
    CHECK(tarf->barrier_levels[0].level == amount("5000"));
    REQUIRE(tarf->schedules.size() == 1);
    CHECK(tarf->schedules[0].schedule_role == "product_schedule");
}

TEST_CASE("a_commodity_tarf_with_no_target_fails", commodity_tags) {
    auto terms = crude_tarf();
    terms.target_amount = amount("0");
    const auto tarf = commodity_tarf_builder(terms).build();
    REQUIRE_FALSE(tarf.has_value());
    CHECK(tarf.error().term == "target_amount");
}

TEST_CASE("a_commodity_generic_barrier_option_monitors_its_barrier_monthly", commodity_tags) {
    const auto option = commodity_generic_barrier_option_builder(
                            {.option = crude_call(),
                             .barrier = barrier_kind::up_and_out,
                             .barrier_level = amount("100"),
                             .monitoring_start = 2026y / 1 / 5})
                            .build();
    REQUIRE(option.has_value());
    REQUIRE(option->barriers.size() == 1);
    CHECK(option->barriers[0].barrier_type == "UpAndOut");
    CHECK(option->barrier_levels[0].level == amount("100"));
    REQUIRE(option->schedules.size() == 1);
    CHECK(option->schedules[0].schedule_role == "barrier_schedule");
}

TEST_CASE("a_commodity_generic_barrier_option_with_no_level_fails", commodity_tags) {
    const auto option = commodity_generic_barrier_option_builder(
                            {.option = crude_call(),
                             .barrier = barrier_kind::down_and_in,
                             .barrier_level = amount("0"),
                             .monitoring_start = 2026y / 1 / 5})
                            .build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "barrier_level");
}

TEST_CASE("a_commodity_strike_resettable_option_keeps_its_trigger", commodity_tags) {
    const auto option = commodity_strike_resettable_option_builder(crude_resettable()).build();
    REQUIRE(option.has_value());
    CHECK(option->instrument.trigger_type == "Up");
    CHECK(option->instrument.trigger_price == amount("90"));
    CHECK(option->instrument.reset_strike == amount("75"));
    CHECK(option->instrument.premium_amount == amount("1200"));
    CHECK(option->instrument.position == "Short");
    CHECK(option->instrument.exercise_type.empty());
    CHECK(option->options.empty());
}

TEST_CASE("a_commodity_strike_resettable_option_settling_before_its_expiry_fails",
          commodity_tags) {
    auto terms = crude_resettable();
    terms.settlement = 2027y / 1 / 4;
    const auto option = commodity_strike_resettable_option_builder(terms).build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "settlement");
}

TEST_CASE("a_commodity_forward_volatility_agreement_fixes_before_its_expiry", commodity_tags) {
    const auto fva = commodity_forward_volatility_agreement_builder(crude_fva()).build();
    REQUIRE(fva.has_value());
    CHECK(fva->instrument.fva_date == 2026y / 7 / 6);
    CHECK(fva->instrument.maturity_date == 2027y / 1 / 5);
    CHECK(fva->instrument.implied_vol_strike == amount("0.3"));
    CHECK(fva->instrument.position == "Long");
}

TEST_CASE("a_commodity_forward_volatility_agreement_fixing_at_its_expiry_fails",
          commodity_tags) {
    auto terms = crude_fva();
    terms.fva_date = 2027y / 1 / 5;
    const auto fva = commodity_forward_volatility_agreement_builder(terms).build();
    REQUIRE_FALSE(fva.has_value());
    CHECK(fva.error().term == "fva_date");
}
