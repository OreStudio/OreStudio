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
#include "ores.trading.api/builders/rate_builders.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace conv = ores::marketdata::conventions;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto rate_tags("[builders][rates]");
constexpr auto rate_euribor_6m = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M";
constexpr auto rate_euribor = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR";
constexpr auto rate_libor_3m = "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M";
constexpr auto rate_libor = "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR";
constexpr auto rate_ukrpi = "oresmd://inflation/UKRPI?type=fixing&index=inflation";

conv::convention_set rate_conventions() {
    ores::refdata::domain::ibor_index_convention euribor;
    euribor.id = "EUR-EURIBOR";
    euribor.fixing_calendar = "TARGET";
    euribor.settlement_days = 2;
    euribor.day_count_fraction = "A360";
    euribor.business_day_convention = "MF";
    euribor.oresmd_uri = rate_euribor;
    auto libor = euribor;
    libor.id = "USD-LIBOR";
    libor.fixing_calendar = "US";
    libor.oresmd_uri = rate_libor;

    ores::refdata::domain::swap_convention eur_swap;
    eur_swap.id = "EUR-6M-SWAP";
    eur_swap.fixed_calendar = "TARGET";
    eur_swap.fixed_frequency = "Annual";
    eur_swap.fixed_convention = "MF";
    eur_swap.fixed_day_count_fraction = "30/360";
    eur_swap.index = "EUR-EURIBOR-6M";
    eur_swap.oresmd_uri = rate_euribor_6m;
    auto usd_swap = eur_swap;
    usd_swap.id = "USD-3M-SWAP";
    usd_swap.fixed_calendar = "US";
    usd_swap.fixed_frequency = "Semiannual";
    usd_swap.index = "USD-LIBOR-3M";
    usd_swap.oresmd_uri = rate_libor_3m;

    ores::refdata::domain::inflation_swap_convention ukrpi;
    ukrpi.id = "UKRPI_INFLATIONSWAP";
    ukrpi.fix_calendar = "UK";
    ukrpi.fix_convention = "ModifiedFollowing";
    ukrpi.day_count_fraction = "30/360";
    ukrpi.index = "UKRPI";
    ukrpi.observation_lag = "2M";
    ukrpi.oresmd_uri = rate_ukrpi;
    return {.ibor_indices = {euribor, libor},
            .swaps = {eur_swap, usd_swap},
            .inflation_swaps = {ukrpi}};
}

md::market_reference rate_reference(const char* uri) {
    auto parsed = md::market_reference::parse(uri);
    REQUIRE(parsed);
    return *parsed;
}

vanilla_swap_terms swap_terms(const char* index = rate_euribor_6m) {
    return {.notional = *decimal::from_string("10000000"),
            .currency = std::string("EUR"),
            .start = 2026y / 1 / 5,
            .maturity = 2031y / 1 / 5,
            .fixed_rate = *decimal::from_string("0.02"),
            .index = rate_reference(index),
            .fixed_side = side::pay};
}

}

TEST_CASE("a_cross_currency_swap_takes_each_leg_from_its_own_currency", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    const cross_currency_swap_terms terms{.domestic = swap_terms(),
                                          .foreign_notional = *decimal::from_string("11000000"),
                                          .foreign_currency = std::string("USD"),
                                          .foreign_index = rate_reference(rate_libor_3m)};
    const auto swap = cross_currency_swap_builder(lookup, terms).build();
    REQUIRE(swap.has_value());
    REQUIRE(swap->legs.size() == 2);
    CHECK(swap->legs[0].currency == "EUR");
    CHECK(swap->legs[0].leg_type_code == "Fixed");
    CHECK(swap->legs[1].currency == "USD");
    CHECK(swap->legs[1].floating_index_code == rate_libor_3m);
    for (const auto& leg : swap->legs) {
        CHECK(leg.notional_initial_exchange == true);
        CHECK(leg.notional_final_exchange == true);
    }
    REQUIRE(swap->leg_amounts.size() == 2);
    CHECK(swap->leg_amounts[1].amount == *decimal::from_string("11000000"));
    REQUIRE(swap->schedules.size() == 2);
    CHECK(swap->schedules[1].calendar == "US");
}

TEST_CASE("a_cross_currency_swap_in_one_currency_fails_and_names_the_term", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    const cross_currency_swap_terms terms{.domestic = swap_terms(),
                                          .foreign_notional = *decimal::from_string("1"),
                                          .foreign_currency = std::string("EUR"),
                                          .foreign_index = rate_reference(rate_euribor_6m)};
    const auto swap = cross_currency_swap_builder(lookup, terms).build();
    REQUIRE_FALSE(swap.has_value());
    CHECK(swap.error().term == "foreign_currency");
}

TEST_CASE("a_fra_states_its_index_strike_and_side", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    auto period = swap_terms();
    period.maturity = 2026y / 7 / 5;
    const auto fra = forward_rate_agreement_builder(
                         lookup, {.period = period, .position = option_position::short_option})
                         .build();
    REQUIRE(fra.has_value());
    const auto* fact = std::get_if<ores::trading::domain::fra_instrument>(&fra->facts);
    REQUIRE(fact != nullptr);
    CHECK(fact->rate_index == rate_euribor_6m);
    CHECK(fact->long_short == "Short");
    CHECK(fact->strike == *decimal::from_string("0.02"));
}

TEST_CASE("a_cap_pays_on_the_floating_leg_alone", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    const auto cap = cap_floor_builder(lookup,
                                       {.swap = swap_terms(),
                                        .kind = cap_floor_kind::cap,
                                        .position = option_position::long_option})
                         .build();
    REQUIRE(cap.has_value());
    REQUIRE(cap->legs.size() == 1);
    CHECK(cap->legs[0].leg_type_code == "Floating");
    CHECK(cap->legs[0].identity.leg_number == 1);
    REQUIRE(cap->schedules.size() == 1);
    CHECK(cap->schedules[0].owner_number == 1);
    REQUIRE(cap->strikes.size() == 1);
    CHECK(cap->strikes[0].strike_role == "cap");
}

TEST_CASE("a_swaption_exercises_once_before_its_swap_starts", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    const auto swaption = swaption_builder(lookup,
                                           {.swap = swap_terms(),
                                            .expiry = 2026y / 1 / 1,
                                            .position = option_position::long_option,
                                            .settles = settlement::cash})
                              .build();
    REQUIRE(swaption.has_value());
    REQUIRE(swaption->options.size() == 1);
    CHECK(swaption->options[0].settlement == "Cash");
    CHECK(swaption->options[0].settlement_method == "ParYieldCurve");
    REQUIRE(swaption->schedule_dates.size() == 1);
    CHECK(swaption->schedule_dates[0].schedule_date == 2026y / 1 / 1);
}

TEST_CASE("a_swaption_that_expires_after_the_swap_starts_fails", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    const auto swaption = swaption_builder(lookup,
                                           {.swap = swap_terms(),
                                            .expiry = 2026y / 2 / 1,
                                            .position = option_position::long_option,
                                            .settles = settlement::cash})
                              .build();
    REQUIRE_FALSE(swaption.has_value());
    CHECK(swaption.error().term == "expiry");
}

TEST_CASE("a_callable_swap_may_be_called_once_a_year_from_the_first_call", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    const auto callable = callable_swap_builder(lookup,
                                                {.swap = swap_terms(),
                                                 .first_call = 2028y / 1 / 5,
                                                 .position = option_position::long_option})
                              .build();
    REQUIRE(callable.has_value());
    REQUIRE(callable->call_dates.size() == 3);
    CHECK(callable->call_dates[2].call_date == 2030y / 1 / 5);
    REQUIRE(callable->options.size() == 1);
    CHECK(callable->options[0].style == "Bermudan");
}

TEST_CASE("a_knock_out_swap_with_a_barrier_type_ore_lacks_fails", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    const auto swap = knock_out_swap_builder(lookup,
                                             {.swap = swap_terms(),
                                              .barrier_type = std::string("Sideways"),
                                              .barrier_level = *decimal::from_string("0.05"),
                                              .barrier_start = 2026y / 1 / 5})
                          .build();
    REQUIRE_FALSE(swap.has_value());
    CHECK(swap.error().term == "barrier_type");
}

TEST_CASE("a_balance_guaranteed_swap_states_one_tranche_of_its_security", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    const auto swap = balance_guaranteed_swap_builder(
                          lookup,
                          {.swap = swap_terms(), .reference_security = std::string("SEC-1")})
                          .build();
    REQUIRE(swap.has_value());
    REQUIRE(swap->tranches.size() == 1);
    CHECK(swap->tranches[0].security_id == "SEC-1");
    REQUIRE(swap->tranche_notionals.size() == 1);
    CHECK(swap->schedules.back().owner_role == "tranches");
}

TEST_CASE("an_inflation_swap_takes_its_lag_and_calendar_from_the_convention", rate_tags) {
    const conv::convention_lookup lookup(rate_conventions());
    const inflation_swap_terms terms{.notional = *decimal::from_string("1000000"),
                                     .currency = std::string("GBP"),
                                     .start = 2026y / 1 / 5,
                                     .maturity = 2031y / 1 / 5,
                                     .fixed_rate = *decimal::from_string("0.03"),
                                     .index = rate_reference(rate_ukrpi),
                                     .base_cpi = 350.0,
                                     .fixed_side = side::pay};
    const auto swap = inflation_swap_builder(lookup, terms).build();
    REQUIRE(swap.has_value());
    REQUIRE(swap->legs.size() == 2);
    CHECK(swap->legs[0].leg_type_code == "ZeroCouponFixed");
    const auto& cpi = swap->legs[1];
    CHECK(cpi.leg_type_code == "CPI");
    CHECK(cpi.observation_lag == "2M");
    CHECK(cpi.cpi_base == 350.0);
    CHECK(cpi.notional_final_exchange == true);
    REQUIRE(swap->schedule_dates.size() == 1);
    CHECK(swap->schedule_dates[0].schedule_date == 2031y / 1 / 5);
    for (const auto& schedule : swap->schedules)
        CHECK(schedule.calendar == "UK");
}

TEST_CASE("an_inflation_index_with_no_convention_fails_and_names_the_term", rate_tags) {
    const conv::convention_lookup lookup(conv::convention_set{});
    const inflation_swap_terms terms{.notional = *decimal::from_string("1000000"),
                                     .currency = std::string("GBP"),
                                     .start = 2026y / 1 / 5,
                                     .maturity = 2031y / 1 / 5,
                                     .fixed_rate = *decimal::from_string("0.03"),
                                     .index = rate_reference(rate_ukrpi),
                                     .base_cpi = 350.0,
                                     .fixed_side = side::pay};
    const auto swap = inflation_swap_builder(lookup, terms).build();
    REQUIRE_FALSE(swap.has_value());
    CHECK(swap.error().term == "index");
}
