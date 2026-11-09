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
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/builders/builder_registry.hpp"
#include "ores.trading.api/builders/vanilla_swap_builder.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <fstream>
#include <regex>
#include <set>
#include <string>
#include <type_traits>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace conv = ores::marketdata::conventions;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto tags("[builders][vanilla_swap]");
constexpr auto euribor_6m = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M";
constexpr auto euribor = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR";

conv::convention_set euribor_conventions() {
    ores::refdata::domain::ibor_index_convention index;
    index.id = "EUR-EURIBOR";
    index.fixing_calendar = "TARGET";
    index.settlement_days = 2;
    index.day_count_fraction = "A360";
    index.business_day_convention = "MF";
    index.oresmd_uri = euribor;
    ores::refdata::domain::swap_convention swap;
    swap.id = "EUR-6M-SWAP";
    swap.fixed_calendar = "TARGET";
    swap.fixed_frequency = "Annual";
    swap.fixed_convention = "MF";
    swap.fixed_day_count_fraction = "30/360";
    swap.index = "EUR-EURIBOR-6M";
    swap.oresmd_uri = euribor_6m;
    return {.ibor_indices = {index}, .swaps = {swap}};
}

vanilla_swap_terms terms(std::string_view address = euribor_6m) {
    return {.notional = *decimal::from_string("10000000"),
            .currency = std::string("EUR"),
            .start = 2026y / 1 / 5,
            .maturity = 2036y / 1 / 5,
            .fixed_rate = *decimal::from_string("0.02"),
            .index = *md::market_reference::parse(address),
            .fixed_side = side::pay};
}

}

TEST_CASE("a_swap_takes_each_legs_terms_from_the_conventions", tags) {
    conv::convention_lookup lookup(euribor_conventions());
    const auto swap = vanilla_swap_builder(lookup, terms()).build();
    REQUIRE(swap.has_value());
    REQUIRE(swap->legs.size() == 2);

    const auto& fixed = swap->legs[0];
    CHECK(fixed.payer == true);
    CHECK(fixed.payment_frequency_code == "Annual");
    CHECK(fixed.day_count_fraction_code == "30/360");

    const auto& floating = swap->legs[1];
    CHECK(floating.payer == false);
    CHECK(floating.floating_index_code == euribor_6m);
    CHECK(floating.payment_frequency_code == "Semiannual");
    CHECK(floating.fixing_days == 2);
    CHECK(floating.day_count_fraction_code == "A360");
}

TEST_CASE("every_leg_has_a_schedule_that_states_its_calendar_and_rule", tags) {
    conv::convention_lookup lookup(euribor_conventions());
    const auto swap = vanilla_swap_builder(lookup, terms()).build();
    REQUIRE(swap.has_value());
    REQUIRE(swap->schedules.size() == 2);
    for (const auto& schedule : swap->schedules) {
        CHECK(schedule.calendar == "TARGET");
        CHECK(schedule.rule == "Forward");
    }
    CHECK(swap->schedules[0].tenor == "1Y");
    CHECK(swap->schedules[1].tenor == "6M");
}

TEST_CASE("a_setter_overrides_what_the_convention_states", tags) {
    conv::convention_lookup lookup(euribor_conventions());
    const auto swap = vanilla_swap_builder(lookup, terms()).fixed_day_count("A365").build();
    REQUIRE(swap.has_value());
    CHECK(swap->legs[0].day_count_fraction_code == "A365");
}

TEST_CASE("an_index_no_convention_states_fails_and_names_the_term", tags) {
    conv::convention_lookup lookup(conv::convention_set{});
    const auto swap = vanilla_swap_builder(lookup, terms()).build();
    REQUIRE_FALSE(swap.has_value());
    CHECK(swap.error().term == "index");
}

TEST_CASE("a_maturity_before_the_start_fails_and_names_the_term", tags) {
    conv::convention_lookup lookup(euribor_conventions());
    auto reversed = terms();
    reversed.maturity = 2020y / 1 / 5;
    const auto swap = vanilla_swap_builder(lookup, reversed).build();
    REQUIRE_FALSE(swap.has_value());
    CHECK(swap.error().term == "maturity");
}

TEST_CASE("the_registry_names_the_swap", tags) {
    CHECK(built_trade_types().contains("Swap"));
}

TEST_CASE("a_frequency_that_has_no_tenor_fails_and_names_the_term", tags) {
    auto conventions = euribor_conventions();
    conventions.swaps[0].fixed_frequency = "Lunarmonth";
    conv::convention_lookup lookup(conventions);
    const auto swap = vanilla_swap_builder(lookup, terms()).build();
    REQUIRE_FALSE(swap.has_value());
    CHECK(swap.error().term == "index");
}

namespace {

std::set<std::string> catalogued_trade_types() {
    const auto path = ores::testing::project_root::resolve(
        "projects/ores.sql/populate/trading/trading_product_types_populate.sql");
    std::ifstream sql(path);
    const std::regex row(R"(^\s*\('(\w+)', ores_utility_system_tenant_id_fn\(\), 0, ')");
    std::set<std::string> out;
    std::smatch found;
    for (std::string line; std::getline(sql, line);)
        if (std::regex_search(line, found, row))
            out.insert(found[1]);
    return out;
}

std::set<std::string> trade_types_awaiting_builders() {
    std::ifstream file(ores::testing::project_root::resolve(
        "projects/ores.trading/api/tests/data/trade_types_awaiting_builders.txt"));
    std::set<std::string> out;
    for (std::string line; std::getline(file, line);)
        if (!line.empty())
            out.insert(line);
    return out;
}

}

TEST_CASE("every_catalogued_trade_type_has_a_builder_or_awaits_one", tags) {
    const auto catalogue = catalogued_trade_types();
    const auto awaiting = trade_types_awaiting_builders();
    REQUIRE_FALSE(catalogue.empty());

    std::set<std::string> accounted(awaiting);
    for (const auto type : built_trade_types()) {
        CHECK(catalogue.contains(std::string(type)));
        CHECK_FALSE(awaiting.contains(std::string(type)));
        accounted.emplace(type);
    }
    CHECK(accounted == catalogue);
}

TEST_CASE("a_notional_that_is_not_positive_fails_and_names_the_term", tags) {
    conv::convention_lookup lookup(euribor_conventions());
    auto zero = terms();
    zero.notional = decimal{};
    const auto swap = vanilla_swap_builder(lookup, zero).build();
    REQUIRE_FALSE(swap.has_value());
    CHECK(swap.error().term == "notional");
}

TEST_CASE("an_empty_currency_fails_and_names_the_term", tags) {
    conv::convention_lookup lookup(euribor_conventions());
    auto blank = terms();
    blank.currency = std::string{};
    const auto swap = vanilla_swap_builder(lookup, blank).build();
    REQUIRE_FALSE(swap.has_value());
    CHECK(swap.error().term == "currency");
}

TEST_CASE("a_required_term_copies_from_a_value_that_is_not_const", tags) {
    required<std::string> original(std::string("EUR"));
    required<std::string> copy = original;
    CHECK(copy.get() == "EUR");
}

TEST_CASE("a_builder_does_not_accept_conventions_that_would_dangle", tags) {
    STATIC_CHECK_FALSE(std::is_constructible_v<vanilla_swap_builder,
                                               conv::convention_lookup&&,
                                               vanilla_swap_terms>);
}
