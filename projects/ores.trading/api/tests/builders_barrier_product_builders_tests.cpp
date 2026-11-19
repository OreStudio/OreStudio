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
#include "ores.trading.api/builders/barrier_product_builders.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <variant>
#include <vector>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace md = ores::marketdata::datum;
namespace domain = ores::trading::domain;
using ores::utility::decimal::decimal;

namespace {

const auto barrier_product_tags("[builders][barrier_product]");

decimal barrier_product_amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference barrier_product_equity(const char* code) {
    auto index = md::market_index::make(md::index_family::equity, code, {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

}

namespace {

window_barrier_option_terms spx_window_barrier() {
    return {.underlying = barrier_product_equity(".SPX"),
            .currency = std::string("USD"),
            .fixing_amount = barrier_product_amount("1000"),
            .strike = barrier_product_amount("4000"),
            .kind = option_kind::call,
            .position = option_position::long_option,
            .expiry = 2027y / 1 / 5,
            .window_start = 2026y / 1 / 5,
            .window_end = 2026y / 7 / 5,
            .barrier =
                scripted_barrier{.type = "DownAndOut", .level = barrier_product_amount("3600")}};
}

generic_barrier_option_terms spx_generic_barrier() {
    return {.underlying = barrier_product_equity(".SPX"),
            .pay_currency = std::string("USD"),
            .quantity = barrier_product_amount("1000"),
            .strike = barrier_product_amount("4000"),
            .kind = option_kind::call,
            .position = option_position::long_option,
            .expiry = 2027y / 1 / 5,
            .settlement = 2027y / 1 / 7,
            .monitoring_start = 2026y / 1 / 5,
            .barriers =
                std::vector<scripted_barrier>{
                    {.type = "DownAndOut", .level = barrier_product_amount("3600")},
                    {.type = "UpAndIn", .level = barrier_product_amount("4400")}},
            .kiko_type = std::string("KoAfterKi")};
}

}

TEST_CASE("a_window_barrier_option_states_its_window_its_option_and_its_barrier",
          barrier_product_tags) {
    const auto built = fx_window_barrier_option_builder(spx_window_barrier()).build();
    REQUIRE(built.has_value());
    REQUIRE(built->window_barrier_options.size() == 1);
    const auto& row = built->window_barrier_options[0];
    CHECK(row.identity.trade_type_code == "FxWindowBarrierOption");
    CHECK(row.currency == "USD");
    CHECK(row.fixing_amount == barrier_product_amount("1000"));
    CHECK(row.strike == barrier_product_amount("4000"));
    CHECK(row.start_date == 2026y / 1 / 5);
    CHECK(row.end_date == 2026y / 7 / 5);
    CHECK_FALSE(row.strike_currency.has_value());
    CHECK(built->instrument_underlyings[0].underlying_type == "FX");
    REQUIRE(built->options.size() == 1);
    CHECK(built->options[0].option_type == "Call");
    REQUIRE(built->barriers.size() == 1);
    CHECK(built->barriers[0].barrier_role == "barrier");
    CHECK(built->barriers[0].barrier_type == "DownAndOut");
    REQUIRE(built->barrier_levels.size() == 1);
    CHECK(built->barrier_levels[0].level == barrier_product_amount("3600"));
}

TEST_CASE("a_window_barrier_option_refuses_a_window_past_the_expiry_and_an_unknown_barrier",
          barrier_product_tags) {
    auto terms = spx_window_barrier();
    terms.window_end = 2027y / 2 / 1;
    auto built = equity_window_barrier_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "window_end");

    terms = spx_window_barrier();
    terms.barrier = scripted_barrier{.type = "KnockOut", .level = barrier_product_amount("3600")};
    built = equity_window_barrier_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "barrier");
}

TEST_CASE("a_generic_barrier_option_states_its_barriers_their_schedule_and_its_kiko_type",
          barrier_product_tags) {
    const auto built = equity_generic_barrier_option_builder(spx_generic_barrier()).build();
    REQUIRE(built.has_value());
    REQUIRE(built->barriers.size() == 2);
    CHECK(built->barriers[1].barrier_role == "barrier");
    CHECK(built->barriers[1].sequence_number == 2);
    CHECK(built->barriers[1].barrier_type == "UpAndIn");
    REQUIRE(built->schedules.size() == 2);
    const auto monitoring = std::ranges::find_if(
        built->schedules, [](const auto& s) { return s.schedule_role == "barrier_schedule"; });
    REQUIRE(monitoring != built->schedules.end());
    CHECK(monitoring->tenor == "1D");
    const auto* row =
        std::get_if<domain::equity_generic_barrier_option_instrument>(&built->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->pay_currency == "USD");
    CHECK(row->quantity == std::optional(1000.0));
    CHECK(row->strike == std::optional(barrier_product_amount("4000")));
    CHECK(row->settlement_date == std::optional(2027y / 1 / 7));
    CHECK(row->kiko_type == std::optional<std::string>("KoAfterKi"));
    REQUIRE(built->options.size() == 1);
    CHECK(built->options[0].payoff_type == std::optional<std::string>("Vanilla"));
}

TEST_CASE("a_generic_barrier_option_refuses_no_barrier_and_an_unknown_kiko_type",
          barrier_product_tags) {
    auto terms = spx_generic_barrier();
    terms.barriers = std::vector<scripted_barrier>{};
    auto built = equity_generic_barrier_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "barriers");

    terms = spx_generic_barrier();
    terms.kiko_type = std::string("Sometimes");
    built = equity_generic_barrier_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "kiko_type");
}
