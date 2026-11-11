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
#include "ores.trading.api/builders/bond_builders.hpp"
#include "ores.trading.api/builders/builder_registry.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto bond_tags("[builders][bond]");

decimal bond_amount(const char* text) {
    return *decimal::from_string(text);
}

boost::uuids::uuid bond_issue() {
    return boost::uuids::string_generator()("6f1c2d3e-4a5b-4c6d-8e7f-001122334455");
}

bond_holding holding() {
    return {.issue_id = bond_issue(),
            .notional = bond_amount("1000000"),
            .currency = std::string("EUR")};
}

md::market_reference bond_index(const char* uri) {
    auto parsed = md::market_reference::parse(uri);
    REQUIRE(parsed);
    return *parsed;
}

}

TEST_CASE("a_bond_issue_states_its_fixed_coupon_leg", bond_tags) {
    const auto security = bond_issue_builder({.issue_id = bond_issue(),
                                              .security_id = std::string("SEC-1"),
                                              .issuer = std::string("ISSUER"),
                                              .currency = std::string("EUR"),
                                              .face_value = bond_amount("1"),
                                              .coupon_rate = bond_amount("0.05"),
                                              .issue_date = 2025y / 1 / 2,
                                              .maturity = 2030y / 1 / 2,
                                              .calendar = std::string("TARGET"),
                                              .reference_curve = "EUR-EURIBOR-6M"})
                              .build();
    REQUIRE(security.has_value());
    REQUIRE(security->legs.size() == 1);
    CHECK(security->legs[0].currency == "EUR");
    REQUIRE(security->rates.size() == 1);
    CHECK(security->rates[0].rate_kind == "fixed");
    CHECK(security->amounts.size() == 2);
    REQUIRE(security->schedules.size() == 1);
    CHECK(security->schedules[0].end_date == 2030y / 1 / 2);
    CHECK(security->issue.reference_curve_id == "EUR-EURIBOR-6M");
}

TEST_CASE("a_bond_option_states_its_option_block_and_one_exercise_date", bond_tags) {
    const auto option = bond_option_builder({.holding = holding(),
                                             .kind = option_kind::call,
                                             .strike = bond_amount("101"),
                                             .expiry = 2027y / 1 / 2,
                                             .position = option_position::long_option})
                            .build();
    REQUIRE(option.has_value());
    REQUIRE(option->option_block.size() == 1);
    CHECK(option->option_block[0].style == "European");
    CHECK(option->option_block[0].option_type == "Call");
    REQUIRE(option->schedule_dates.size() == 1);
    CHECK(option->schedule_dates[0].schedule_date == 2027y / 1 / 2);
    REQUIRE(option->options.size() == 1);
    CHECK(option->options[0].option_type == "Call");
}

TEST_CASE("a_forward_bond_states_its_side_as_ore_reads_it", bond_tags) {
    const auto forward = forward_bond_builder({.holding = holding(),
                                               .long_in_forward = true,
                                               .forward_maturity = 2027y / 1 / 2,
                                               .settlement = 2026y / 12 / 2,
                                               .lock_rate = bond_amount("0.03")})
                             .build();
    REQUIRE(forward.has_value());
    REQUIRE(forward->forwards.size() == 1);
    CHECK(forward->forwards[0].long_in_forward == "true");
    CHECK(forward->forwards[0].forward_settlement_date == "2026-12-02");
    CHECK(forward->forwards[0].lock_rate.has_value());
    CHECK_FALSE(forward->forwards[0].amount.has_value());
}

TEST_CASE("a_bond_repo_states_its_cash_leg_at_the_repo_rate", bond_tags) {
    const auto repo = bond_repo_builder({.holding = holding(),
                                         .repo_rate = bond_amount("0.04"),
                                         .start = 2026y / 1 / 2,
                                         .end = 2027y / 1 / 2})
                          .build();
    REQUIRE(repo.has_value());
    REQUIRE(repo->legs.size() == 1);
    CHECK(repo->legs[0].leg_role == "repo");
    CHECK(repo->legs[0].day_counter == "A360");
    REQUIRE(repo->leg_rates.size() == 1);
    CHECK(repo->leg_rates[0].rate_kind == "fixed");
    REQUIRE(repo->schedules.size() == 1);
    CHECK(repo->schedules[0].owner_role == "repo");
}

TEST_CASE("a_bond_trs_states_its_funding_leg_in_the_bond_currency", bond_tags) {
    const auto trs = bond_trs_builder({.holding = holding(),
                                       .funding_rate = bond_amount("0.02"),
                                       .start = 2026y / 1 / 2,
                                       .end = 2027y / 1 / 2})
                         .build();
    REQUIRE(trs.has_value());
    REQUIRE(trs->legs.size() == 1);
    CHECK(trs->legs[0].leg_role == "trs_funding");
    CHECK(trs->legs[0].currency == "EUR");
    REQUIRE(trs->schedules.size() == 2);
    CHECK(trs->schedules[1].owner_role == "trs");
    CHECK(trs->schedules[1].tenor == "3M");
}

TEST_CASE("an_ascot_recalls_against_a_floating_swap_leg", bond_tags) {
    const auto euribor = bond_index("oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=3M");
    const auto ascot = ascot_builder({.convertible = {.holding = holding(),
                                                      .share = std::string(".STOXX50E"),
                                                      .conversion_ratio = 10.0},
                                      .kind = option_kind::call,
                                      .funding_index = euribor,
                                      .recall_spread = bond_amount("0.02"),
                                      .start = 2026y / 1 / 2,
                                      .end = 2030y / 1 / 2})
                           .build();
    REQUIRE(ascot.has_value());
    CHECK(ascot->row.instrument.identity.trade_type_code == "Ascot");
    REQUIRE(ascot->legs.size() == 1);
    CHECK(ascot->legs[0].leg_type == "Floating");
    REQUIRE(ascot->leg_rates.size() == 1);
    CHECK(ascot->leg_rates[0].index == euribor.uri());
    CHECK(ascot->conversion_targets.size() == 1);
}

TEST_CASE("a_bond_position_holds_its_security_as_a_bond_underlying", bond_tags) {
    auto index = md::market_index::make(md::index_family::bond, "SEC-1", {});
    REQUIRE(index);
    auto security = md::market_reference::of(std::move(*index));
    REQUIRE(security);
    const auto position =
        bond_position_builder({.holding = holding(), .security = *security}).build();
    REQUIRE(position.has_value());
    REQUIRE(position->underlyings.size() == 1);
    CHECK(position->underlyings[0].underlying_type == "Bond");
    CHECK(position->underlyings[0].name == security->uri());
}

TEST_CASE("a_callable_bond_with_no_call_fails_and_names_the_term", bond_tags) {
    const auto callable = callable_bond_builder({.holding = holding(), .calls = std::vector<year_month_day>{}}).build();
    REQUIRE_FALSE(callable.has_value());
    CHECK(callable.error().term == "calls");
}

TEST_CASE("a_bond_with_no_notional_fails_and_names_the_term", bond_tags) {
    auto empty = holding();
    empty.notional = decimal{};
    const auto bond = bond_builder(empty).build();
    REQUIRE_FALSE(bond.has_value());
    CHECK(bond.error().term == "notional");
}

TEST_CASE("the_registry_names_every_bond_builder", bond_tags) {
    const auto types = built_trade_types();
    for (const auto* type : {"Bond",
                             "BondPosition",
                             "BondOption",
                             "ForwardBond",
                             "BondFuture",
                             "BondRepo",
                             "BondTRS",
                             "CallableBond",
                             "ConvertibleBond",
                             "Ascot"})
        CHECK(types.contains(type));
}
