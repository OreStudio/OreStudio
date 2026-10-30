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
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_product_batch.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

using namespace ores::trading::domain;
using ores::utility::decimal::decimal;

decimal bond_money(const std::string& text) {
    return *decimal::from_string(text);
}

boost::uuids::uuid an_issue() {
    return boost::uuids::string_generator()("2f2a5e14-3a9d-4c1e-9f0a-3d6b8c7e5a41");
}

boost::uuids::uuid an_underlying() {
    return boost::uuids::string_generator()("8c1d4b7a-6e2f-4a3d-8b9c-1e5f7a2d4c60");
}

bond_common_terms common_terms() {
    return bond_common_terms{.issue_id = an_issue(), .notional = bond_money("1000000")};
}

std::chrono::year_month_day a_bond_date() {
    return std::chrono::year{2028} / std::chrono::month{6} / std::chrono::day{15};
}

}

TEST_CASE("a_bond_option_states_its_strike_and_its_type", "[bond]") {
    bond_option_terms terms;
    terms.common = common_terms();
    terms.option_type = "Call";
    terms.strike = bond_money("98.5");
    terms.redemption = "100";
    terms.price_type = "Clean";
    terms.knocks_out = "false";

    const auto product = construct_bond_option(terms);
    CHECK(product.row.instrument.identity.trade_type_code == "BondOption");
    REQUIRE(product.options.size() == 1u);
    const auto& option = product.options.front();
    CHECK(option.option_type == "Call");
    CHECK(option.option_strike.to_string() == "98.5");
    CHECK(option.redemption.has_value());
    // A bond option states no forward and no repo row: it is one thing.
    CHECK(product.forwards.empty());
    CHECK(product.repos.empty());
}

TEST_CASE("a_bond_option_refuses_a_product_it_cannot_be", "[bond]") {
    bond_option_terms terms;
    terms.common = common_terms();
    terms.strike = bond_money("98.5");

    // An option without its type is a bond with a number on it.
    CHECK_THROWS_AS(construct_bond_option(terms), bond_product_error);

    terms.option_type = "Call";
    terms.common.issue_id = boost::uuids::nil_uuid();
    CHECK_THROWS_AS(construct_bond_option(terms), bond_product_error);

    terms.common = common_terms();
    terms.common.notional = decimal{};
    CHECK_THROWS_AS(construct_bond_option(terms), bond_product_error);
}

TEST_CASE("a_forward_bond_states_the_date_it_settles", "[bond]") {
    bond_forward_terms terms;
    terms.common = common_terms();
    terms.long_in_forward = "Long";
    terms.forward_settlement_date = "2028-06-15";
    terms.settlement = "Cash";
    terms.lock_rate = bond_money("0.0315");
    terms.settlement_dirty = "false";

    const auto product = construct_forward_bond(terms);
    REQUIRE(product.forwards.size() == 1u);
    const auto& forward = product.forwards.front();
    CHECK(forward.forward_settlement_date == "2028-06-15");
    CHECK(forward.amount.has_value());

    terms.forward_settlement_date.clear();
    CHECK_THROWS_AS(construct_forward_bond(terms), bond_product_error);
}

TEST_CASE("a_bond_future_states_its_contract", "[bond]") {
    bond_future_terms terms;
    terms.common = common_terms();
    terms.contract_name = "FGBL";
    terms.contract_notional = bond_money("100000");
    terms.long_short = "Short";

    const auto product = construct_bond_future(terms);
    REQUIRE(product.futures.size() == 1u);
    CHECK(product.futures.front().contract_name == "FGBL");
    CHECK(product.futures.front().long_short == "Short");

    terms.contract_name.clear();
    CHECK_THROWS_AS(construct_bond_future(terms), bond_product_error);
}

TEST_CASE("an_option_on_a_future_states_both_the_contract_and_the_option", "[bond]") {
    bond_future_option_terms terms;
    terms.future.common = common_terms();
    terms.future.contract_name = "FGBL";
    terms.future.contract_notional = bond_money("100000");
    terms.option_type = "Put";
    terms.strike = bond_money("135.25");

    const auto product = construct_bond_future_option(terms);
    CHECK(product.row.instrument.identity.trade_type_code == "BondFutureOption");
    // Neither half is the product alone, so both rows are stated.
    REQUIRE(product.futures.size() == 1u);
    REQUIRE(product.options.size() == 1u);
    CHECK(product.options.front().option_type == "Put");
}

TEST_CASE("a_repo_states_a_rate_or_the_index_it_pays_and_not_neither", "[bond]") {
    bond_repo_terms terms;
    terms.common = common_terms();
    terms.repo_type = "Repurchase";
    terms.repo_rate = bond_money("0.0425");

    const auto product = construct_bond_repo(terms);
    REQUIRE(product.repos.size() == 1u);
    CHECK(product.repos.front().repo_type == "Repurchase");

    bond_repo_terms indexed;
    indexed.common = common_terms();
    indexed.repo_type = "Repurchase";
    indexed.repo_index = "EUR-EONIA";
    CHECK(construct_bond_repo(indexed).repos.size() == 1u);

    indexed.repo_index.clear();
    CHECK_THROWS_AS(construct_bond_repo(indexed), bond_product_error);
}

TEST_CASE("a_total_return_swap_states_its_return_and_its_funding", "[bond]") {
    bond_trs_terms terms;
    terms.common = common_terms();
    terms.return_type = "Price";
    terms.funding_leg_type = "Floating";
    terms.funding_index = "EUR-EONIA";
    terms.price_type = "Clean";
    terms.initial_price = bond_money("99.75");

    const auto product = construct_bond_trs(terms);
    REQUIRE(product.trs.size() == 1u);
    CHECK(product.trs.front().return_type == "Price");
    CHECK(product.trs.front().funding_index.has_value());

    terms.return_type.clear();
    CHECK_THROWS_AS(construct_bond_trs(terms), bond_product_error);
}

TEST_CASE("an_asset_swapped_convertible_states_its_option_type", "[bond]") {
    ascot_terms terms;
    terms.common = common_terms();
    terms.option_type = "Call";

    const auto product = construct_ascot(terms);
    REQUIRE(product.ascots.size() == 1u);
    CHECK(product.ascots.front().ascot_option_type == "Call");

    terms.option_type.clear();
    CHECK_THROWS_AS(construct_ascot(terms), bond_product_error);
}

TEST_CASE("a_callable_bond_states_the_dates_its_issuer_may_call_it", "[bond]") {
    callable_bond_terms terms;
    terms.common = common_terms();
    terms.calls = {{.call_date = a_bond_date()}, {.call_date = a_bond_date()}};

    const auto product = construct_callable_bond(terms);
    // The calls belong to the security, so they join the issue and not the
    // instrument: a reader of the issue sees when it may be called.
    REQUIRE(product.call_dates.size() == 2u);
    CHECK(product.call_dates.front().sequence_number == 1);
    CHECK(product.call_dates.back().sequence_number == 2);
    CHECK(product.call_dates.front().issue_id == an_issue());
}

TEST_CASE("a_callable_bond_states_at_least_one_call_date", "[bond]") {
    callable_bond_terms terms;
    terms.common = common_terms();
    CHECK_THROWS_AS(construct_callable_bond(terms), bond_product_error);
}

TEST_CASE("a_convertible_bond_states_what_it_may_convert_into", "[bond]") {
    convertible_bond_terms terms;
    terms.common = common_terms();
    terms.targets = {{.underlying_id = boost::uuids::to_string(an_underlying()),
                      .conversion_ratio = 12.5}};

    const auto product = construct_convertible_bond(terms);
    REQUIRE(product.conversion_targets.size() == 1u);
    CHECK(product.conversion_targets.front().conversion_ratio == 12.5);
    CHECK(product.conversion_targets.front().issue_id == an_issue());

    convertible_bond_terms no_targets;
    no_targets.common = common_terms();
    CHECK_THROWS_AS(construct_convertible_bond(no_targets), bond_product_error);

    terms.targets.front().underlying_id.clear();
    CHECK_THROWS_AS(construct_convertible_bond(terms), bond_product_error);
}

TEST_CASE("a_bond_position_is_the_security_and_the_amount_of_it", "[bond]") {
    const auto product = construct_bond_position(common_terms());
    CHECK(product.row.instrument.identity.trade_type_code == "BondPosition");
    CHECK(product.row.instrument.issue_id == an_issue());
    CHECK(product.options.empty());
    CHECK(product.call_dates.empty());
}

TEST_CASE("a_whole_bond_product_crosses_the_batch_in_one_append", "[bond]") {
    bond_future_option_terms terms;
    terms.future.common = common_terms();
    terms.future.contract_name = "FGBL";
    terms.future.contract_notional = bond_money("100000");
    terms.option_type = "Put";
    terms.strike = bond_money("135.25");

    instrument_batch batch;
    append(batch, construct_bond_future_option(terms));

    REQUIRE(batch.bond_instruments.size() == 1u);
    REQUIRE(batch.bond_futures.size() == 1u);
    REQUIRE(batch.bond_options.size() == 1u);
    CHECK(batch.bond_repos.empty());
}
