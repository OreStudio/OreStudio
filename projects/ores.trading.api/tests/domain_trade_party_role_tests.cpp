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
#include "ores.trading.api/domain/trade_party_role.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_party_role_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/domain/trade_party_role_table.hpp"
#include "ores.trading.api/domain/trade_party_role_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/generator/trade_party_role_generator.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

}

using ores::trading::domain::trade_party_role;
using ores::trading::generator::generate_synthetic_trade_party_role;
using ores::trading::generator::generate_synthetic_trade_party_roles;
using ores::utility::generation::generation_context;
using namespace ores::logging;

TEST_CASE("create_trade_party_role_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    trade_party_role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.trade_id = boost::uuids::random_generator()();
    sut.counterparty_id = boost::uuids::random_generator()();
    sut.role = "Counterparty";
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Initial assignment";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade party role: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.trade_id.is_nil());
    CHECK(!sut.counterparty_id.is_nil());
    CHECK(sut.role == "Counterparty");
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("trade_party_role_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    trade_party_role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.trade_id = boost::uuids::random_generator()();
    sut.counterparty_id = boost::uuids::random_generator()();
    sut.role = "CalculationAgent";
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade party role: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("CalculationAgent") != std::string::npos);
}

TEST_CASE("create_trade_party_role_with_faker", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto sut = generate_synthetic_trade_party_role(ctx);
    BOOST_LOG_SEV(lg, info) << "Trade party role: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_trade_party_roles", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    const std::size_t count = 3;
    auto items = generate_synthetic_trade_party_roles(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        BOOST_LOG_SEV(lg, info) << "Trade party role: " << item;
        CHECK(!item.id.is_nil());
        CHECK(item.version == 1);
    }
}

TEST_CASE("trade_party_role_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto tpr = generate_synthetic_trade_party_role(ctx);

    std::vector<trade_party_role> items = {tpr};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("trade_party_role_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto items = generate_synthetic_trade_party_roles(3, ctx);
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("trade_party_role_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_party_role> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("trade_party_role_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto items = generate_synthetic_trade_party_roles(5, ctx);
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
}
