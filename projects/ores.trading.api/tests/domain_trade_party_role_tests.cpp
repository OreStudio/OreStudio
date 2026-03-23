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

namespace {

using ores::trading::domain::trade_party_role;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

trade_party_role make_trade_party_role(const std::string& role) {
    trade_party_role tpr;
    tpr.version = 1;
    tpr.id = boost::uuids::random_generator()();
    tpr.trade_id = boost::uuids::random_generator()();
    tpr.counterparty_id = boost::uuids::random_generator()();
    tpr.role = role;
    tpr.modified_by = "system";
    tpr.performed_by = "system";
    tpr.change_reason_code = "system.new";
    tpr.change_commentary = "Test data";
    tpr.recorded_at = std::chrono::system_clock::now();
    return tpr;
}

}

using ores::trading::domain::trade_party_role;
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

    trade_party_role sut;
    sut.version = faker::number::integer(1, 10);
    sut.id = boost::uuids::random_generator()();
    sut.trade_id = boost::uuids::random_generator()();
    sut.counterparty_id = boost::uuids::random_generator()();
    sut.role = std::string(faker::word::noun()) + "_role";
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade party role: " << sut;

    CHECK(sut.version >= 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_trade_party_roles", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> roles = {"Counterparty", "CalculationAgent",
        "ExecutingBroker", "NovationTransferee"};
    for (const auto& role : roles) {
        auto sut = make_trade_party_role(role);
        BOOST_LOG_SEV(lg, info) << "Trade party role: " << sut;
        CHECK(!sut.id.is_nil());
        CHECK(sut.version == 1);
    }
}

TEST_CASE("trade_party_role_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_party_role> items = {make_trade_party_role("Counterparty")};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("trade_party_role_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_party_role> items;
    for (int i = 0; i < 3; ++i)
        items.push_back(make_trade_party_role("Role" + std::to_string(i)));

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

    std::vector<trade_party_role> items;
    for (int i = 0; i < 5; ++i) {
        trade_party_role tpr;
        tpr.version = 1;
        tpr.id = boost::uuids::random_generator()();
        tpr.trade_id = boost::uuids::random_generator()();
        tpr.counterparty_id = boost::uuids::random_generator()();
        tpr.role = std::string(faker::word::noun()) + "_role_" + std::to_string(i);
        tpr.modified_by = "system";
        tpr.performed_by = "system";
        tpr.change_reason_code = "system.new";
        tpr.change_commentary = "Test";
        tpr.recorded_at = std::chrono::system_clock::now();
        items.push_back(tpr);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
}
