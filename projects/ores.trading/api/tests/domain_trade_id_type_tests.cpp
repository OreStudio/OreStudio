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
#include "ores.trading.api/domain/trade_id_type.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_id_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/domain/trade_id_type_table.hpp"
#include "ores.trading.api/domain/trade_id_type_table_io.hpp" // IWYU pragma: keep.

namespace {

using ores::trading::domain::trade_id_type;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

trade_id_type make_trade_id_type(const std::string& code,
    const std::string& description = "") {
    trade_id_type tit;
    tit.version = 1;
    tit.code = code;
    tit.description = description.empty() ? code + " trade identifier type" : description;
    tit.modified_by = "system";
    tit.performed_by = "system";
    tit.change_reason_code = "system.new";
    tit.change_commentary = "Test data";
    tit.recorded_at = std::chrono::system_clock::now();
    return tit;
}

}

using ores::trading::domain::trade_id_type;
using namespace ores::logging;

TEST_CASE("create_trade_id_type_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_trade_id_type("UTI", "Unique Trade Identifier");
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Trade ID type: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "UTI");
    CHECK(sut.description == "Unique Trade Identifier");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("trade_id_type_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_trade_id_type("USI", "Unique Swap Identifier");
    BOOST_LOG_SEV(lg, info) << "Trade ID type: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("USI") != std::string::npos);
}

TEST_CASE("create_trade_id_type_with_faker", tags) {
    auto lg(make_logger(test_suite));

    trade_id_type sut;
    sut.version = faker::number::integer(1, 10);
    sut.code = std::string(faker::word::noun()) + "_id";
    sut.description = std::string(faker::lorem::sentence());
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade ID type: " << sut;

    CHECK(sut.version >= 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
}

TEST_CASE("create_multiple_random_trade_id_types", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> codes = {"UTI", "USI", "Internal", "CICI"};
    for (const auto& code : codes) {
        auto sut = make_trade_id_type(code);
        BOOST_LOG_SEV(lg, info) << "Trade ID type: " << sut;
        CHECK(!sut.code.empty());
    }
}

TEST_CASE("trade_id_type_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_id_type> items = {make_trade_id_type("Internal", "Internal trade reference")};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("Internal") != std::string::npos);
}

TEST_CASE("trade_id_type_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_id_type> items;
    for (int i = 0; i < 3; ++i)
        items.push_back(make_trade_id_type("IdType" + std::to_string(i)));

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("IdType0") != std::string::npos);
    CHECK(table.find("IdType1") != std::string::npos);
    CHECK(table.find("IdType2") != std::string::npos);
}

TEST_CASE("trade_id_type_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_id_type> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("trade_id_type_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_id_type> items;
    for (int i = 0; i < 5; ++i) {
        trade_id_type tit;
        tit.version = 1;
        tit.code = std::string(faker::word::noun()) + "_id_" + std::to_string(i);
        tit.description = std::string(faker::lorem::sentence());
        tit.modified_by = "system";
        tit.performed_by = "system";
        tit.change_reason_code = "system.new";
        tit.change_commentary = "Test";
        tit.recorded_at = std::chrono::system_clock::now();
        items.push_back(tit);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& item : items)
        CHECK(table.find(item.code) != std::string::npos);
}
