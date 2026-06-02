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
#include "ores.trading.api/domain/trade_type.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/domain/trade_type_table.hpp"
#include "ores.trading.api/domain/trade_type_table_io.hpp" // IWYU pragma: keep.

namespace {

using ores::trading::domain::trade_type;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

trade_type make_trade_type(const std::string& code, const std::string& description = "") {
    trade_type tt;
    tt.version = 1;
    tt.code = code;
    tt.description = description.empty() ? code + " instrument type" : description;
    tt.modified_by = "system";
    tt.performed_by = "system";
    tt.change_reason_code = "system.new";
    tt.change_commentary = "Test data";
    tt.recorded_at = std::chrono::system_clock::now();
    return tt;
}

}

using ores::trading::domain::trade_type;
using namespace ores::logging;

TEST_CASE("create_trade_type_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_trade_type("Swap", "Interest rate swap instrument");
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Trade type: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "Swap");
    CHECK(sut.description == "Interest rate swap instrument");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("trade_type_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_trade_type("FxForward", "FX forward contract");
    BOOST_LOG_SEV(lg, info) << "Trade type: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("FxForward") != std::string::npos);
}

TEST_CASE("create_trade_type_with_faker", tags) {
    auto lg(make_logger(test_suite));

    trade_type sut;
    sut.version = faker::number::integer(1, 10);
    sut.code = std::string(faker::word::noun()) + "_type";
    sut.description = std::string(faker::lorem::sentence());
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade type: " << sut;

    CHECK(sut.version >= 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
}

TEST_CASE("create_multiple_random_trade_types", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> codes = {"Swap", "FxForward", "CapFloor"};
    for (const auto& code : codes) {
        auto sut = make_trade_type(code);
        BOOST_LOG_SEV(lg, info) << "Trade type: " << sut;
        CHECK(!sut.code.empty());
        CHECK(sut.version == 1);
    }
}

TEST_CASE("trade_type_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_type> items = {make_trade_type("CapFloor", "Cap or floor on floating rate")};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("CapFloor") != std::string::npos);
}

TEST_CASE("trade_type_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_type> items;
    for (int i = 0; i < 3; ++i)
        items.push_back(make_trade_type("Type" + std::to_string(i)));

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("Type0") != std::string::npos);
    CHECK(table.find("Type1") != std::string::npos);
    CHECK(table.find("Type2") != std::string::npos);
}

TEST_CASE("trade_type_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_type> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("trade_type_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_type> items;
    for (int i = 0; i < 5; ++i) {
        trade_type tt;
        tt.version = 1;
        tt.code = std::string(faker::word::noun()) + "_" + std::to_string(i);
        tt.description = std::string(faker::lorem::sentence());
        tt.modified_by = "system";
        tt.performed_by = "system";
        tt.change_reason_code = "system.new";
        tt.change_commentary = "Test";
        tt.recorded_at = std::chrono::system_clock::now();
        items.push_back(tt);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& item : items)
        CHECK(table.find(item.code) != std::string::npos);
}
