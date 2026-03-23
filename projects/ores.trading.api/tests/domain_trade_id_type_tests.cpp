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
#include "ores.trading.core/generator/trade_id_type_generator.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

}

using ores::trading::domain::trade_id_type;
using ores::trading::generator::generate_synthetic_trade_id_type;
using ores::trading::generator::generate_synthetic_trade_id_types;
using ores::utility::generation::generation_context;
using namespace ores::logging;

TEST_CASE("create_trade_id_type_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    trade_id_type sut;
    sut.version = 1;
    sut.code = "UTI";
    sut.description = "Unique Trade Identifier";
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Initial creation";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade ID type: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "UTI");
    CHECK(sut.description == "Unique Trade Identifier");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("trade_id_type_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    trade_id_type sut;
    sut.version = 1;
    sut.code = "USI";
    sut.description = "Unique Swap Identifier";
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade ID type: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("USI") != std::string::npos);
}

TEST_CASE("create_trade_id_type_with_faker", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto sut = generate_synthetic_trade_id_type(ctx);
    BOOST_LOG_SEV(lg, info) << "Trade ID type: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_trade_id_types", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    const std::size_t count = 3;
    auto items = generate_synthetic_trade_id_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        BOOST_LOG_SEV(lg, info) << "Trade ID type: " << item;
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

TEST_CASE("trade_id_type_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    trade_id_type tit;
    tit.version = 1;
    tit.code = "Internal";
    tit.description = "Internal trade reference";
    tit.modified_by = "admin";
    tit.performed_by = "admin";
    tit.change_reason_code = "system.new";
    tit.change_commentary = "Test";
    tit.recorded_at = std::chrono::system_clock::now();

    std::vector<trade_id_type> items = {tit};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("Internal") != std::string::npos);
}

TEST_CASE("trade_id_type_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade_id_type> items;
    for (int i = 0; i < 3; ++i) {
        trade_id_type tit;
        tit.version = i + 1;
        tit.code = "IdType" + std::to_string(i);
        tit.description = "Description " + std::to_string(i);
        tit.modified_by = "system";
        tit.performed_by = "system";
        tit.change_reason_code = "system.new";
        tit.change_commentary = "Test";
        tit.recorded_at = std::chrono::system_clock::now();
        items.push_back(tit);
    }

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

    generation_context ctx;
    auto items = generate_synthetic_trade_id_types(5, ctx);
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& item : items) {
        CHECK(table.find(item.code) != std::string::npos);
    }
}
