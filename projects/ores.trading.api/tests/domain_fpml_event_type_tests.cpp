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
#include "ores.trading.api/domain/fpml_event_type.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/fpml_event_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/domain/fpml_event_type_table.hpp"
#include "ores.trading.api/domain/fpml_event_type_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/generator/fpml_event_type_generator.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

}

using ores::trading::domain::fpml_event_type;
using ores::trading::generator::generate_synthetic_fpml_event_type;
using ores::trading::generator::generate_synthetic_fpml_event_types;
using ores::utility::generation::generation_context;
using namespace ores::logging;

TEST_CASE("create_fpml_event_type_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    fpml_event_type sut;
    sut.version = 1;
    sut.code = "New";
    sut.description = "New trade creation event";
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Initial creation";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "FpML event type: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "New");
    CHECK(sut.description == "New trade creation event");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("fpml_event_type_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    fpml_event_type sut;
    sut.version = 1;
    sut.code = "Amendment";
    sut.description = "Trade amendment event";
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "FpML event type: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("Amendment") != std::string::npos);
}

TEST_CASE("create_fpml_event_type_with_faker", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto sut = generate_synthetic_fpml_event_type(ctx);
    BOOST_LOG_SEV(lg, info) << "FpML event type: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_fpml_event_types", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    const std::size_t count = 3;
    auto items = generate_synthetic_fpml_event_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        BOOST_LOG_SEV(lg, info) << "FpML event type: " << item;
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

TEST_CASE("fpml_event_type_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    fpml_event_type et;
    et.version = 1;
    et.code = "Novation";
    et.description = "Trade novation event";
    et.modified_by = "admin";
    et.performed_by = "admin";
    et.change_reason_code = "system.new";
    et.change_commentary = "Test";
    et.recorded_at = std::chrono::system_clock::now();

    std::vector<fpml_event_type> items = {et};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("Novation") != std::string::npos);
}

TEST_CASE("fpml_event_type_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<fpml_event_type> items;
    for (int i = 0; i < 3; ++i) {
        fpml_event_type et;
        et.version = i + 1;
        et.code = "Event" + std::to_string(i);
        et.description = "Description " + std::to_string(i);
        et.modified_by = "system";
        et.performed_by = "system";
        et.change_reason_code = "system.new";
        et.change_commentary = "Test";
        et.recorded_at = std::chrono::system_clock::now();
        items.push_back(et);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("Event0") != std::string::npos);
    CHECK(table.find("Event1") != std::string::npos);
    CHECK(table.find("Event2") != std::string::npos);
}

TEST_CASE("fpml_event_type_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<fpml_event_type> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("fpml_event_type_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto items = generate_synthetic_fpml_event_types(5, ctx);
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& item : items) {
        CHECK(table.find(item.code) != std::string::npos);
    }
}
