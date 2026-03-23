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
#include "ores.trading.api/domain/lifecycle_event.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/lifecycle_event_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/domain/lifecycle_event_table.hpp"
#include "ores.trading.api/domain/lifecycle_event_table_io.hpp" // IWYU pragma: keep.

namespace {

using ores::trading::domain::lifecycle_event;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

lifecycle_event make_lifecycle_event(const std::string& code,
    const std::string& description = "") {
    lifecycle_event le;
    le.version = 1;
    le.code = code;
    le.description = description.empty() ? code + " lifecycle event" : description;
    le.modified_by = "system";
    le.performed_by = "system";
    le.change_reason_code = "system.new";
    le.change_commentary = "Test data";
    le.recorded_at = std::chrono::system_clock::now();
    return le;
}

}

using ores::trading::domain::lifecycle_event;
using namespace ores::logging;

TEST_CASE("create_lifecycle_event_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_lifecycle_event("New", "New trade lifecycle event");
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Lifecycle event: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "New");
    CHECK(sut.description == "New trade lifecycle event");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("lifecycle_event_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_lifecycle_event("Amendment");
    BOOST_LOG_SEV(lg, info) << "Lifecycle event: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("Amendment") != std::string::npos);
}

TEST_CASE("create_lifecycle_event_with_faker", tags) {
    auto lg(make_logger(test_suite));

    lifecycle_event sut;
    sut.version = faker::number::integer(1, 10);
    sut.code = std::string(faker::word::noun()) + "_event";
    sut.description = std::string(faker::lorem::sentence());
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Lifecycle event: " << sut;

    CHECK(sut.version >= 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
}

TEST_CASE("create_multiple_random_lifecycle_events", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> codes = {"New", "Amendment", "Novation",
        "PartialTermination", "FullTermination"};
    for (const auto& code : codes) {
        auto sut = make_lifecycle_event(code);
        BOOST_LOG_SEV(lg, info) << "Lifecycle event: " << sut;
        CHECK(!sut.code.empty());
    }
}

TEST_CASE("lifecycle_event_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<lifecycle_event> items = {
        make_lifecycle_event("FullTermination", "Full termination of trade")};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("FullTermination") != std::string::npos);
}

TEST_CASE("lifecycle_event_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<lifecycle_event> items;
    for (int i = 0; i < 3; ++i)
        items.push_back(make_lifecycle_event("Event" + std::to_string(i)));

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("Event0") != std::string::npos);
    CHECK(table.find("Event1") != std::string::npos);
    CHECK(table.find("Event2") != std::string::npos);
}

TEST_CASE("lifecycle_event_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<lifecycle_event> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("lifecycle_event_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<lifecycle_event> items;
    for (int i = 0; i < 5; ++i) {
        lifecycle_event le;
        le.version = 1;
        le.code = std::string(faker::word::noun()) + "_" + std::to_string(i);
        le.description = std::string(faker::lorem::sentence());
        le.modified_by = "system";
        le.performed_by = "system";
        le.change_reason_code = "system.new";
        le.change_commentary = "Test";
        le.recorded_at = std::chrono::system_clock::now();
        items.push_back(le);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& item : items)
        CHECK(table.find(item.code) != std::string::npos);
}
