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
#include "ores.trading.api/domain/activity_type.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/activity_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/domain/activity_type_table.hpp"
#include "ores.trading.api/domain/activity_type_table_io.hpp" // IWYU pragma: keep.

namespace {

using ores::trading::domain::activity_type;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

activity_type make_activity_type(const std::string& code,
    const std::string& category = "lifecycle_event") {
    activity_type at;
    at.version = 1;
    at.code = code;
    at.category = category;
    at.requires_confirmation = false;
    at.description = code + " activity type";
    at.fpml_event_type_code = "New";
    at.modified_by = "system";
    at.performed_by = "system";
    at.change_reason_code = "system.new";
    at.change_commentary = "Test data";
    at.recorded_at = std::chrono::system_clock::now();
    return at;
}

}

using ores::trading::domain::activity_type;
using namespace ores::logging;

TEST_CASE("create_activity_type_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    activity_type sut;
    sut.version = 1;
    sut.code = "new_booking";
    sut.category = "new_activity";
    sut.requires_confirmation = false;
    sut.description = "New trade booking activity";
    sut.fpml_event_type_code = "New";
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Initial creation";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Activity type: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "new_booking");
    CHECK(sut.category == "new_activity");
    CHECK(sut.requires_confirmation == false);
    CHECK(sut.description == "New trade booking activity");
    CHECK(sut.fpml_event_type_code == "New");
    CHECK(!sut.fsm_transition_id.has_value());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_activity_type_with_fsm_transition", tags) {
    auto lg(make_logger(test_suite));

    activity_type sut;
    sut.version = 1;
    sut.code = "novation";
    sut.category = "lifecycle_event";
    sut.requires_confirmation = true;
    sut.description = "Novation of a trade";
    sut.fpml_event_type_code = "Novation";
    sut.fsm_transition_id = boost::uuids::random_generator()();
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Activity type with FSM: " << sut;

    CHECK(sut.code == "novation");
    CHECK(sut.requires_confirmation == true);
    CHECK(sut.fsm_transition_id.has_value());
    CHECK(!sut.fsm_transition_id->is_nil());
}

TEST_CASE("activity_type_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    activity_type sut;
    sut.version = 1;
    sut.code = "amendment";
    sut.category = "lifecycle_event";
    sut.requires_confirmation = false;
    sut.description = "Trade amendment";
    sut.fpml_event_type_code = "Amendment";
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Activity type: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("amendment") != std::string::npos);
}

TEST_CASE("create_activity_type_with_faker", tags) {
    auto lg(make_logger(test_suite));

    activity_type sut;
    sut.version = faker::number::integer(1, 10);
    sut.code = std::string(faker::word::noun()) + "_activity";
    sut.category = std::string(faker::word::noun());
    sut.requires_confirmation = faker::datatype::boolean();
    sut.description = std::string(faker::lorem::sentence());
    sut.fpml_event_type_code = "New";
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Activity type: " << sut;

    CHECK(sut.version >= 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_activity_types", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> codes = {"new_booking", "amendment",
        "novation", "cancellation"};
    for (const auto& code : codes) {
        auto sut = make_activity_type(code);
        BOOST_LOG_SEV(lg, info) << "Activity type: " << sut;
        CHECK(!sut.code.empty());
        CHECK(sut.version == 1);
    }
}

TEST_CASE("activity_type_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<activity_type> items = {
        make_activity_type("cancellation", "cancellation")};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("cancellation") != std::string::npos);
}

TEST_CASE("activity_type_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<activity_type> items;
    for (int i = 0; i < 3; ++i)
        items.push_back(make_activity_type("activity" + std::to_string(i)));

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("activity0") != std::string::npos);
    CHECK(table.find("activity1") != std::string::npos);
    CHECK(table.find("activity2") != std::string::npos);
}

TEST_CASE("activity_type_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<activity_type> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("activity_type_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<activity_type> items;
    for (int i = 0; i < 5; ++i) {
        activity_type at;
        at.version = 1;
        at.code = std::string(faker::word::noun()) + "_act_" + std::to_string(i);
        at.category = std::string(faker::word::noun());
        at.requires_confirmation = false;
        at.description = std::string(faker::lorem::sentence());
        at.fpml_event_type_code = "New";
        at.modified_by = "system";
        at.performed_by = "system";
        at.change_reason_code = "system.new";
        at.change_commentary = "Test";
        at.recorded_at = std::chrono::system_clock::now();
        items.push_back(at);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& item : items)
        CHECK(table.find(item.code) != std::string::npos);
}
