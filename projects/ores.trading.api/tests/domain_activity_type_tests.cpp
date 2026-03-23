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
#include "ores.trading.core/generator/activity_type_generator.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

}

using ores::trading::domain::activity_type;
using ores::trading::generator::generate_synthetic_activity_type;
using ores::trading::generator::generate_synthetic_activity_types;
using ores::utility::generation::generation_context;
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

    generation_context ctx;
    auto sut = generate_synthetic_activity_type(ctx);
    BOOST_LOG_SEV(lg, info) << "Activity type: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_activity_types", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    const std::size_t count = 3;
    auto items = generate_synthetic_activity_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        BOOST_LOG_SEV(lg, info) << "Activity type: " << item;
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

TEST_CASE("activity_type_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    activity_type at;
    at.version = 1;
    at.code = "cancellation";
    at.category = "cancellation";
    at.requires_confirmation = false;
    at.description = "Trade cancellation";
    at.modified_by = "admin";
    at.performed_by = "admin";
    at.change_reason_code = "system.new";
    at.change_commentary = "Test";
    at.recorded_at = std::chrono::system_clock::now();

    std::vector<activity_type> items = {at};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("cancellation") != std::string::npos);
}

TEST_CASE("activity_type_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<activity_type> items;
    for (int i = 0; i < 3; ++i) {
        activity_type at;
        at.version = i + 1;
        at.code = "activity" + std::to_string(i);
        at.category = "new_activity";
        at.requires_confirmation = false;
        at.description = "Description " + std::to_string(i);
        at.modified_by = "system";
        at.performed_by = "system";
        at.change_reason_code = "system.new";
        at.change_commentary = "Test";
        at.recorded_at = std::chrono::system_clock::now();
        items.push_back(at);
    }

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

    generation_context ctx;
    auto items = generate_synthetic_activity_types(5, ctx);
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& item : items) {
        CHECK(table.find(item.code) != std::string::npos);
    }
}
