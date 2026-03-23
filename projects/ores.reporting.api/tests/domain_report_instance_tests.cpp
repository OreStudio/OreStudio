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
#include "ores.reporting.api/domain/report_instance.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_instance_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/domain/report_instance_table.hpp"
#include "ores.reporting.api/domain/report_instance_table_io.hpp" // IWYU pragma: keep.

namespace {

using ores::reporting::domain::report_instance;

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[domain]");

report_instance make_report_instance(const std::string& name) {
    report_instance ri;
    ri.version = 1;
    ri.id = boost::uuids::random_generator()();
    ri.name = name;
    ri.description = name + " instance";
    ri.party_id = boost::uuids::random_generator()();
    ri.definition_id = boost::uuids::random_generator()();
    ri.trigger_run_id = 1;
    ri.output_message = "";
    ri.modified_by = "system";
    ri.performed_by = "system";
    ri.change_reason_code = "system.new";
    ri.change_commentary = "Test data";
    ri.recorded_at = std::chrono::system_clock::now();
    return ri;
}

}

using ores::reporting::domain::report_instance;
using namespace ores::logging;

TEST_CASE("create_report_instance_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    report_instance sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = "Daily Risk Report";
    sut.description = "Execution of daily risk report";
    sut.party_id = boost::uuids::random_generator()();
    sut.definition_id = boost::uuids::random_generator()();
    sut.trigger_run_id = 42;
    sut.output_message = "";
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Instance created by scheduler";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Report instance: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(sut.name == "Daily Risk Report");
    CHECK(!sut.definition_id.is_nil());
    CHECK(sut.trigger_run_id == 42);
    CHECK(!sut.fsm_state_id.has_value());
    CHECK(!sut.started_at.has_value());
    CHECK(!sut.completed_at.has_value());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_report_instance_with_optional_fields", tags) {
    auto lg(make_logger(test_suite));

    const auto now = std::chrono::system_clock::now();

    report_instance sut;
    sut.version = 2;
    sut.id = boost::uuids::random_generator()();
    sut.name = "Weekly Grid Report";
    sut.description = "Completed execution";
    sut.party_id = boost::uuids::random_generator()();
    sut.definition_id = boost::uuids::random_generator()();
    sut.fsm_state_id = boost::uuids::random_generator()();
    sut.trigger_run_id = 100;
    sut.output_message = "Report generated successfully";
    sut.started_at = now - std::chrono::minutes(5);
    sut.completed_at = now;
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.complete";
    sut.change_commentary = "Completed";
    sut.recorded_at = now;
    BOOST_LOG_SEV(lg, info) << "Report instance with optionals: " << sut;

    CHECK(sut.fsm_state_id.has_value());
    CHECK(!sut.fsm_state_id->is_nil());
    CHECK(sut.started_at.has_value());
    CHECK(sut.completed_at.has_value());
    CHECK(sut.output_message == "Report generated successfully");
}

TEST_CASE("report_instance_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    report_instance sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = "Test Report";
    sut.description = "Test description";
    sut.party_id = boost::uuids::random_generator()();
    sut.definition_id = boost::uuids::random_generator()();
    sut.trigger_run_id = 1;
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Report instance: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("Test Report") != std::string::npos);
}

TEST_CASE("create_report_instance_with_faker", tags) {
    auto lg(make_logger(test_suite));

    report_instance sut;
    sut.version = faker::number::integer(1, 10);
    sut.id = boost::uuids::random_generator()();
    sut.name = std::string(faker::word::adjective()) + " Report";
    sut.description = std::string(faker::lorem::sentence());
    sut.party_id = boost::uuids::random_generator()();
    sut.definition_id = boost::uuids::random_generator()();
    sut.trigger_run_id = faker::number::integer(1, 1000);
    sut.output_message = "";
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Report instance: " << sut;

    CHECK(sut.version >= 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_report_instances", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> names = {"Daily Risk", "Weekly Grid",
        "Monthly Summary"};
    for (const auto& name : names) {
        auto sut = make_report_instance(name);
        BOOST_LOG_SEV(lg, info) << "Report instance: " << sut;
        CHECK(!sut.id.is_nil());
        CHECK(sut.version == 1);
    }
}

TEST_CASE("report_instance_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_instance> items = {make_report_instance("Daily Risk")};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("report_instance_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_instance> items;
    for (int i = 0; i < 3; ++i)
        items.push_back(make_report_instance("Report " + std::to_string(i)));

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("report_instance_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_instance> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("report_instance_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_instance> items;
    for (int i = 0; i < 5; ++i) {
        report_instance ri;
        ri.version = 1;
        ri.id = boost::uuids::random_generator()();
        ri.name = std::string(faker::word::adjective()) + " Report " + std::to_string(i);
        ri.description = std::string(faker::lorem::sentence());
        ri.party_id = boost::uuids::random_generator()();
        ri.definition_id = boost::uuids::random_generator()();
        ri.trigger_run_id = i + 1;
        ri.output_message = "";
        ri.modified_by = "system";
        ri.performed_by = "system";
        ri.change_reason_code = "system.new";
        ri.change_commentary = "Test";
        ri.recorded_at = std::chrono::system_clock::now();
        items.push_back(ri);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
}
