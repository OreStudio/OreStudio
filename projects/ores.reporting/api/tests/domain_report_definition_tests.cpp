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
#include "ores.reporting.api/domain/report_definition.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/domain/report_definition_table.hpp"
#include "ores.reporting.api/domain/report_definition_table_io.hpp" // IWYU pragma: keep.

namespace {

using ores::reporting::domain::report_definition;

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[domain]");

report_definition make_report_definition(const std::string& name,
    const std::string& report_type = "risk") {
    report_definition rd;
    rd.version = 1;
    rd.id = boost::uuids::random_generator()();
    rd.name = name;
    rd.party_id = boost::uuids::random_generator()();
    rd.description = name + " report definition";
    rd.report_type = report_type;
    rd.schedule_expression = "0 6 * * 1-5";
    rd.concurrency_policy = "skip";
    rd.modified_by = "system";
    rd.performed_by = "system";
    rd.change_reason_code = "system.new";
    rd.change_commentary = "Test data";
    rd.recorded_at = std::chrono::system_clock::now();
    return rd;
}

}

using ores::reporting::domain::report_definition;
using namespace ores::logging;

TEST_CASE("create_report_definition_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    report_definition sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = "Daily Risk Report";
    sut.party_id = boost::uuids::random_generator()();
    sut.description = "Daily risk calculation";
    sut.report_type = "risk";
    sut.schedule_expression = "0 6 * * 1-5";
    sut.concurrency_policy = "skip";
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Initial creation";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Report definition: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(sut.name == "Daily Risk Report");
    CHECK(sut.report_type == "risk");
    CHECK(sut.schedule_expression == "0 6 * * 1-5");
    CHECK(sut.concurrency_policy == "skip");
    CHECK(!sut.fsm_state_id.has_value());
    CHECK(!sut.scheduler_job_id.has_value());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_report_definition_with_optional_fields", tags) {
    auto lg(make_logger(test_suite));

    report_definition sut;
    sut.version = 2;
    sut.id = boost::uuids::random_generator()();
    sut.name = "Weekly Grid Report";
    sut.party_id = boost::uuids::random_generator()();
    sut.description = "Weekly grid sensitivity";
    sut.report_type = "grid";
    sut.fsm_state_id = boost::uuids::random_generator()();
    sut.schedule_expression = "0 8 * * 1";
    sut.concurrency_policy = "queue";
    sut.scheduler_job_id = boost::uuids::random_generator()();
    sut.modified_by = "admin";
    sut.performed_by = "admin";
    sut.change_reason_code = "system.update";
    sut.change_commentary = "Activated";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Report definition with optionals: " << sut;

    CHECK(sut.fsm_state_id.has_value());
    CHECK(!sut.fsm_state_id->is_nil());
    CHECK(sut.scheduler_job_id.has_value());
    CHECK(!sut.scheduler_job_id->is_nil());
}

TEST_CASE("report_definition_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    report_definition sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = "Monthly Summary";
    sut.party_id = boost::uuids::random_generator()();
    sut.report_type = "risk";
    sut.schedule_expression = "0 9 1 * *";
    sut.concurrency_policy = "fail";
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Report definition: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("Monthly Summary") != std::string::npos);
}

TEST_CASE("create_report_definition_with_faker", tags) {
    auto lg(make_logger(test_suite));

    report_definition sut;
    sut.version = faker::number::integer(1, 10);
    sut.id = boost::uuids::random_generator()();
    sut.name = std::string(faker::word::adjective()) + " Report";
    sut.party_id = boost::uuids::random_generator()();
    sut.description = std::string(faker::lorem::sentence());
    sut.report_type = std::string(faker::word::noun());
    sut.schedule_expression = "0 6 * * 1-5";
    sut.concurrency_policy = "skip";
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Report definition: " << sut;

    CHECK(sut.version >= 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_report_definitions", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::pair<std::string, std::string>> defs = {
        {"Daily Risk", "risk"}, {"Weekly Grid", "grid"},
        {"Monthly Summary", "risk"}};
    for (const auto& [name, type] : defs) {
        auto sut = make_report_definition(name, type);
        BOOST_LOG_SEV(lg, info) << "Report definition: " << sut;
        CHECK(!sut.id.is_nil());
        CHECK(sut.version == 1);
    }
}

TEST_CASE("report_definition_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_definition> items = {
        make_report_definition("Daily Risk Report")};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("report_definition_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_definition> items;
    for (int i = 0; i < 3; ++i)
        items.push_back(make_report_definition("Report " + std::to_string(i)));

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("report_definition_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_definition> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("report_definition_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_definition> items;
    for (int i = 0; i < 5; ++i) {
        report_definition rd;
        rd.version = 1;
        rd.id = boost::uuids::random_generator()();
        rd.name = std::string(faker::word::adjective()) + " Report " + std::to_string(i);
        rd.party_id = boost::uuids::random_generator()();
        rd.description = std::string(faker::lorem::sentence());
        rd.report_type = std::string(faker::word::noun());
        rd.schedule_expression = "0 6 * * 1-5";
        rd.concurrency_policy = "skip";
        rd.modified_by = "system";
        rd.performed_by = "system";
        rd.change_reason_code = "system.new";
        rd.change_commentary = "Test";
        rd.recorded_at = std::chrono::system_clock::now();
        items.push_back(rd);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
}
