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
#include "ores.compute.api/domain/result.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/result_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.api/domain/result_table.hpp"

namespace {

const std::string_view test_suite("ores.compute.tests");
const std::string tags("[domain]");

// Server state codes: 1=Inactive, 2=Unsent, 4=InProgress, 5=Done
// Outcome codes: 1=Success, 3=ClientError, 4=NoReply

}

using ores::compute::domain::result;
using namespace ores::compute::domain;
using namespace ores::logging;

TEST_CASE("create_result_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    result sut;
    sut.version = 1;
    sut.modified_by = "assimilator";
    sut.id = boost::uuids::random_generator()();
    sut.workunit_id = boost::uuids::random_generator()();
    sut.host_id = boost::uuids::random_generator()();
    sut.pgmq_msg_id = 42;
    sut.server_state = 5; // Done
    sut.outcome = 1; // Success
    sut.output_uri = "s3://bucket/outputs/result-001.zip";
    sut.received_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Result: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "assimilator");
    CHECK(sut.server_state == 5);
    CHECK(sut.outcome == 1);
    CHECK(sut.output_uri == "s3://bucket/outputs/result-001.zip");
}

TEST_CASE("create_inactive_result", tags) {
    auto lg(make_logger(test_suite));

    result sut;
    sut.version = 1;
    sut.modified_by = "scheduler";
    sut.id = boost::uuids::random_generator()();
    sut.workunit_id = boost::uuids::random_generator()();
    sut.host_id = boost::uuids::nil_uuid();
    sut.pgmq_msg_id = 0;
    sut.server_state = 1; // Inactive
    sut.outcome = 0;
    sut.output_uri = "";
    sut.received_at = std::chrono::system_clock::time_point{};

    BOOST_LOG_SEV(lg, info) << "Result: " << sut;

    CHECK(sut.server_state == 1);
    CHECK(sut.output_uri.empty());
}

TEST_CASE("create_in_progress_result", tags) {
    auto lg(make_logger(test_suite));

    result sut;
    sut.version = 1;
    sut.modified_by = "dispatcher";
    sut.id = boost::uuids::random_generator()();
    sut.workunit_id = boost::uuids::random_generator()();
    sut.host_id = boost::uuids::random_generator()();
    sut.pgmq_msg_id = 789;
    sut.server_state = 4; // InProgress
    sut.outcome = 0;
    sut.output_uri = "";
    sut.received_at = std::chrono::system_clock::time_point{};

    BOOST_LOG_SEV(lg, info) << "Result: " << sut;

    CHECK(sut.server_state == 4);
    CHECK(sut.pgmq_msg_id == 789);
}

TEST_CASE("create_result_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440040");

    result sut;
    sut.version = 2;
    sut.modified_by = "validator";
    sut.id = specific_id;
    sut.workunit_id = boost::uuids::random_generator()();
    sut.host_id = boost::uuids::random_generator()();
    sut.pgmq_msg_id = 0;
    sut.server_state = 5;
    sut.outcome = 1;
    sut.output_uri = "s3://bucket/outputs/validated.zip";
    sut.received_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Result: " << sut;

    CHECK(sut.version == 2);
    CHECK(sut.outcome == 1);
}

TEST_CASE("result_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    result sut;
    sut.version = 1;
    sut.modified_by = "developer";
    sut.id = boost::uuids::random_generator()();
    sut.workunit_id = boost::uuids::random_generator()();
    sut.host_id = boost::uuids::random_generator()();
    sut.pgmq_msg_id = 100;
    sut.server_state = 5;
    sut.outcome = 1;
    sut.output_uri = "s3://bucket/outputs/serialization-test.zip";
    sut.received_at = std::chrono::system_clock::now();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "Result JSON: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("serialization-test") != std::string::npos);
}

TEST_CASE("result_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    result r;
    r.version = 1;
    r.modified_by = "admin";
    r.id = boost::uuids::random_generator()();
    r.workunit_id = boost::uuids::random_generator()();
    r.host_id = boost::uuids::random_generator()();
    r.pgmq_msg_id = 0;
    r.server_state = 5;
    r.outcome = 1;
    r.output_uri = "s3://bucket/outputs/result.zip";
    r.received_at = std::chrono::system_clock::now();

    std::vector<result> results = {r};
    auto table = convert_to_table(results);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("result_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<result> results;
    for (int i = 0; i < 3; ++i) {
        result r;
        r.version = i + 1;
        r.modified_by = "system";
        r.id = boost::uuids::random_generator()();
        r.workunit_id = boost::uuids::random_generator()();
        r.host_id = boost::uuids::random_generator()();
        r.pgmq_msg_id = i;
        r.server_state = 5;
        r.outcome = 1;
        r.output_uri = "s3://bucket/outputs/result-" + std::to_string(i) + ".zip";
        r.received_at = std::chrono::system_clock::now();
        results.push_back(r);
    }

    auto table = convert_to_table(results);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("result_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<result> results;
    auto table = convert_to_table(results);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("create_result_with_faker", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<int> states = {1, 2, 4, 5};
    const std::vector<int> outcomes = {0, 1, 3, 4};

    result sut;
    sut.version = faker::number::integer(1, 10);
    sut.modified_by = std::string(faker::internet::username());
    sut.id = boost::uuids::random_generator()();
    sut.workunit_id = boost::uuids::random_generator()();
    sut.host_id = boost::uuids::random_generator()();
    sut.pgmq_msg_id = faker::number::integer(0, 10000);
    sut.server_state = states[faker::number::integer(0, 3)];
    sut.outcome = outcomes[faker::number::integer(0, 3)];
    sut.output_uri = "s3://bucket/outputs/" + std::string(faker::word::noun()) + ".zip";
    sut.received_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Result: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.output_uri.empty());
}
