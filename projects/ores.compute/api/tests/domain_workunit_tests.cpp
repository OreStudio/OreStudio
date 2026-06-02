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
#include "ores.compute.api/domain/workunit.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/workunit_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.api/domain/workunit_table.hpp"

namespace {

const std::string_view test_suite("ores.compute.tests");
const std::string tags("[domain]");

}

using ores::compute::domain::workunit;
using namespace ores::compute::domain;
using namespace ores::logging;

TEST_CASE("create_workunit_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    workunit sut;
    sut.version = 1;
    sut.modified_by = "scheduler";
    sut.id = boost::uuids::random_generator()();
    sut.batch_id = boost::uuids::random_generator()();
    sut.app_version_id = boost::uuids::random_generator()();
    sut.input_uri = "s3://bucket/inputs/trade-portfolio-2026.zip";
    sut.config_uri = "s3://bucket/configs/ore-config.xml";
    sut.priority = 10;
    sut.target_redundancy = 2;

    BOOST_LOG_SEV(lg, info) << "Workunit: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "scheduler");
    CHECK(sut.input_uri == "s3://bucket/inputs/trade-portfolio-2026.zip");
    CHECK(sut.config_uri == "s3://bucket/configs/ore-config.xml");
    CHECK(sut.priority == 10);
    CHECK(sut.target_redundancy == 2);
}

TEST_CASE("create_high_priority_workunit", tags) {
    auto lg(make_logger(test_suite));

    workunit sut;
    sut.version = 1;
    sut.modified_by = "admin";
    sut.id = boost::uuids::random_generator()();
    sut.batch_id = boost::uuids::random_generator()();
    sut.app_version_id = boost::uuids::random_generator()();
    sut.input_uri = "s3://bucket/inputs/urgent-run.zip";
    sut.config_uri = "s3://bucket/configs/stress-test.xml";
    sut.priority = 100;
    sut.target_redundancy = 3;

    BOOST_LOG_SEV(lg, info) << "Workunit: " << sut;

    CHECK(sut.priority == 100);
    CHECK(sut.target_redundancy == 3);
}

TEST_CASE("create_workunit_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440030");

    workunit sut;
    sut.version = 2;
    sut.modified_by = "updater";
    sut.id = specific_id;
    sut.batch_id = boost::uuids::random_generator()();
    sut.app_version_id = boost::uuids::random_generator()();
    sut.input_uri = "s3://bucket/inputs/data.zip";
    sut.config_uri = "s3://bucket/configs/config.xml";
    sut.priority = 5;
    sut.target_redundancy = 1;

    BOOST_LOG_SEV(lg, info) << "Workunit: " << sut;

    CHECK(sut.version == 2);
    CHECK(sut.priority == 5);
}

TEST_CASE("workunit_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    workunit sut;
    sut.version = 1;
    sut.modified_by = "developer";
    sut.id = boost::uuids::random_generator()();
    sut.batch_id = boost::uuids::random_generator()();
    sut.app_version_id = boost::uuids::random_generator()();
    sut.input_uri = "s3://bucket/inputs/serialization-test.zip";
    sut.config_uri = "s3://bucket/configs/test.xml";
    sut.priority = 1;
    sut.target_redundancy = 1;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "Workunit JSON: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("serialization-test") != std::string::npos);
}

TEST_CASE("workunit_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    workunit wu;
    wu.version = 1;
    wu.modified_by = "admin";
    wu.id = boost::uuids::random_generator()();
    wu.batch_id = boost::uuids::random_generator()();
    wu.app_version_id = boost::uuids::random_generator()();
    wu.input_uri = "s3://bucket/inputs/portfolio.zip";
    wu.config_uri = "s3://bucket/configs/ore.xml";
    wu.priority = 10;
    wu.target_redundancy = 2;

    std::vector<workunit> workunits = {wu};
    auto table = convert_to_table(workunits);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("workunit_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<workunit> workunits;
    for (int i = 0; i < 3; ++i) {
        workunit wu;
        wu.version = i + 1;
        wu.modified_by = "system";
        wu.id = boost::uuids::random_generator()();
        wu.batch_id = boost::uuids::random_generator()();
        wu.app_version_id = boost::uuids::random_generator()();
        wu.input_uri = "s3://bucket/inputs/run-" + std::to_string(i) + ".zip";
        wu.config_uri = "s3://bucket/configs/config-" + std::to_string(i) + ".xml";
        wu.priority = i + 1;
        wu.target_redundancy = 1;
        workunits.push_back(wu);
    }

    auto table = convert_to_table(workunits);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("workunit_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<workunit> workunits;
    auto table = convert_to_table(workunits);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("create_workunit_with_faker", tags) {
    auto lg(make_logger(test_suite));

    workunit sut;
    sut.version = faker::number::integer(1, 10);
    sut.modified_by = std::string(faker::internet::username());
    sut.id = boost::uuids::random_generator()();
    sut.batch_id = boost::uuids::random_generator()();
    sut.app_version_id = boost::uuids::random_generator()();
    sut.input_uri = "s3://bucket/inputs/" + std::string(faker::word::noun()) + ".zip";
    sut.config_uri = "s3://bucket/configs/" + std::string(faker::word::noun()) + ".xml";
    sut.priority = faker::number::integer(1, 100);
    sut.target_redundancy = faker::number::integer(1, 5);

    BOOST_LOG_SEV(lg, info) << "Workunit: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.input_uri.empty());
    CHECK(!sut.config_uri.empty());
    CHECK(sut.priority >= 1);
    CHECK(sut.target_redundancy >= 1);
}
