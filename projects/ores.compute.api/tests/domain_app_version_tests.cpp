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
#include "ores.compute.api/domain/app_version.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/app_version_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.api/domain/app_version_table.hpp"

namespace {

const std::string_view test_suite("ores.compute.tests");
const std::string tags("[domain]");

}

using ores::compute::domain::app_version;
using namespace ores::compute::domain;
using namespace ores::logging;

TEST_CASE("create_app_version_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    app_version sut;
    sut.version = 1;
    sut.modified_by = "admin";
    sut.id = boost::uuids::random_generator()();
    sut.app_id = boost::uuids::random_generator()();
    sut.wrapper_version = "v1.2.0";
    sut.engine_version = "ORE-Studio-7.1";
    sut.min_ram_mb = 4096;

    BOOST_LOG_SEV(lg, info) << "AppVersion: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "admin");
    CHECK(sut.wrapper_version == "v1.2.0");
    CHECK(sut.engine_version == "ORE-Studio-7.1");
    CHECK(sut.min_ram_mb == 4096);
}

TEST_CASE("create_app_version_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440001");
    const auto specific_app_id = uuid_gen("550e8400-e29b-41d4-a716-446655440002");

    app_version sut;
    sut.version = 2;
    sut.modified_by = "updater";
    sut.id = specific_id;
    sut.app_id = specific_app_id;
    sut.wrapper_version = "v2.0.0";
    sut.engine_version = "ORE-Studio-8.0";
    sut.min_ram_mb = 8192;

    BOOST_LOG_SEV(lg, info) << "AppVersion: " << sut;

    CHECK(sut.version == 2);
    CHECK(sut.wrapper_version == "v2.0.0");
}

TEST_CASE("app_version_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    app_version sut;
    sut.version = 1;
    sut.modified_by = "developer";
    sut.id = boost::uuids::random_generator()();
    sut.app_id = boost::uuids::random_generator()();
    sut.wrapper_version = "v1.0.0";
    sut.engine_version = "ORE-7.0";
    sut.min_ram_mb = 2048;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "AppVersion JSON: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("v1.0.0") != std::string::npos);
}

TEST_CASE("app_version_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    app_version av;
    av.version = 1;
    av.modified_by = "admin";
    av.id = boost::uuids::random_generator()();
    av.app_id = boost::uuids::random_generator()();
    av.wrapper_version = "v1.2.0";
    av.engine_version = "ORE-Studio-7.1";
    av.min_ram_mb = 4096;

    std::vector<app_version> versions = {av};
    auto table = convert_to_table(versions);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("v1.2.0") != std::string::npos);
}

TEST_CASE("app_version_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<app_version> versions;
    for (int i = 0; i < 3; ++i) {
        app_version av;
        av.version = i + 1;
        av.modified_by = "system";
        av.id = boost::uuids::random_generator()();
        av.app_id = boost::uuids::random_generator()();
        av.wrapper_version = "v1." + std::to_string(i) + ".0";
        av.engine_version = "ORE-" + std::to_string(i);
        av.min_ram_mb = 2048 * (i + 1);
        versions.push_back(av);
    }

    auto table = convert_to_table(versions);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("v1.0.0") != std::string::npos);
    CHECK(table.find("v1.1.0") != std::string::npos);
    CHECK(table.find("v1.2.0") != std::string::npos);
}

TEST_CASE("app_version_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<app_version> versions;
    auto table = convert_to_table(versions);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("create_app_version_with_faker", tags) {
    auto lg(make_logger(test_suite));

    app_version sut;
    sut.version = faker::number::integer(1, 10);
    sut.modified_by = std::string(faker::internet::username());
    sut.id = boost::uuids::random_generator()();
    sut.app_id = boost::uuids::random_generator()();
    sut.wrapper_version = "v" + std::to_string(faker::number::integer(1, 5)) + ".0.0";
    sut.engine_version = "ORE-" + std::to_string(faker::number::integer(7, 9));
    sut.min_ram_mb = faker::number::integer(1024, 32768);

    BOOST_LOG_SEV(lg, info) << "AppVersion: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.wrapper_version.empty());
    CHECK(!sut.engine_version.empty());
    CHECK(sut.min_ram_mb >= 1024);
}
