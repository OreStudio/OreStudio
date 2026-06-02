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
#include "ores.compute.api/domain/app.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/app_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.api/domain/app_table.hpp"

namespace {

const std::string_view test_suite("ores.compute.tests");
const std::string tags("[domain]");

}

using ores::compute::domain::app;
using namespace ores::compute::domain;
using namespace ores::logging;

TEST_CASE("create_app_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    app sut;
    sut.version = 1;
    sut.modified_by = "admin";
    sut.id = boost::uuids::random_generator()();
    sut.name = "ORE_STUDIO";
    sut.description = "ORE Studio financial computation engine";

    BOOST_LOG_SEV(lg, info) << "App: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "admin");
    CHECK(sut.name == "ORE_STUDIO");
    CHECK(sut.description == "ORE Studio financial computation engine");
}

TEST_CASE("create_app_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440000");

    app sut;
    sut.version = 2;
    sut.modified_by = "updater";
    sut.id = specific_id;
    sut.name = "LLAMA_CPP";
    sut.description = "Llama inference engine";

    BOOST_LOG_SEV(lg, info) << "App: " << sut;

    CHECK(sut.version == 2);
    CHECK(sut.name == "LLAMA_CPP");
}

TEST_CASE("app_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    app sut;
    sut.version = 1;
    sut.modified_by = "developer";
    sut.id = boost::uuids::random_generator()();
    sut.name = "LEDGER";
    sut.description = "Ledger accounting engine";

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "App JSON: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("LEDGER") != std::string::npos);
}

TEST_CASE("app_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    app a;
    a.version = 1;
    a.modified_by = "admin";
    a.id = boost::uuids::random_generator()();
    a.name = "ORE_STUDIO";
    a.description = "ORE Studio engine";

    std::vector<app> apps = {a};
    auto table = convert_to_table(apps);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("ORE_STUDIO") != std::string::npos);
}

TEST_CASE("app_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<app> apps;
    const std::vector<std::string> names = {"ORE_STUDIO", "LLAMA_CPP", "LEDGER"};
    for (int i = 0; i < 3; ++i) {
        app a;
        a.version = i + 1;
        a.modified_by = "system";
        a.id = boost::uuids::random_generator()();
        a.name = names[i];
        a.description = "Engine " + std::to_string(i);
        apps.push_back(a);
    }

    auto table = convert_to_table(apps);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("ORE_STUDIO") != std::string::npos);
    CHECK(table.find("LLAMA_CPP") != std::string::npos);
    CHECK(table.find("LEDGER") != std::string::npos);
}

TEST_CASE("app_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<app> apps;
    auto table = convert_to_table(apps);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("create_app_with_faker", tags) {
    auto lg(make_logger(test_suite));

    app sut;
    sut.version = faker::number::integer(1, 10);
    sut.modified_by = std::string(faker::internet::username());
    sut.id = boost::uuids::random_generator()();
    sut.name = std::string(faker::word::noun());
    sut.description = std::string(faker::lorem::sentence());

    BOOST_LOG_SEV(lg, info) << "App: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
}

TEST_CASE("app_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<app> apps;
    for (int i = 0; i < 5; ++i) {
        app a;
        a.version = faker::number::integer(1, 10);
        a.modified_by = std::string(faker::internet::username());
        a.id = boost::uuids::random_generator()();
        a.name = std::string(faker::word::noun());
        a.description = std::string(faker::lorem::sentence());
        apps.push_back(a);
    }

    auto table = convert_to_table(apps);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& a : apps) {
        CHECK(table.find(a.name) != std::string::npos);
    }
}
