/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.connections/domain/folder.hpp"
#include "ores.connections/domain/folder_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/domain/folder_table_io.hpp" // IWYU pragma: keep.

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace {

const std::string test_suite("ores.connections.tests");
const std::string tags("[domain]");

}

using namespace ores::connections::domain;
using namespace ores::logging;
using ores::utility::uuid::uuid_v7_generator;

TEST_CASE("create_folder_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    folder sut;
    sut.id = gen();
    sut.name = "Development";
    sut.parent_id = std::nullopt;

    BOOST_LOG_SEV(lg, info) << "folder: " << sut;

    CHECK(sut.name == "Development");
    CHECK(!sut.parent_id.has_value());
}

TEST_CASE("create_folder_with_parent", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    const auto parent_id = gen();

    folder sut;
    sut.id = gen();
    sut.name = "Staging";
    sut.parent_id = parent_id;

    BOOST_LOG_SEV(lg, info) << "folder with parent: " << sut;

    CHECK(sut.name == "Staging");
    CHECK(sut.parent_id.has_value());
    CHECK(sut.parent_id.value() == parent_id);
}

TEST_CASE("folder_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    folder sut;
    sut.id = gen();
    sut.name = "Production";
    sut.parent_id = std::nullopt;

    BOOST_LOG_SEV(lg, info) << "folder: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("Production") != std::string::npos);
    CHECK(json_output.find("id") != std::string::npos);
    CHECK(json_output.find("name") != std::string::npos);
}

TEST_CASE("create_folder_with_faker", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    folder sut;
    sut.id = gen();
    sut.name = std::string(faker::word::noun());
    sut.parent_id = std::nullopt;

    BOOST_LOG_SEV(lg, info) << "folder with faker data: " << sut;

    CHECK(!sut.name.empty());
}

TEST_CASE("folder_table_output", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    std::vector<folder> folders;

    folder root;
    root.id = gen();
    root.name = "Root";
    root.parent_id = std::nullopt;
    folders.push_back(root);

    folder child;
    child.id = gen();
    child.name = "Child";
    child.parent_id = root.id;
    folders.push_back(child);

    BOOST_LOG_SEV(lg, info) << "folders table:\n" << folders;

    std::ostringstream os;
    os << folders;
    const std::string table_output = os.str();

    CHECK(!table_output.empty());
    CHECK(table_output.find("Root") != std::string::npos);
    CHECK(table_output.find("Child") != std::string::npos);
    CHECK(table_output.find("(root)") != std::string::npos);
}
