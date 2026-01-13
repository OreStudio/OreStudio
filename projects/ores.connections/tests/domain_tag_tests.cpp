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
#include "ores.connections/domain/tag.hpp"
#include "ores.connections/domain/tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/domain/tag_table_io.hpp" // IWYU pragma: keep.

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

TEST_CASE("create_tag_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    tag sut;
    sut.id = gen();
    sut.name = "production";

    BOOST_LOG_SEV(lg, info) << "tag: " << sut;

    CHECK(sut.name == "production");
}

TEST_CASE("tag_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    tag sut;
    sut.id = gen();
    sut.name = "staging";

    BOOST_LOG_SEV(lg, info) << "tag: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("staging") != std::string::npos);
    CHECK(json_output.find("id") != std::string::npos);
    CHECK(json_output.find("name") != std::string::npos);
}

TEST_CASE("create_tag_with_faker", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    tag sut;
    sut.id = gen();
    sut.name = std::string(faker::word::adjective());

    BOOST_LOG_SEV(lg, info) << "tag with faker data: " << sut;

    CHECK(!sut.name.empty());
}

TEST_CASE("tag_table_output", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    std::vector<tag> tags_list;

    tag t1;
    t1.id = gen();
    t1.name = "development";
    tags_list.push_back(t1);

    tag t2;
    t2.id = gen();
    t2.name = "production";
    tags_list.push_back(t2);

    tag t3;
    t3.id = gen();
    t3.name = "critical";
    tags_list.push_back(t3);

    BOOST_LOG_SEV(lg, info) << "tags table:\n" << tags_list;

    std::ostringstream os;
    os << tags_list;
    const std::string table_output = os.str();

    CHECK(!table_output.empty());
    CHECK(table_output.find("development") != std::string::npos);
    CHECK(table_output.find("production") != std::string::npos);
    CHECK(table_output.find("critical") != std::string::npos);
}
