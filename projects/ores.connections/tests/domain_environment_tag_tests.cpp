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
#include "ores.connections/domain/environment_tag.hpp"
#include "ores.connections/domain/environment_tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/domain/environment_tag_table_io.hpp" // IWYU pragma: keep.

#include <sstream>
#include <catch2/catch_test_macros.hpp>
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

TEST_CASE("create_environment_tag_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    const auto env_id = gen();
    const auto tag_id = gen();

    environment_tag sut;
    sut.environment_id = env_id;
    sut.tag_id = tag_id;

    BOOST_LOG_SEV(lg, info) << "environment_tag: " << sut;

    CHECK(sut.environment_id == env_id);
    CHECK(sut.tag_id == tag_id);
}

TEST_CASE("environment_tag_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    environment_tag sut;
    sut.environment_id = gen();
    sut.tag_id = gen();

    BOOST_LOG_SEV(lg, info) << "environment_tag: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("environment_id") != std::string::npos);
    CHECK(json_output.find("tag_id") != std::string::npos);
}

TEST_CASE("environment_tag_table_output", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    const auto env1_id = gen();
    const auto env2_id = gen();
    const auto tag1_id = gen();
    const auto tag2_id = gen();

    std::vector<environment_tag> associations;

    environment_tag et1;
    et1.environment_id = env1_id;
    et1.tag_id = tag1_id;
    associations.push_back(et1);

    environment_tag et2;
    et2.environment_id = env1_id;
    et2.tag_id = tag2_id;
    associations.push_back(et2);

    environment_tag et3;
    et3.environment_id = env2_id;
    et3.tag_id = tag1_id;
    associations.push_back(et3);

    BOOST_LOG_SEV(lg, info) << "environment_tags table:\n" << associations;

    std::ostringstream os;
    os << associations;
    const std::string table_output = os.str();

    CHECK(!table_output.empty());
    CHECK(table_output.find("Environment ID") != std::string::npos);
    CHECK(table_output.find("Tag ID") != std::string::npos);
}

TEST_CASE("multiple_tags_for_same_environment", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    const auto env_id = gen();

    std::vector<environment_tag> associations;
    for (int i = 0; i < 5; ++i) {
        environment_tag et;
        et.environment_id = env_id;
        et.tag_id = gen();
        associations.push_back(et);
    }

    BOOST_LOG_SEV(lg, info) << "multiple tags for environment:\n" << associations;

    CHECK(associations.size() == 5);
    for (const auto& et : associations) {
        CHECK(et.environment_id == env_id);
    }
}

TEST_CASE("same_tag_for_multiple_environments", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    const auto tag_id = gen();

    std::vector<environment_tag> associations;
    for (int i = 0; i < 3; ++i) {
        environment_tag et;
        et.environment_id = gen();
        et.tag_id = tag_id;
        associations.push_back(et);
    }

    BOOST_LOG_SEV(lg, info) << "same tag for multiple environments:\n" << associations;

    CHECK(associations.size() == 3);
    for (const auto& et : associations) {
        CHECK(et.tag_id == tag_id);
    }
}
