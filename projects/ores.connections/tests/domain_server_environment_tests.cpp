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
#include "ores.connections/domain/server_environment.hpp"
#include "ores.connections/domain/server_environment_json_io.hpp" // IWYU pragma: keep.
#include "ores.connections/domain/server_environment_table_io.hpp" // IWYU pragma: keep.

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

TEST_CASE("create_server_environment_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    server_environment sut;
    sut.id = gen();
    sut.folder_id = std::nullopt;
    sut.name = "Local Development";
    sut.host = "localhost";
    sut.port = 5432;
    sut.username = "admin";
    sut.encrypted_password = "encrypted_data_here";
    sut.description = "Local development server";

    BOOST_LOG_SEV(lg, info) << "server_environment: " << sut;

    CHECK(sut.name == "Local Development");
    CHECK(sut.host == "localhost");
    CHECK(sut.port == 5432);
    CHECK(sut.username == "admin");
    CHECK(!sut.encrypted_password.empty());
    CHECK(!sut.folder_id.has_value());
}

TEST_CASE("create_server_environment_with_folder", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    const auto folder_id = gen();

    server_environment sut;
    sut.id = gen();
    sut.folder_id = folder_id;
    sut.name = "Production Server";
    sut.host = "prod.example.com";
    sut.port = 5432;
    sut.username = "produser";
    sut.encrypted_password = "encrypted_prod_password";
    sut.description = "Main production database";

    BOOST_LOG_SEV(lg, info) << "server_environment with folder: " << sut;

    CHECK(sut.folder_id.has_value());
    CHECK(sut.folder_id.value() == folder_id);
}

TEST_CASE("server_environment_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    server_environment sut;
    sut.id = gen();
    sut.folder_id = std::nullopt;
    sut.name = "Test Server";
    sut.host = "test.example.com";
    sut.port = 3306;
    sut.username = "testuser";
    sut.encrypted_password = "encrypted_test";
    sut.description = "Test environment";

    BOOST_LOG_SEV(lg, info) << "server_environment: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("Test Server") != std::string::npos);
    CHECK(json_output.find("test.example.com") != std::string::npos);
    CHECK(json_output.find("3306") != std::string::npos);
    CHECK(json_output.find("testuser") != std::string::npos);
}

TEST_CASE("create_server_environment_with_faker", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    server_environment sut;
    sut.id = gen();
    sut.folder_id = std::nullopt;
    sut.name = std::string(faker::word::noun()) + " Server";
    sut.host = std::string(faker::internet::domainName());
    sut.port = faker::number::integer(1024, 65535);
    sut.username = std::string(faker::internet::username());
    sut.encrypted_password = std::string(faker::string::alphanumeric(32));
    sut.description = std::string(faker::lorem::sentence());

    BOOST_LOG_SEV(lg, info) << "server_environment with faker data: " << sut;

    CHECK(!sut.name.empty());
    CHECK(!sut.host.empty());
    CHECK(sut.port >= 1024);
    CHECK(sut.port <= 65535);
    CHECK(!sut.username.empty());
    CHECK(!sut.encrypted_password.empty());
}

TEST_CASE("server_environment_table_output", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    std::vector<server_environment> envs;

    server_environment env1;
    env1.id = gen();
    env1.folder_id = std::nullopt;
    env1.name = "Dev";
    env1.host = "localhost";
    env1.port = 5432;
    env1.username = "dev";
    env1.encrypted_password = "encrypted1";
    env1.description = "Development";
    envs.push_back(env1);

    server_environment env2;
    env2.id = gen();
    env2.folder_id = std::nullopt;
    env2.name = "Prod";
    env2.host = "prod.example.com";
    env2.port = 5432;
    env2.username = "prod";
    env2.encrypted_password = "";  // No password
    env2.description = "Production";
    envs.push_back(env2);

    BOOST_LOG_SEV(lg, info) << "server_environments table:\n" << envs;

    std::ostringstream os;
    os << envs;
    const std::string table_output = os.str();

    CHECK(!table_output.empty());
    CHECK(table_output.find("Dev") != std::string::npos);
    CHECK(table_output.find("Prod") != std::string::npos);
    CHECK(table_output.find("Y") != std::string::npos);  // Has password
    CHECK(table_output.find("N") != std::string::npos);  // No password
}

TEST_CASE("server_environment_with_empty_password", tags) {
    auto lg(make_logger(test_suite));
    uuid_v7_generator gen;

    server_environment sut;
    sut.id = gen();
    sut.folder_id = std::nullopt;
    sut.name = "Passwordless";
    sut.host = "localhost";
    sut.port = 5432;
    sut.username = "nopass";
    sut.encrypted_password = "";
    sut.description = "Server without password";

    BOOST_LOG_SEV(lg, info) << "server_environment without password: " << sut;

    CHECK(sut.encrypted_password.empty());
}
