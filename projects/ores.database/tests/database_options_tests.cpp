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
#include "ores.database/domain/database_options.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.database.tests");
const std::string tags("[database]");

}

using namespace ores::database;
using namespace ores::logging;

TEST_CASE("database_options_to_credentials", tags) {
    auto lg(make_logger(test_suite));

    database_options opts;
    opts.host = "localhost";
    opts.port = 5432;
    opts.database = "test_db";
    opts.user = "test_user";
    opts.password = "test_pass";

    auto creds = to_credentials(opts);

    REQUIRE(creds.host == "localhost");
    REQUIRE(creds.port == 5432);
    REQUIRE(creds.dbname == "test_db");
    REQUIRE(creds.user == "test_user");
    REQUIRE(creds.password == "test_pass");

    BOOST_LOG_SEV(lg, info) << "Database options to credentials test passed";
}

TEST_CASE("database_options_default_values", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing database_options default values";

    database_options sut;

    CHECK(sut.host == "localhost");
    CHECK(sut.port == 5432);
    CHECK(sut.user.empty());
    CHECK(sut.database.empty());
}

TEST_CASE("database_options_custom_values", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing database_options with custom values";

    database_options sut;
    sut.host = "db.production.example.com";
    sut.port = 5433;
    sut.database = "production_db";
    sut.user = "app_user";
    sut.password = "secret123";

    CHECK(sut.host == "db.production.example.com");
    CHECK(sut.port == 5433);
    CHECK(sut.database == "production_db");
    CHECK(sut.user == "app_user");
}

TEST_CASE("database_options_streaming", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing database_options streaming";

    database_options sut;
    sut.host = "test-host";
    sut.port = 5432;
    sut.database = "testdb";
    sut.user = "testuser";
    sut.password = "testpass";

    std::ostringstream os;
    os << sut;
    const std::string output = os.str();

    BOOST_LOG_SEV(lg, debug) << "Output: " << output;

    CHECK(!output.empty());
    CHECK(output.find("test-host") != std::string::npos);
    CHECK(output.find("testdb") != std::string::npos);
    CHECK(output.find("testuser") != std::string::npos);
}

TEST_CASE("database_options_to_credentials_with_defaults", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing to_credentials with default host/port";

    database_options opts;
    opts.database = "mydb";
    opts.user = "myuser";
    opts.password = "mypass";

    auto creds = to_credentials(opts);

    CHECK(creds.host == "localhost");
    CHECK(creds.port == 5432);
    CHECK(creds.dbname == "mydb");
    CHECK(creds.user == "myuser");
    CHECK(creds.password == "mypass");
}

TEST_CASE("database_options_to_credentials_different_port", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing to_credentials with non-standard port";

    database_options opts;
    opts.host = "remote-db.example.com";
    opts.port = 5433;
    opts.database = "remotedb";
    opts.user = "remoteuser";
    opts.password = "remotepass";

    auto creds = to_credentials(opts);

    CHECK(creds.host == "remote-db.example.com");
    CHECK(creds.port == 5433);
    CHECK(creds.dbname == "remotedb");
}
