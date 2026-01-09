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
#include "ores.database/service/context_factory.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.database.tests");
const std::string tags("[context_factory]");

}

using namespace ores::database;
using namespace ores::logging;

TEST_CASE("context_factory_configuration_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing context_factory::configuration construction";

    context_factory::configuration sut;
    sut.database_options.host = "localhost";
    sut.database_options.port = 5432;
    sut.database_options.database = "test_db";
    sut.database_options.user = "test_user";
    sut.database_options.password = "test_pass";
    sut.pool_size = 10;
    sut.num_attempts = 3;
    sut.wait_time_in_seconds = 5;

    CHECK(sut.database_options.host == "localhost");
    CHECK(sut.database_options.port == 5432);
    CHECK(sut.database_options.database == "test_db");
    CHECK(sut.pool_size == 10);
    CHECK(sut.num_attempts == 3);
    CHECK(sut.wait_time_in_seconds == 5);
}

TEST_CASE("context_factory_configuration_streaming", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing context_factory::configuration streaming";

    context_factory::configuration sut;
    sut.database_options.host = "db.example.com";
    sut.database_options.port = 5432;
    sut.database_options.database = "production";
    sut.database_options.user = "app_user";
    sut.database_options.password = "secret";
    sut.pool_size = 20;
    sut.num_attempts = 5;
    sut.wait_time_in_seconds = 10;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, debug) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("db.example.com") != std::string::npos);
    CHECK(json_output.find("production") != std::string::npos);
    CHECK(json_output.find("app_user") != std::string::npos);
    CHECK(json_output.find("20") != std::string::npos);
}

TEST_CASE("context_factory_configuration_default_values", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing context_factory::configuration default values";

    context_factory::configuration sut{};

    CHECK(sut.pool_size == 0);
    CHECK(sut.num_attempts == 0);
    CHECK(sut.wait_time_in_seconds == 0);
    CHECK(sut.database_options.host.empty());
}
