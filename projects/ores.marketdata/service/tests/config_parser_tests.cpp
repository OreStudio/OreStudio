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
#include "ores.marketdata.service/config/parser.hpp"
#include "ores.testing/scoped_environment_override.hpp"
#include <catch2/catch_test_macros.hpp>
#include <sstream>
#include <string>
#include <vector>

using ores::marketdata::service::config::parser;

TEST_CASE("parse_defaults_returns_expected_values", "[config]") {
    // Local ctest loads .env into the test process, so the shared NATS and
    // service variables are cleared to assert the compiled-in defaults.
    const ores::testing::scoped_environment_override env_guard(
        {},
        {"ORES_NATS_URL",
         "ORES_NATS_SUBJECT_PREFIX",
         "ORES_NATS_WIRE_FORMAT",
         "ORES_MARKETDATA_SERVICE_HTTP_BASE_URL"});

    std::ostringstream info, err;
    const auto result = parser{}.parse({}, info, err);

    REQUIRE(result.has_value());
    CHECK(result->nats.url == "nats://localhost:4222");
    CHECK(result->database.port == 5432);
    CHECK(result->http_base_url == "http://localhost:8080");
    CHECK_FALSE(result->logging.has_value());
}

TEST_CASE("parse_custom_http_base_url", "[config]") {
    const std::vector<std::string> args{"--http-base-url", "https://ores.example:8443"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    REQUIRE(result.has_value());
    CHECK(result->http_base_url == "https://ores.example:8443");
}

TEST_CASE("parse_custom_nats_and_database", "[config]") {
    const std::vector<std::string> args{"--nats-url",
                                        "nats://myserver:5555",
                                        "--db-host",
                                        "dbserver.internal",
                                        "--db-port",
                                        "5433"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    REQUIRE(result.has_value());
    CHECK(result->nats.url == "nats://myserver:5555");
    CHECK(result->database.host == "dbserver.internal");
    CHECK(result->database.port == 5433);
}

TEST_CASE("parse_help_prints_usage_and_returns_nothing", "[config]") {
    const std::vector<std::string> args{"--help"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    CHECK_FALSE(result.has_value());
    CHECK(info.str().find("http-base-url") != std::string::npos);
}

TEST_CASE("parse_tick_writer_role", "[config]") {
    const ores::testing::scoped_environment_override env_guard(
        {},
        {"ORES_MARKETDATA_SERVICE_WRITER_DB_USER", "ORES_MARKETDATA_SERVICE_WRITER_DB_PASSWORD"});

    std::ostringstream info, err;
    const auto none = parser{}.parse({}, info, err);
    REQUIRE(none.has_value());
    CHECK(none->tick_writer_user.empty());

    const std::vector<std::string> args{
        "--writer-db-user", "ores_x_marketdata_writer", "--writer-db-password", "secret"};
    const auto given = parser{}.parse(args, info, err);
    REQUIRE(given.has_value());
    CHECK(given->tick_writer_user == "ores_x_marketdata_writer");
    CHECK(given->tick_writer_password() == "secret");
}
