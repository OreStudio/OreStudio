/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 3 of the License, or (at your option)
 * any later version.
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
#include "ores.assets.service/config/parser.hpp"
#include "ores.assets.service/config/parser_exception.hpp"
#include "ores.logging/logger.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <sstream>
#include <string>
#include <vector>

using namespace ores::assets::service::config;

namespace {

/// The least a parser needs before it will run: the database user and name
/// carry no compiled default because a service cannot work without them. A
/// test that asserts a compiled default states only these, so no value a
/// developer exported can reach the assertion.
std::map<std::string, std::string> least_environment() {
    return {{"ORES_ASSETS_SERVICE_DB_USER", "ores_test_user"},
            {"ORES_ASSETS_SERVICE_DB_DATABASE", "ores_test_db"}};
}

}

TEST_CASE("parse_defaults_returns_expected_values", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args;
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err, least_environment());

    REQUIRE(result.has_value());
    CHECK(result->nats.url == "nats://localhost:4222");
    CHECK(result->nats.subject_prefix.empty());
    CHECK(result->database.host == "localhost");
    CHECK(result->database.port == 5432);
    CHECK_FALSE(result->logging.has_value());
}

TEST_CASE("an_environment_value_wins_over_the_compiled_default", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args;
    std::ostringstream info, err;
    auto environment = least_environment();
    environment["ORES_ASSETS_SERVICE_DB_PORT"] = "20007";
    const auto result = parser{}.parse(args, info, err, environment);

    REQUIRE(result.has_value());
    CHECK(result->database.port == 20007);
}

TEST_CASE("parse_custom_nats_url", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--nats-url", "nats://myserver:5555"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err, least_environment());

    REQUIRE(result.has_value());
    CHECK(result->nats.url == "nats://myserver:5555");
}

TEST_CASE("parse_nats_subject_prefix", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--nats-subject-prefix", "ores.prod.main1"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err, least_environment());

    REQUIRE(result.has_value());
    CHECK(result->nats.subject_prefix == "ores.prod.main1");
}

TEST_CASE("parse_custom_database_host_and_port", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--db-host", "dbserver.internal", "--db-port", "5433"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err, least_environment());

    REQUIRE(result.has_value());
    CHECK(result->database.host == "dbserver.internal");
    CHECK(result->database.port == 5433);
}

TEST_CASE("parse_custom_database_name", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--db-database", "ores_prod"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err, least_environment());

    REQUIRE(result.has_value());
    CHECK(result->database.database == "ores_prod");
}

TEST_CASE("parse_custom_log_level", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--log-enabled", "--log-level", "debug"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err, least_environment());

    REQUIRE(result.has_value());
    REQUIRE(result->logging.has_value());
    CHECK(result->logging->severity == "debug");
}

TEST_CASE("parse_custom_log_filename", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--log-enabled", "--log-filename", "custom.log"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err, least_environment());

    REQUIRE(result.has_value());
    REQUIRE(result->logging.has_value());
    CHECK(result->logging->filename == "custom.log");
}

TEST_CASE("parse_help_returns_empty", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--help"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err, least_environment());

    CHECK_FALSE(result.has_value());
    CHECK_FALSE(info.str().empty());
}

TEST_CASE("parse_version_returns_empty", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--version"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err, least_environment());

    CHECK_FALSE(result.has_value());
    CHECK_FALSE(info.str().empty());
}

TEST_CASE("parse_unknown_option_throws", "[config]") {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--unknown-option", "value"};
    std::ostringstream info, err;
    CHECK_THROWS_AS(parser{}.parse(args, info, err, least_environment()), parser_exception);
}
