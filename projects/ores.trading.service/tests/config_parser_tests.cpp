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
#include "ores.trading.service/config/parser.hpp"

#include <sstream>
#include <vector>
#include <string>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.trading.service/config/parser_exception.hpp"

namespace {

const std::string_view test_suite("ores.trading.service.tests");
const std::string tags("[config]");

}

using namespace ores::trading::service::config;

TEST_CASE("parse_defaults_returns_expected_values", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args;
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    REQUIRE(result.has_value());
    CHECK(result->nats.url == "nats://localhost:4222");
    CHECK(result->nats.subject_prefix.empty());
    CHECK(result->database.host == "localhost");
    CHECK(result->database.port == 5432);
    REQUIRE(result->logging.has_value());
    CHECK(result->logging->severity == "info");
    CHECK(result->logging->filename == "ores.trading.service.log");
}

TEST_CASE("parse_custom_nats_url", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--nats-url", "nats://myserver:5555"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    REQUIRE(result.has_value());
    CHECK(result->nats.url == "nats://myserver:5555");
}

TEST_CASE("parse_nats_subject_prefix", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{
        "--nats-subject-prefix", "ores.prod.main1"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    REQUIRE(result.has_value());
    CHECK(result->nats.subject_prefix == "ores.prod.main1");
}

TEST_CASE("parse_custom_database_host_and_port", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{
        "--db-host", "dbserver.internal",
        "--db-port", "5433"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    REQUIRE(result.has_value());
    CHECK(result->database.host == "dbserver.internal");
    CHECK(result->database.port == 5433);
}

TEST_CASE("parse_custom_database_name", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--db-database", "ores_prod"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    REQUIRE(result.has_value());
    CHECK(result->database.database == "ores_prod");
}

TEST_CASE("parse_custom_log_level", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--log-level", "debug"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    REQUIRE(result.has_value());
    REQUIRE(result->logging.has_value());
    CHECK(result->logging->severity == "debug");
}

TEST_CASE("parse_custom_log_filename", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--log-filename", "custom.log"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    REQUIRE(result.has_value());
    REQUIRE(result->logging.has_value());
    CHECK(result->logging->filename == "custom.log");
}

TEST_CASE("parse_help_returns_empty", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--help"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    CHECK_FALSE(result.has_value());
    CHECK_FALSE(info.str().empty());
}

TEST_CASE("parse_version_returns_empty", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--version"};
    std::ostringstream info, err;
    const auto result = parser{}.parse(args, info, err);

    CHECK_FALSE(result.has_value());
    CHECK_FALSE(info.str().empty());
}

TEST_CASE("parse_unknown_option_throws", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--unknown-option", "value"};
    std::ostringstream info, err;
    CHECK_THROWS_AS(parser{}.parse(args, info, err), parser_exception);
}
