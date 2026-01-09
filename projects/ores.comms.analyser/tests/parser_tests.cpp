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
#include "ores.comms.analyser/config/parser.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.comms.analyser.tests");
const std::string tags("[parser]");

}

using namespace ores::comms::analyser::config;
using namespace ores::logging;

TEST_CASE("parser_help_returns_nullopt", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing --help returns nullopt";

    const char* argv[] = {"ores.comms.analyser", "--help"};
    auto result = parser::parse(2, argv);

    REQUIRE_FALSE(result.has_value());
}

TEST_CASE("parser_short_help_returns_nullopt", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing -h returns nullopt";

    const char* argv[] = {"ores.comms.analyser", "-h"};
    auto result = parser::parse(2, argv);

    REQUIRE_FALSE(result.has_value());
}

TEST_CASE("parser_version_returns_nullopt", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing --version returns nullopt";

    const char* argv[] = {"ores.comms.analyser", "--version"};
    auto result = parser::parse(2, argv);

    REQUIRE_FALSE(result.has_value());
}

TEST_CASE("parser_short_version_returns_nullopt", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing -v returns nullopt";

    const char* argv[] = {"ores.comms.analyser", "-v"};
    auto result = parser::parse(2, argv);

    REQUIRE_FALSE(result.has_value());
}

TEST_CASE("parser_read_command_with_file", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing read command with file";

    const char* argv[] = {"ores.comms.analyser", "read", "session.ores"};
    auto result = parser::parse(3, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->cmd == command::read);
    REQUIRE(result->input_file == "session.ores");
    REQUIRE_FALSE(result->verbose);
}

TEST_CASE("parser_info_command_with_file", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing info command with file";

    const char* argv[] = {"ores.comms.analyser", "info", "session.ores"};
    auto result = parser::parse(3, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->cmd == command::info);
    REQUIRE(result->input_file == "session.ores");
}

TEST_CASE("parser_file_only_defaults_to_read", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing file-only argument defaults to read command";

    const char* argv[] = {"ores.comms.analyser", "session.ores"};
    auto result = parser::parse(2, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->cmd == command::read);
    REQUIRE(result->input_file == "session.ores");
}

TEST_CASE("parser_verbose_flag", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing --verbose flag";

    const char* argv[] = {"ores.comms.analyser", "read", "session.ores", "--verbose"};
    auto result = parser::parse(4, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->verbose);
}

TEST_CASE("parser_verbose_before_file", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing --verbose flag before file";

    const char* argv[] = {"ores.comms.analyser", "--verbose", "session.ores"};
    auto result = parser::parse(3, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->verbose);
    REQUIRE(result->input_file == "session.ores");
}

TEST_CASE("parser_no_file_throws", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing missing file throws exception";

    const char* argv[] = {"ores.comms.analyser", "read"};

    REQUIRE_THROWS_AS(parser::parse(2, argv), std::runtime_error);
}

TEST_CASE("parser_no_args_throws", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing no arguments throws exception";

    const char* argv[] = {"ores.comms.analyser"};

    REQUIRE_THROWS_AS(parser::parse(1, argv), std::runtime_error);
}

TEST_CASE("parser_path_with_directory", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing path with directory";

    const char* argv[] = {"ores.comms.analyser", "read", "/path/to/session.ores"};
    auto result = parser::parse(3, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->input_file == "/path/to/session.ores");
}

TEST_CASE("parser_relative_path", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing relative path";

    const char* argv[] = {"ores.comms.analyser", "./recordings/session.ores"};
    auto result = parser::parse(2, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->input_file == "./recordings/session.ores");
}

TEST_CASE("parser_file_with_spaces_in_path", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing file with spaces in path";

    const char* argv[] = {"ores.comms.analyser", "/path/with spaces/session file.ores"};
    auto result = parser::parse(2, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->input_file == "/path/with spaces/session file.ores");
}

TEST_CASE("parser_unrecognized_command_treated_as_filename", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing unrecognized command treated as filename";

    const char* argv[] = {"ores.comms.analyser", "my-session-file.ores"};
    auto result = parser::parse(2, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->cmd == command::read);
    REQUIRE(result->input_file == "my-session-file.ores");
}

TEST_CASE("parser_info_with_verbose", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing info command with verbose";

    const char* argv[] = {"ores.comms.analyser", "info", "session.ores", "--verbose"};
    auto result = parser::parse(4, argv);

    REQUIRE(result.has_value());
    REQUIRE(result->cmd == command::info);
    REQUIRE(result->verbose);
}

TEST_CASE("parser_default_verbose_is_false", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing default verbose is false";

    const char* argv[] = {"ores.comms.analyser", "session.ores"};
    auto result = parser::parse(2, argv);

    REQUIRE(result.has_value());
    REQUIRE_FALSE(result->verbose);
}
