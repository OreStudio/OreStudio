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
#include "ores.logging/logging_configuration.hpp"
#include "ores.logging/logging_exception.hpp"
#include "ores.logging/logging_options.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/program_options.hpp>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace {

const std::string_view test_suite("ores.logging.tests");
const std::string tags("[logging]");

const std::string default_log_file("app.log");

std::optional<ores::logging::logging_options> read_options(const std::vector<std::string>& args) {
    using namespace boost::program_options;
    using ores::logging::logging_configuration;

    const auto od(logging_configuration::make_options_description(default_log_file));
    variables_map vm;
    store(command_line_parser(args).options(od).run(), vm);
    notify(vm);
    return logging_configuration::read_options(vm);
}

}

using ores::logging::logging_configuration;
using ores::logging::logging_exception;

TEST_CASE("read_options_returns_nullopt_without_log_enabled", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args;
    const auto result(read_options(args));
    BOOST_LOG_SEV(lg, ores::logging::info) << "has value: " << result.has_value();

    CHECK_FALSE(result.has_value());
}

TEST_CASE("make_options_description_lists_every_logging_option", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    std::ostringstream os;
    os << logging_configuration::make_options_description(default_log_file);
    const std::string description(os.str());
    BOOST_LOG_SEV(lg, ores::logging::info) << description;

    CHECK(description.contains("log-enabled"));
    CHECK(description.contains("log-level"));
    CHECK(description.contains("log-to-console"));
    CHECK(description.contains("log-directory"));
    CHECK(description.contains("log-filename"));
    CHECK(description.contains("log-include-pid"));
    CHECK(description.contains("log-replica-index"));
}

TEST_CASE("read_options_returns_defaults_when_logging_enabled", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--log-enabled", "--log-level", "debug"};
    const auto result(read_options(args));
    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, ores::logging::info) << *result;

    CHECK(result->severity == "debug");
    CHECK(result->filename == "app.log");
    CHECK(result->output_directory == std::filesystem::path("log"));
    CHECK_FALSE(result->output_to_console);
    CHECK_FALSE(result->include_pid);
    CHECK_FALSE(result->replica_index.has_value());
}

TEST_CASE("read_options_reads_console_pid_and_replica_index", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{
        "--log-enabled", "--log-to-console", "--log-include-pid", "--log-replica-index", "3"};
    const auto result(read_options(args));
    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, ores::logging::info) << *result;

    CHECK(result->output_to_console);
    CHECK(result->include_pid);
    CHECK(result->replica_index == 3);
}

TEST_CASE("read_options_rejects_invalid_log_level", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::vector<std::string> args{"--log-enabled", "--log-level", "fatal"};
    BOOST_LOG_SEV(lg, ores::logging::info) << "log level: fatal";

    CHECK_THROWS_AS(read_options(args), logging_exception);
}
