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
#include "ores.telemetry/log/logging_configuration.hpp"

#include <vector>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[logging_configuration]");

namespace po = boost::program_options;
using namespace ores::telemetry::log;

po::variables_map parse_args(const po::options_description& desc,
                             const std::vector<const char*>& args) {
    po::variables_map vm;
    po::store(po::parse_command_line(
        static_cast<int>(args.size()), args.data(), desc), vm);
    po::notify(vm);
    return vm;
}

}

TEST_CASE("logging_make_options_description_has_expected_options", tags) {
    auto desc = logging_configuration::make_options_description("test.log");

    REQUIRE(desc.find_nothrow("log-enabled", false) != nullptr);
    REQUIRE(desc.find_nothrow("log-level", false) != nullptr);
    REQUIRE(desc.find_nothrow("log-to-console", false) != nullptr);
    REQUIRE(desc.find_nothrow("log-directory", false) != nullptr);
    REQUIRE(desc.find_nothrow("log-filename", false) != nullptr);
    REQUIRE(desc.find_nothrow("log-include-pid", false) != nullptr);
}

TEST_CASE("logging_read_options_returns_nullopt_when_disabled", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE_FALSE(opts.has_value());
}

TEST_CASE("logging_read_options_returns_value_when_enabled", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
}

TEST_CASE("logging_read_options_uses_default_filename", tags) {
    auto desc = logging_configuration::make_options_description("my-app.log");
    std::vector<const char*> args = {"test", "--log-enabled"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->filename == "my-app.log");
}

TEST_CASE("logging_read_options_parses_custom_filename", tags) {
    auto desc = logging_configuration::make_options_description("default.log");
    std::vector<const char*> args = {"test", "--log-enabled", "--log-filename", "custom.log"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->filename == "custom.log");
}

TEST_CASE("logging_read_options_uses_default_directory", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->output_directory == "log");
}

TEST_CASE("logging_read_options_parses_custom_directory", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled", "--log-directory", "/var/log/myapp"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->output_directory == "/var/log/myapp");
}

TEST_CASE("logging_read_options_uses_default_level", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->severity == "info");
}

TEST_CASE("logging_read_options_parses_log_level", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled", "--log-level", "debug"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->severity == "debug");
}

TEST_CASE("logging_read_options_parses_short_log_level", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "-e", "-l", "warn"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->severity == "warn");
}

TEST_CASE("logging_read_options_console_disabled_by_default", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE_FALSE(opts->output_to_console);
}

TEST_CASE("logging_read_options_enables_console_output", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled", "--log-to-console"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->output_to_console);
}

TEST_CASE("logging_read_options_include_pid_disabled_by_default", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE_FALSE(opts->include_pid);
}

TEST_CASE("logging_read_options_enables_include_pid", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled", "--log-include-pid"};
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->include_pid);
}

TEST_CASE("logging_read_options_throws_on_invalid_level", tags) {
    auto desc = logging_configuration::make_options_description("test.log");
    std::vector<const char*> args = {"test", "--log-enabled", "--log-level", "invalid"};
    auto vm = parse_args(desc, args);

    REQUIRE_THROWS(logging_configuration::read_options(vm));
}

TEST_CASE("logging_read_options_parses_all_options", tags) {
    auto desc = logging_configuration::make_options_description("default.log");
    std::vector<const char*> args = {
        "test",
        "--log-enabled",
        "--log-level", "trace",
        "--log-to-console",
        "--log-directory", "/tmp/logs",
        "--log-filename", "myapp.log",
        "--log-include-pid"
    };
    auto vm = parse_args(desc, args);

    auto opts = logging_configuration::read_options(vm);

    REQUIRE(opts.has_value());
    REQUIRE(opts->severity == "trace");
    REQUIRE(opts->output_to_console);
    REQUIRE(opts->output_directory == "/tmp/logs");
    REQUIRE(opts->filename == "myapp.log");
    REQUIRE(opts->include_pid);
}
