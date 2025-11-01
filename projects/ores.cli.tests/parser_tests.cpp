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
#include <vector>
#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/streaming/std_optional.hpp" // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.cli/config/parser.hpp"
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/format.hpp"
#include "ores.cli/config/parser_exception.hpp"

using ores::cli::config::parser;
using ores::cli::config::entity;
using ores::cli::config::format;
using ores::cli::config::parser_exception;

TEST_CASE("test_help_option", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"--help"};

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("ORE Studio") != std::string::npos);
    CHECK(info.str().find("Commands:") != std::string::npos);
    CHECK(info.str().find("import") != std::string::npos);
    CHECK(info.str().find("export") != std::string::npos);
}

TEST_CASE("test_version_option", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"--version"};

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("OreStudio") != std::string::npos);
    CHECK(info.str().find("Copyright") != std::string::npos);
}

TEST_CASE("test_import_help", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"import", "--help"};

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("import") != std::string::npos);
    CHECK(info.str().find("--entity") != std::string::npos);
    CHECK(info.str().find("--target") != std::string::npos);
}

TEST_CASE("test_export_help", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"export", "--help"};

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("export") != std::string::npos);
    CHECK(info.str().find("--entity") != std::string::npos);
    CHECK(info.str().find("--as-of") != std::string::npos);
    CHECK(info.str().find("--key") != std::string::npos);
    CHECK(info.str().find("--all-versions") != std::string::npos);
    CHECK(info.str().find("--format") != std::string::npos);
}

TEST_CASE("test_logging_options", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "import",
        "--log-enabled",
        "--log-level", "debug",
        "--log-directory", "test_logs",
        "--log-to-console",
        "--entity", "currencies",
        "--target", "test.xml"
    };

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->logging.has_value());
    CHECK(result->logging->severity == "debug");
    CHECK(result->logging->output_directory == "test_logs");
    CHECK(result->logging->output_to_console);
}

TEST_CASE("test_import_basic", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "import",
        "--entity", "currencies",
        "--target", "test_file.xml"
    };

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->importing.has_value());
    CHECK(result->importing->target_entity == entity::currencies);
    REQUIRE(result->importing->targets.size() == 1);
    CHECK(result->importing->targets[0].filename().string() == "test_file.xml");
}

TEST_CASE("test_import_multiple_targets", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "import",
        "--entity", "currencies",
        "--target", "file1.xml",
        "--target", "file2.xml",
        "--target", "file3.xml"
    };

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->importing.has_value());
    CHECK(result->importing->target_entity == entity::currencies);
    REQUIRE(result->importing->targets.size() == 3);
    CHECK(result->importing->targets[0].filename().string() == "file1.xml");
    CHECK(result->importing->targets[1].filename().string() == "file2.xml");
    CHECK(result->importing->targets[2].filename().string() == "file3.xml");
}

TEST_CASE("test_export_basic", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "export",
        "--entity", "currencies"
    };

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::currencies);
    CHECK(result->exporting->as_of.empty());
    CHECK(result->exporting->key.empty());
    CHECK(!result->exporting->all_versions);
    CHECK(result->exporting->target_format == format::json);
}

TEST_CASE("test_export_full_options", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "export",
        "--entity", "currencies",
        "--as-of", "2025-01-01",
        "--key", "USD",
        "--all-versions",
        "--format", "xml"
    };

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::currencies);
    CHECK(result->exporting->as_of == "2025-01-01");
    CHECK(result->exporting->key == "USD");
    CHECK(result->exporting->all_versions);
    CHECK(result->exporting->target_format == format::xml);
}

TEST_CASE("test_invalid_command", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"invalid_command"};
    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_missing_required_import_args", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"import"};
    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_missing_required_export_args", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"export"};
    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_import_with_logging", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "import",
        "--log-enabled",
        "--log-level", "trace",
        "--entity", "currencies",
        "--target", "test.xml"
    };

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->logging.has_value());
    REQUIRE(result->importing.has_value());
    CHECK(result->logging->severity == "trace");
    CHECK(result->importing->target_entity == entity::currencies);
}

TEST_CASE("test_export_with_logging", "[parser_tests]") {
    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "export",
        "--log-enabled",
        "--log-level", "warn",
        "--entity", "currencies",
        "--format", "json"
    };

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->logging.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->logging->severity == "warn");
    CHECK(result->exporting->target_entity == entity::currencies);
    CHECK(result->exporting->target_format == format::json);
}
