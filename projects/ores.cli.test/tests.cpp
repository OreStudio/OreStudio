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
#include <boost/test/unit_test.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include "ores.cli/config/parser.hpp"
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/format.hpp"

using namespace ores::cli::config;

BOOST_AUTO_TEST_SUITE(parser_tests)

BOOST_AUTO_TEST_CASE(test_help_option) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {"--help"};
    auto result = p.parse(args, info, error);
    
    BOOST_CHECK(!result.has_value());
    BOOST_CHECK(!info.str().empty());
    BOOST_CHECK(info.str().find("ORE Studio") != std::string::npos);
    BOOST_CHECK(info.str().find("Commands:") != std::string::npos);
    BOOST_CHECK(info.str().find("import") != std::string::npos);
    BOOST_CHECK(info.str().find("export") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(test_version_option) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {"--version"};
    auto result = p.parse(args, info, error);
    
    BOOST_CHECK(!result.has_value());
    BOOST_CHECK(!info.str().empty());
    BOOST_CHECK(info.str().find("OreStudio") != std::string::npos);
    BOOST_CHECK(info.str().find("Copyright") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(test_import_help) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {"import", "--help"};
    auto result = p.parse(args, info, error);
    
    BOOST_CHECK(!result.has_value());
    BOOST_CHECK(!info.str().empty());
    BOOST_CHECK(info.str().find("import") != std::string::npos);
    BOOST_CHECK(info.str().find("--entity") != std::string::npos);
    BOOST_CHECK(info.str().find("--target") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(test_export_help) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {"export", "--help"};
    auto result = p.parse(args, info, error);
    
    BOOST_CHECK(!result.has_value());
    BOOST_CHECK(!info.str().empty());
    BOOST_CHECK(info.str().find("export") != std::string::npos);
    BOOST_CHECK(info.str().find("--entity") != std::string::npos);
    BOOST_CHECK(info.str().find("--as-of") != std::string::npos);
    BOOST_CHECK(info.str().find("--key") != std::string::npos);
    BOOST_CHECK(info.str().find("--all-versions") != std::string::npos);
    BOOST_CHECK(info.str().find("--format") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(test_logging_options) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {
        "import",
        "--log-enabled",
        "--log-level", "debug",
        "--log-directory", "test_logs",
        "--log-to-console",
        "--entity", "currency_config",
        "--target", "test.xml"
    };
    auto result = p.parse(args, info, error);
    
    BOOST_REQUIRE(result.has_value());
    BOOST_REQUIRE(result->logging.has_value());
    BOOST_CHECK_EQUAL(result->logging->severity, "debug");
    BOOST_CHECK_EQUAL(result->logging->output_directory, "test_logs");
    BOOST_CHECK(result->logging->output_to_console);
}

BOOST_AUTO_TEST_CASE(test_import_basic) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {
        "import",
        "--entity", "currency_config",
        "--target", "test_file.xml"
    };
    auto result = p.parse(args, info, error);
    
    BOOST_REQUIRE(result.has_value());
    BOOST_REQUIRE(result->importing.has_value());
    BOOST_CHECK(result->importing->target_entity == entity::currency_config);
    BOOST_REQUIRE(result->importing->targets.size() == 1);
    BOOST_CHECK_EQUAL(result->importing->targets[0].filename().string(), "test_file.xml");
}

BOOST_AUTO_TEST_CASE(test_import_multiple_targets) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {
        "import",
        "--entity", "currency_config",
        "--target", "file1.xml",
        "--target", "file2.xml",
        "--target", "file3.xml"
    };
    auto result = p.parse(args, info, error);
    
    BOOST_REQUIRE(result.has_value());
    BOOST_REQUIRE(result->importing.has_value());
    BOOST_CHECK(result->importing->target_entity == entity::currency_config);
    BOOST_REQUIRE(result->importing->targets.size() == 3);
    BOOST_CHECK_EQUAL(result->importing->targets[0].filename().string(), "file1.xml");
    BOOST_CHECK_EQUAL(result->importing->targets[1].filename().string(), "file2.xml");
    BOOST_CHECK_EQUAL(result->importing->targets[2].filename().string(), "file3.xml");
}

BOOST_AUTO_TEST_CASE(test_export_basic) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {
        "export",
        "--entity", "currency_config"
    };
    auto result = p.parse(args, info, error);
    
    BOOST_REQUIRE(result.has_value());
    BOOST_REQUIRE(result->exporting.has_value());
    BOOST_CHECK(result->exporting->target_entity == entity::currency_config);
    BOOST_CHECK(result->exporting->as_of.empty());
    BOOST_CHECK(result->exporting->key.empty());
    BOOST_CHECK(!result->exporting->all_versions);
    BOOST_CHECK(result->exporting->target_format == format::json);
}

BOOST_AUTO_TEST_CASE(test_export_full_options) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {
        "export",
        "--entity", "currency_config",
        "--as-of", "2025-01-01",
        "--key", "USD",
        "--all-versions",
        "--format", "xml"
    };
    auto result = p.parse(args, info, error);
    
    BOOST_REQUIRE(result.has_value());
    BOOST_REQUIRE(result->exporting.has_value());
    BOOST_CHECK(result->exporting->target_entity == entity::currency_config);
    BOOST_CHECK_EQUAL(result->exporting->as_of, "2025-01-01");
    BOOST_CHECK_EQUAL(result->exporting->key, "USD");
    BOOST_CHECK(result->exporting->all_versions);
    BOOST_CHECK(result->exporting->target_format == format::xml);
}

BOOST_AUTO_TEST_CASE(test_invalid_command) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {"invalid_command"};
    auto result = p.parse(args, info, error);
    
    BOOST_CHECK(!result.has_value());
    BOOST_CHECK(!error.str().empty());
    BOOST_CHECK(error.str().find("Invalid or unsupported command") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(test_missing_required_import_args) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {"import"};
    auto result = p.parse(args, info, error);
    
    BOOST_CHECK(!result.has_value());
    BOOST_CHECK(!error.str().empty());
    BOOST_CHECK(error.str().find("Must supply entity") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(test_missing_required_export_args) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {"export"};
    auto result = p.parse(args, info, error);
    
    BOOST_CHECK(!result.has_value());
    BOOST_CHECK(!error.str().empty());
    BOOST_CHECK(error.str().find("Must supply entity") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(test_import_with_logging) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {
        "import",
        "--log-enabled",
        "--log-level", "trace",
        "--entity", "currency_config",
        "--target", "test.xml"
    };
    auto result = p.parse(args, info, error);
    
    BOOST_REQUIRE(result.has_value());
    BOOST_REQUIRE(result->logging.has_value());
    BOOST_REQUIRE(result->importing.has_value());
    BOOST_CHECK_EQUAL(result->logging->severity, "trace");
    BOOST_CHECK(result->importing->target_entity == entity::currency_config);
}

BOOST_AUTO_TEST_CASE(test_export_with_logging) {
    parser p;
    std::ostringstream info, error;
    
    std::vector<std::string> args = {
        "export",
        "--log-enabled",
        "--log-level", "warn",
        "--entity", "currency_config",
        "--format", "json"
    };
    auto result = p.parse(args, info, error);
    
    BOOST_REQUIRE(result.has_value());
    BOOST_REQUIRE(result->logging.has_value());
    BOOST_REQUIRE(result->exporting.has_value());
    BOOST_CHECK_EQUAL(result->logging->severity, "warn");
    BOOST_CHECK(result->exporting->target_entity == entity::currency_config);
    BOOST_CHECK(result->exporting->target_format == format::json);
}

BOOST_AUTO_TEST_SUITE_END()
