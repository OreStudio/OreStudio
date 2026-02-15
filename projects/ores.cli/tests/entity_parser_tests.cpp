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
#include "ores.cli/config/parser.hpp"

#include <vector>
#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_optional.hpp" // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/format.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.cli/config/add_account_options.hpp"
#include "ores.cli/config/add_role_options.hpp"
#include "ores.cli/config/add_permission_options.hpp"
#include "ores.cli/config/add_country_options.hpp"
#include "ores.cli/config/add_change_reason_options.hpp"
#include "ores.cli/config/add_change_reason_category_options.hpp"
#include "ores.cli/config/add_feature_flag_options.hpp"
#include "ores.cli/config/add_login_info_options.hpp"

namespace {

const std::string_view test_suite("ores.cli.tests");

}

using namespace ores::logging;
using ores::cli::config::parser;
using ores::cli::config::entity;
using ores::cli::config::format;
using ores::cli::config::parser_exception;

// ==========================================================================
// Accounts parser tests
// ==========================================================================
const std::string accounts_tags("[accounts_parser]");

TEST_CASE("test_accounts_help_with_no_operation", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"accounts", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);
    BOOST_LOG_SEV(lg, debug) << "Result: " << result;
    BOOST_LOG_SEV(lg, debug) << "Info: " << info.str();

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("accounts") != std::string::npos);
}

TEST_CASE("test_accounts_no_operation", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"accounts"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_accounts_invalid_operation", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"accounts", "invalid_op"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_accounts_list_operation", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"accounts", "list"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);
    BOOST_LOG_SEV(lg, debug) << "Result: " << result;

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::accounts);
    CHECK(result->exporting->target_format == format::json);
}

TEST_CASE("test_accounts_delete_operation", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "accounts", "delete", "--key", "test_user"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);
    BOOST_LOG_SEV(lg, debug) << "Result: " << result;

    REQUIRE(result.has_value());
    REQUIRE(result->deleting.has_value());
    CHECK(result->deleting->target_entity == entity::accounts);
    CHECK(result->deleting->key == "test_user");
}

TEST_CASE("test_accounts_add_missing_required_fields", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"accounts", "add"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_accounts_add_with_all_fields", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "accounts", "add",
        "--username", "john_doe",
        "--email", "john@example.com",
        "--password", "secret123",
        "--admin",
        "--modified-by", "admin"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);
    BOOST_LOG_SEV(lg, debug) << "Result: " << result;

    REQUIRE(result.has_value());
    REQUIRE(result->adding.has_value());
    const auto& opts = std::get<ores::cli::config::add_account_options>(
        *result->adding);
    CHECK(opts.username == "john_doe");
    CHECK(opts.email == "john@example.com");
    CHECK(opts.password == "secret123");
    CHECK(opts.modified_by == "admin");
    REQUIRE(opts.admin.has_value());
    CHECK(*opts.admin == true);
}

TEST_CASE("test_accounts_list_help", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"accounts", "list", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);
    BOOST_LOG_SEV(lg, debug) << "Info: " << info.str();

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}

TEST_CASE("test_accounts_delete_help", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"accounts", "delete", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}

TEST_CASE("test_accounts_add_help", accounts_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"accounts", "add", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}

// ==========================================================================
// Roles parser tests
// ==========================================================================
const std::string roles_tags("[roles_parser]");

TEST_CASE("test_roles_help_with_no_operation", roles_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"roles", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("roles") != std::string::npos);
}

TEST_CASE("test_roles_no_operation", roles_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"roles"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_roles_invalid_operation", roles_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"roles", "invalid_op"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_roles_list_operation", roles_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"roles", "list"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::roles);
    CHECK(result->exporting->target_format == format::json);
}

TEST_CASE("test_roles_delete_operation", roles_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "roles", "delete", "--key", "test_role_id"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->deleting.has_value());
    CHECK(result->deleting->target_entity == entity::roles);
    CHECK(result->deleting->key == "test_role_id");
}

TEST_CASE("test_roles_add_missing_required_fields", roles_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"roles", "add"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_roles_add_with_all_fields", roles_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "roles", "add",
        "--name", "editor",
        "--description", "Can edit content",
        "--modified-by", "admin",
        "--change-reason-code", "INIT",
        "--change-commentary", "Initial setup",
        "--permission-code", "content:read", "content:write"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->adding.has_value());
    const auto& opts = std::get<ores::cli::config::add_role_options>(
        *result->adding);
    CHECK(opts.name == "editor");
    CHECK(opts.description == "Can edit content");
    CHECK(opts.modified_by == "admin");
    REQUIRE(opts.change_reason_code.has_value());
    CHECK(*opts.change_reason_code == "INIT");
    REQUIRE(opts.change_commentary.has_value());
    CHECK(*opts.change_commentary == "Initial setup");
    REQUIRE(opts.permission_codes.size() == 2);
    CHECK(opts.permission_codes[0] == "content:read");
    CHECK(opts.permission_codes[1] == "content:write");
}

TEST_CASE("test_roles_list_help", roles_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"roles", "list", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}

// ==========================================================================
// Permissions parser tests
// ==========================================================================
const std::string permissions_tags("[permissions_parser]");

TEST_CASE("test_permissions_help_with_no_operation", permissions_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"permissions", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("permissions") != std::string::npos);
}

TEST_CASE("test_permissions_no_operation", permissions_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"permissions"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_permissions_invalid_operation", permissions_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"permissions", "invalid_op"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_permissions_list_operation", permissions_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"permissions", "list"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::permissions);
    CHECK(result->exporting->target_format == format::json);
}

TEST_CASE("test_permissions_delete_operation", permissions_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "permissions", "delete", "--key", "content:read"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->deleting.has_value());
    CHECK(result->deleting->target_entity == entity::permissions);
    CHECK(result->deleting->key == "content:read");
}

TEST_CASE("test_permissions_add_missing_required_fields", permissions_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"permissions", "add"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_permissions_add_with_all_fields", permissions_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "permissions", "add",
        "--code", "reports:generate",
        "--description", "Generate reports"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->adding.has_value());
    const auto& opts = std::get<ores::cli::config::add_permission_options>(
        *result->adding);
    CHECK(opts.code == "reports:generate");
    REQUIRE(opts.description.has_value());
    CHECK(*opts.description == "Generate reports");
}

TEST_CASE("test_permissions_list_help", permissions_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"permissions", "list", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}

// ==========================================================================
// Countries parser tests
// ==========================================================================
const std::string countries_tags("[countries_parser]");

TEST_CASE("test_countries_help_with_no_operation", countries_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"countries", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("countries") != std::string::npos);
}

TEST_CASE("test_countries_no_operation", countries_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"countries"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_countries_invalid_operation", countries_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"countries", "invalid_op"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_countries_list_operation", countries_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"countries", "list"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::countries);
    CHECK(result->exporting->target_format == format::json);
}

TEST_CASE("test_countries_delete_operation", countries_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "countries", "delete", "--key", "US"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->deleting.has_value());
    CHECK(result->deleting->target_entity == entity::countries);
    CHECK(result->deleting->key == "US");
}

TEST_CASE("test_countries_add_missing_required_fields", countries_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"countries", "add"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_countries_add_with_all_fields", countries_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "countries", "add",
        "--alpha2-code", "GB",
        "--alpha3-code", "GBR",
        "--name", "United Kingdom",
        "--numeric-code", "826",
        "--official-name", "United Kingdom of Great Britain and Northern Ireland",
        "--modified-by", "admin",
        "--change-reason-code", "INIT",
        "--change-commentary", "Initial setup"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->adding.has_value());
    const auto& opts = std::get<ores::cli::config::add_country_options>(
        *result->adding);
    CHECK(opts.alpha2_code == "GB");
    CHECK(opts.alpha3_code == "GBR");
    CHECK(opts.name == "United Kingdom");
    REQUIRE(opts.numeric_code.has_value());
    CHECK(*opts.numeric_code == "826");
    REQUIRE(opts.official_name.has_value());
    CHECK(*opts.official_name == "United Kingdom of Great Britain and Northern Ireland");
    CHECK(opts.modified_by == "admin");
    REQUIRE(opts.change_reason_code.has_value());
    CHECK(*opts.change_reason_code == "INIT");
    REQUIRE(opts.change_commentary.has_value());
    CHECK(*opts.change_commentary == "Initial setup");
}

TEST_CASE("test_countries_list_help", countries_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"countries", "list", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}

// ==========================================================================
// Change reasons parser tests
// ==========================================================================
const std::string change_reasons_tags("[change_reasons_parser]");

TEST_CASE("test_change_reasons_help_with_no_operation", change_reasons_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reasons", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("change-reasons") != std::string::npos);
}

TEST_CASE("test_change_reasons_no_operation", change_reasons_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reasons"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_change_reasons_invalid_operation", change_reasons_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reasons", "invalid_op"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_change_reasons_list_operation", change_reasons_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reasons", "list"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::change_reasons);
    CHECK(result->exporting->target_format == format::json);
}

TEST_CASE("test_change_reasons_delete_operation", change_reasons_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "change-reasons", "delete", "--key", "INIT.001"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->deleting.has_value());
    CHECK(result->deleting->target_entity == entity::change_reasons);
    CHECK(result->deleting->key == "INIT.001");
}

TEST_CASE("test_change_reasons_add_missing_required_fields", change_reasons_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reasons", "add"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_change_reasons_add_with_all_fields", change_reasons_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "change-reasons", "add",
        "--code", "INIT.001",
        "--description", "Initial creation",
        "--category-code", "INIT",
        "--applies-to-amend",
        "--applies-to-delete",
        "--requires-commentary",
        "--display-order", "5",
        "--modified-by", "admin",
        "--change-commentary", "Adding new reason"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->adding.has_value());
    const auto& opts = std::get<ores::cli::config::add_change_reason_options>(
        *result->adding);
    CHECK(opts.code == "INIT.001");
    CHECK(opts.description == "Initial creation");
    CHECK(opts.category_code == "INIT");
    CHECK(opts.modified_by == "admin");
    REQUIRE(opts.display_order.has_value());
    CHECK(*opts.display_order == 5);
    REQUIRE(opts.change_commentary.has_value());
    CHECK(*opts.change_commentary == "Adding new reason");
}

TEST_CASE("test_change_reasons_list_help", change_reasons_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reasons", "list", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}

// ==========================================================================
// Change reason categories parser tests
// ==========================================================================
const std::string change_reason_categories_tags("[change_reason_categories_parser]");

TEST_CASE("test_change_reason_categories_help_with_no_operation",
    change_reason_categories_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reason-categories", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("change-reason-categories") != std::string::npos);
}

TEST_CASE("test_change_reason_categories_no_operation",
    change_reason_categories_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reason-categories"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_change_reason_categories_invalid_operation",
    change_reason_categories_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reason-categories", "invalid_op"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_change_reason_categories_list_operation",
    change_reason_categories_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reason-categories", "list"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::change_reason_categories);
    CHECK(result->exporting->target_format == format::json);
}

TEST_CASE("test_change_reason_categories_delete_operation",
    change_reason_categories_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "change-reason-categories", "delete", "--key", "INIT"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->deleting.has_value());
    CHECK(result->deleting->target_entity == entity::change_reason_categories);
    CHECK(result->deleting->key == "INIT");
}

TEST_CASE("test_change_reason_categories_add_missing_required_fields",
    change_reason_categories_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"change-reason-categories", "add"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_change_reason_categories_add_with_all_fields",
    change_reason_categories_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "change-reason-categories", "add",
        "--code", "MAINT",
        "--description", "Maintenance changes",
        "--modified-by", "admin",
        "--change-commentary", "Adding maintenance category"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->adding.has_value());
    const auto& opts =
        std::get<ores::cli::config::add_change_reason_category_options>(
            *result->adding);
    CHECK(opts.code == "MAINT");
    CHECK(opts.description == "Maintenance changes");
    CHECK(opts.modified_by == "admin");
    REQUIRE(opts.change_commentary.has_value());
    CHECK(*opts.change_commentary == "Adding maintenance category");
}

TEST_CASE("test_change_reason_categories_list_help",
    change_reason_categories_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "change-reason-categories", "list", "--help"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}

// ==========================================================================
// Feature flags parser tests
// ==========================================================================
const std::string feature_flags_tags("[feature_flags_parser]");

TEST_CASE("test_feature_flags_help_with_no_operation", feature_flags_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"feature-flags", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("feature-flags") != std::string::npos);
}

TEST_CASE("test_feature_flags_no_operation", feature_flags_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"feature-flags"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_feature_flags_invalid_operation", feature_flags_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"feature-flags", "invalid_op"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_feature_flags_list_operation", feature_flags_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"feature-flags", "list"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::feature_flags);
    CHECK(result->exporting->target_format == format::json);
}

TEST_CASE("test_feature_flags_delete_operation", feature_flags_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "feature-flags", "delete", "--key", "dark_mode"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->deleting.has_value());
    CHECK(result->deleting->target_entity == entity::feature_flags);
    CHECK(result->deleting->key == "dark_mode");
}

TEST_CASE("test_feature_flags_add_missing_required_fields", feature_flags_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"feature-flags", "add"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_feature_flags_add_with_all_fields", feature_flags_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "feature-flags", "add",
        "--name", "dark_mode",
        "--description", "Enable dark mode UI",
        "--enabled", "true",
        "--modified-by", "admin"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->adding.has_value());
    const auto& opts = std::get<ores::cli::config::add_feature_flag_options>(
        *result->adding);
    CHECK(opts.flag_name == "dark_mode");
    CHECK(opts.modified_by == "admin");
    REQUIRE(opts.description.has_value());
    CHECK(*opts.description == "Enable dark mode UI");
    REQUIRE(opts.enabled.has_value());
    CHECK(*opts.enabled == true);
}

TEST_CASE("test_feature_flags_list_help", feature_flags_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"feature-flags", "list", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}

// ==========================================================================
// Login info parser tests
// ==========================================================================
const std::string login_info_tags("[login_info_parser]");

TEST_CASE("test_login_info_help_with_no_operation", login_info_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"login-info", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
    CHECK(info.str().find("login-info") != std::string::npos);
}

TEST_CASE("test_login_info_no_operation", login_info_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"login-info"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_login_info_invalid_operation", login_info_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"login-info", "invalid_op"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_login_info_list_operation", login_info_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"login-info", "list"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->exporting.has_value());
    CHECK(result->exporting->target_entity == entity::login_info);
    CHECK(result->exporting->target_format == format::json);
}

TEST_CASE("test_login_info_delete_operation", login_info_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "login-info", "delete",
        "--key", "550e8400-e29b-41d4-a716-446655440000"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->deleting.has_value());
    CHECK(result->deleting->target_entity == entity::login_info);
    CHECK(result->deleting->key == "550e8400-e29b-41d4-a716-446655440000");
}

TEST_CASE("test_login_info_add_missing_required_fields", login_info_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"login-info", "add"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    CHECK_THROWS_AS(p.parse(args, info, error), parser_exception);
}

TEST_CASE("test_login_info_add_with_all_fields", login_info_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {
        "login-info", "add",
        "--account-id", "550e8400-e29b-41d4-a716-446655440000",
        "--locked", "true",
        "--failed-logins", "3"
    };
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    REQUIRE(result.has_value());
    REQUIRE(result->adding.has_value());
    const auto& opts = std::get<ores::cli::config::add_login_info_options>(
        *result->adding);
    CHECK(opts.account_id == "550e8400-e29b-41d4-a716-446655440000");
    REQUIRE(opts.locked.has_value());
    CHECK(*opts.locked == true);
    REQUIRE(opts.failed_logins.has_value());
    CHECK(*opts.failed_logins == 3);
}

TEST_CASE("test_login_info_list_help", login_info_tags) {
    auto lg = make_logger(test_suite);

    parser p;
    std::ostringstream info, error;

    std::vector<std::string> args = {"login-info", "list", "--help"};
    BOOST_LOG_SEV(lg, debug) << "Args: " << args;

    auto result = p.parse(args, info, error);

    CHECK(!result.has_value());
    CHECK(!info.str().empty());
}
