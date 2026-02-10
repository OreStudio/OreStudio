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
#include "ores.cli/config/parser_helpers.hpp"
#include "ores.cli/config/parser_exception.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[parser_helpers]");

using namespace ores::cli::config;
namespace ph = ores::cli::config::parser_helpers;

/**
 * @brief Helper to create a populated variables_map from command line args.
 */
boost::program_options::variables_map
make_vm(const boost::program_options::options_description& desc,
        const std::vector<std::string>& args) {
    namespace po = boost::program_options;
    po::variables_map vm;
    auto parsed = po::command_line_parser(args).options(desc).run();
    po::store(parsed, vm);
    po::notify(vm);
    return vm;
}

}

// --- validate_operation ---

TEST_CASE("validate_operation_accepts_allowed_operation", tags) {
    const std::vector<std::string> allowed = {"list", "delete", "add"};
    REQUIRE_NOTHROW(ph::validate_operation("currencies", "list", allowed));
    REQUIRE_NOTHROW(ph::validate_operation("currencies", "delete", allowed));
    REQUIRE_NOTHROW(ph::validate_operation("currencies", "add", allowed));
}

TEST_CASE("validate_operation_throws_on_invalid_operation", tags) {
    const std::vector<std::string> allowed = {"list", "delete"};
    REQUIRE_THROWS_AS(
        ph::validate_operation("currencies", "import", allowed),
        parser_exception);
}

TEST_CASE("validate_operation_throws_on_empty_operation", tags) {
    const std::vector<std::string> allowed = {"list", "delete"};
    REQUIRE_THROWS_AS(
        ph::validate_operation("currencies", "", allowed),
        parser_exception);
}

// --- read_format ---

TEST_CASE("read_format_defaults_to_json_when_not_specified", tags) {
    auto desc = ph::make_export_options_description();
    auto vm = make_vm(desc, {});

    auto f = ph::read_format(vm);
    REQUIRE(f == format::json);
}

TEST_CASE("read_format_parses_all_valid_formats", tags) {
    auto desc = ph::make_export_options_description();

    auto vm_json = make_vm(desc, {"--format", "json"});
    CHECK(ph::read_format(vm_json) == format::json);

    auto vm_xml = make_vm(desc, {"--format", "xml"});
    CHECK(ph::read_format(vm_xml) == format::xml);

    auto vm_csv = make_vm(desc, {"--format", "csv"});
    CHECK(ph::read_format(vm_csv) == format::csv);

    auto vm_table = make_vm(desc, {"--format", "table"});
    CHECK(ph::read_format(vm_table) == format::table);
}

TEST_CASE("read_format_throws_on_invalid_format", tags) {
    auto desc = ph::make_export_options_description();
    auto vm = make_vm(desc, {"--format", "invalid_format"});

    REQUIRE_THROWS_AS(ph::read_format(vm), parser_exception);
}

// --- read_export_options ---

TEST_CASE("read_export_options_with_defaults", tags) {
    auto desc = ph::make_export_options_description();
    auto vm = make_vm(desc, {});

    auto opts = ph::read_export_options(vm, entity::currencies);

    CHECK(opts.target_entity == entity::currencies);
    CHECK(opts.target_format == format::json);
    CHECK(!opts.all_versions);
    CHECK(opts.as_of.empty());
    CHECK(opts.key.empty());
}

TEST_CASE("read_export_options_with_all_flags", tags) {
    auto desc = ph::make_export_options_description();
    auto vm = make_vm(desc, {
        "--format", "xml",
        "--as-of", "2025-01-01",
        "--key", "USD",
        "--all-versions"
    });

    auto opts = ph::read_export_options(vm, entity::currencies);

    CHECK(opts.target_entity == entity::currencies);
    CHECK(opts.target_format == format::xml);
    CHECK(opts.all_versions);
    CHECK(opts.as_of == "2025-01-01");
    CHECK(opts.key == "USD");
}

// --- read_delete_options ---

TEST_CASE("read_delete_options_with_key", tags) {
    auto desc = ph::make_delete_options_description();
    auto vm = make_vm(desc, {"--key", "test_user"});

    auto opts = ph::read_delete_options(vm, entity::accounts);

    CHECK(opts.target_entity == entity::accounts);
    CHECK(opts.key == "test_user");
}

TEST_CASE("read_delete_options_throws_without_key", tags) {
    auto desc = ph::make_delete_options_description();
    auto vm = make_vm(desc, {});

    REQUIRE_THROWS_AS(
        ph::read_delete_options(vm, entity::accounts),
        parser_exception);
}

// --- print_entity_help ---

TEST_CASE("print_entity_help_produces_expected_output", tags) {
    std::ostringstream ss;
    const std::vector<std::pair<std::string, std::string>> operations = {
        {"list", "List all currencies"},
        {"delete", "Delete a currency"}
    };

    ph::print_entity_help("currencies", "Manage currencies", operations, ss);
    const auto output = ss.str();

    CHECK(output.find("currencies") != std::string::npos);
    CHECK(output.find("Manage currencies") != std::string::npos);
    CHECK(output.find("list") != std::string::npos);
    CHECK(output.find("delete") != std::string::npos);
    CHECK(output.find("ores.cli") != std::string::npos);
}

// --- print_help_header ---

TEST_CASE("print_help_header_produces_nonempty_output", tags) {
    std::ostringstream ss;
    ph::print_help_header(ss);

    CHECK(!ss.str().empty());
    CHECK(ss.str().find("ORE Studio") != std::string::npos);
}
