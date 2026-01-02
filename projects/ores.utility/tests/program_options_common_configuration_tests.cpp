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
#include "ores.utility/program_options/common_configuration.hpp"

#include <vector>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string test_suite("ores.utility.tests");
const std::string tags("[common_configuration]");

namespace po = boost::program_options;
using ores::utility::program_options::common_configuration;
using ores::utility::program_options::common_options;

po::variables_map parse_args(const std::vector<const char*>& args) {
    auto desc = common_configuration::make_options_description();
    po::variables_map vm;
    po::store(po::parse_command_line(
        static_cast<int>(args.size()), args.data(), desc), vm);
    po::notify(vm);
    return vm;
}

}

TEST_CASE("make_options_description_returns_general_group", tags) {
    auto desc = common_configuration::make_options_description();

    // Check that the description is not empty
    REQUIRE(desc.options().size() == 3);
}

TEST_CASE("make_options_description_contains_help_option", tags) {
    auto desc = common_configuration::make_options_description();

    // Should contain help option with short form
    const auto* help = desc.find_nothrow("help", false);
    REQUIRE(help != nullptr);
}

TEST_CASE("make_options_description_contains_version_option", tags) {
    auto desc = common_configuration::make_options_description();

    // Should contain version option with short form
    const auto* version = desc.find_nothrow("version", false);
    REQUIRE(version != nullptr);
}

TEST_CASE("make_options_description_contains_verbose_option", tags) {
    auto desc = common_configuration::make_options_description();

    // Should contain verbose option (long form only)
    const auto* verbose = desc.find_nothrow("verbose", false);
    REQUIRE(verbose != nullptr);
}

TEST_CASE("read_options_returns_verbose_false_when_not_set", tags) {
    std::vector<const char*> args = {"test"};
    auto vm = parse_args(args);

    auto opts = common_configuration::read_options(vm);

    REQUIRE(opts.verbose == false);
}

TEST_CASE("read_options_returns_verbose_true_when_set", tags) {
    std::vector<const char*> args = {"test", "--verbose"};
    auto vm = parse_args(args);

    auto opts = common_configuration::read_options(vm);

    REQUIRE(opts.verbose == true);
}

TEST_CASE("parse_help_short_form", tags) {
    std::vector<const char*> args = {"test", "-h"};
    auto desc = common_configuration::make_options_description();
    po::variables_map vm;

    // -h should be recognized (will throw if not)
    REQUIRE_NOTHROW(po::store(po::parse_command_line(
        static_cast<int>(args.size()), args.data(), desc), vm));
    REQUIRE(vm.count("help") != 0);
}

TEST_CASE("parse_version_short_form", tags) {
    std::vector<const char*> args = {"test", "-v"};
    auto desc = common_configuration::make_options_description();
    po::variables_map vm;

    // -v should be recognized
    REQUIRE_NOTHROW(po::store(po::parse_command_line(
        static_cast<int>(args.size()), args.data(), desc), vm));
    REQUIRE(vm.count("version") != 0);
}

TEST_CASE("parse_help_long_form", tags) {
    std::vector<const char*> args = {"test", "--help"};
    auto desc = common_configuration::make_options_description();
    po::variables_map vm;

    REQUIRE_NOTHROW(po::store(po::parse_command_line(
        static_cast<int>(args.size()), args.data(), desc), vm));
    REQUIRE(vm.count("help") != 0);
}

TEST_CASE("parse_version_long_form", tags) {
    std::vector<const char*> args = {"test", "--version"};
    auto desc = common_configuration::make_options_description();
    po::variables_map vm;

    REQUIRE_NOTHROW(po::store(po::parse_command_line(
        static_cast<int>(args.size()), args.data(), desc), vm));
    REQUIRE(vm.count("version") != 0);
}

TEST_CASE("parse_verbose_long_form", tags) {
    std::vector<const char*> args = {"test", "--verbose"};
    auto desc = common_configuration::make_options_description();
    po::variables_map vm;

    REQUIRE_NOTHROW(po::store(po::parse_command_line(
        static_cast<int>(args.size()), args.data(), desc), vm));
    REQUIRE(vm.count("verbose") != 0);
}
