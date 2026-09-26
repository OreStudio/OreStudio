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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.service/config/standard_service_options.hpp"
#include <boost/program_options.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[standard_service_options]");
const std::string log_file("ores.service.tests.log");
const std::string app_name("SERVICE_TESTS");

}

using ores::service::config::standard_service_options;

TEST_CASE("the common options describe help and version", tags) {
    const auto od = standard_service_options::make_options_description(log_file);

    const auto help = standard_service_options::parse(od, {"--help"}, app_name);
    REQUIRE(standard_service_options::wants_help(help));
    REQUIRE_FALSE(standard_service_options::wants_version(help));

    const auto version = standard_service_options::parse(od, {"--version"}, app_name);
    REQUIRE_FALSE(standard_service_options::wants_help(version));
    REQUIRE(standard_service_options::wants_version(version));
}

TEST_CASE("the application's own options are merged into the shared set", tags) {
    boost::program_options::options_description extra("Extra");
    extra.add_options()("custom", boost::program_options::value<std::string>(), "A custom option");

    const auto od = standard_service_options::make_options_description(log_file, extra);
    const auto vm = standard_service_options::parse(od, {"--custom=hello"}, app_name);

    REQUIRE(vm["custom"].as<std::string>() == "hello");
}

TEST_CASE("read_options reports no logging until logging is switched on", tags) {
    const auto od = standard_service_options::make_options_description(log_file);
    const std::vector<std::string> required{"--db-user", "ores_test_user", "--db-database", "ores_test_db"};

    const auto off = standard_service_options::read_options(
        standard_service_options::parse(od, required, app_name));
    REQUIRE_FALSE(off.logging.has_value());

    auto with_logging = required;
    with_logging.push_back("--log-enabled");
    const auto on = standard_service_options::read_options(
        standard_service_options::parse(od, with_logging, app_name));
    REQUIRE(on.logging.has_value());
    REQUIRE(on.logging->filename == log_file);
    REQUIRE(on.logging->severity == "info");
}

TEST_CASE("read_options reads the database sub-options it was given", tags) {
    const auto od = standard_service_options::make_options_description(log_file);
    const auto vm = standard_service_options::parse(
        od, {"--db-user", "ores_test_user", "--db-database", "ores_test_db"}, app_name);

    const auto opts = standard_service_options::read_options(vm);

    REQUIRE(opts.database.user == "ores_test_user");
    REQUIRE(opts.database.database == "ores_test_db");
    REQUIRE(opts.database.host == "localhost");
    REQUIRE(opts.database.port == 5432);
}
