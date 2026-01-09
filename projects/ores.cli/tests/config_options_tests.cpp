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
#include "ores.cli/config/import_options.hpp"
#include "ores.cli/config/export_options.hpp"
#include "ores.cli/config/delete_options.hpp"
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/format.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.cli.tests");
const std::string tags("[config]");

}

using namespace ores::cli::config;
using namespace ores::logging;

TEST_CASE("import_options_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing import_options construction";

    import_options sut;
    sut.target_entity = entity::currencies;
    sut.targets = {"/path/to/file1.xml", "/path/to/file2.xml"};

    CHECK(sut.target_entity == entity::currencies);
    REQUIRE(sut.targets.size() == 2);
    CHECK(sut.targets[0] == "/path/to/file1.xml");
    CHECK(sut.targets[1] == "/path/to/file2.xml");
}

TEST_CASE("import_options_streaming", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing import_options streaming";

    import_options sut;
    sut.target_entity = entity::accounts;
    sut.targets = {"/data/accounts.json"};

    std::ostringstream os;
    os << sut;
    const std::string output = os.str();

    BOOST_LOG_SEV(lg, debug) << "Output: " << output;

    CHECK(!output.empty());
}

TEST_CASE("import_options_all_entities", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing import_options with all entity types";

    import_options currencies_opts;
    currencies_opts.target_entity = entity::currencies;
    CHECK(currencies_opts.target_entity == entity::currencies);

    import_options accounts_opts;
    accounts_opts.target_entity = entity::accounts;
    CHECK(accounts_opts.target_entity == entity::accounts);

    import_options flags_opts;
    flags_opts.target_entity = entity::feature_flags;
    CHECK(flags_opts.target_entity == entity::feature_flags);

    import_options login_opts;
    login_opts.target_entity = entity::login_info;
    CHECK(login_opts.target_entity == entity::login_info);
}

TEST_CASE("export_options_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing export_options construction";

    export_options sut;
    sut.target_entity = entity::currencies;
    sut.as_of = "2025-01-01";
    sut.key = "USD";
    sut.all_versions = true;
    sut.target_format = format::json;

    CHECK(sut.target_entity == entity::currencies);
    CHECK(sut.as_of == "2025-01-01");
    CHECK(sut.key == "USD");
    CHECK(sut.all_versions);
    CHECK(sut.target_format == format::json);
}

TEST_CASE("export_options_streaming", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing export_options streaming";

    export_options sut;
    sut.target_entity = entity::accounts;
    sut.target_format = format::xml;

    std::ostringstream os;
    os << sut;
    const std::string output = os.str();

    BOOST_LOG_SEV(lg, debug) << "Output: " << output;

    CHECK(!output.empty());
}

TEST_CASE("export_options_all_formats", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing export_options with all format types";

    export_options json_opts;
    json_opts.target_format = format::json;
    CHECK(json_opts.target_format == format::json);

    export_options xml_opts;
    xml_opts.target_format = format::xml;
    CHECK(xml_opts.target_format == format::xml);

    export_options csv_opts;
    csv_opts.target_format = format::csv;
    CHECK(csv_opts.target_format == format::csv);

    export_options table_opts;
    table_opts.target_format = format::table;
    CHECK(table_opts.target_format == format::table);
}

TEST_CASE("export_options_default_values", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing export_options default values";

    export_options sut{};

    CHECK(sut.as_of.empty());
    CHECK(sut.key.empty());
    CHECK_FALSE(sut.all_versions);
}

TEST_CASE("delete_options_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing delete_options construction";

    delete_options sut;
    sut.target_entity = entity::currencies;
    sut.key = "USD";

    CHECK(sut.target_entity == entity::currencies);
    CHECK(sut.key == "USD");
}

TEST_CASE("delete_options_streaming", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing delete_options streaming";

    delete_options sut;
    sut.target_entity = entity::accounts;
    sut.key = "admin-account";

    std::ostringstream os;
    os << sut;
    const std::string output = os.str();

    BOOST_LOG_SEV(lg, debug) << "Output: " << output;

    CHECK(!output.empty());
}

TEST_CASE("delete_options_all_entities", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing delete_options with all entity types";

    delete_options currencies_opts;
    currencies_opts.target_entity = entity::currencies;
    currencies_opts.key = "EUR";
    CHECK(currencies_opts.target_entity == entity::currencies);

    delete_options accounts_opts;
    accounts_opts.target_entity = entity::accounts;
    accounts_opts.key = "user@example.com";
    CHECK(accounts_opts.target_entity == entity::accounts);

    delete_options flags_opts;
    flags_opts.target_entity = entity::feature_flags;
    flags_opts.key = "new-feature";
    CHECK(flags_opts.target_entity == entity::feature_flags);

    delete_options login_opts;
    login_opts.target_entity = entity::login_info;
    login_opts.key = "session-123";
    CHECK(login_opts.target_entity == entity::login_info);
}
