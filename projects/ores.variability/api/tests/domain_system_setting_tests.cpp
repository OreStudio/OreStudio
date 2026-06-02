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
#include "ores.variability.api/domain/system_setting.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.variability.api/domain/system_setting_json_io.hpp" // IWYU pragma: keep.
#include "ores.variability.api/domain/system_setting_table_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[domain][system_setting]");

}

using ores::variability::domain::system_setting;
using namespace ores::logging;

TEST_CASE("create_boolean_system_setting_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    system_setting sut;
    sut.name = "system.bootstrap_mode";
    sut.value = "true";
    sut.data_type = "boolean";
    sut.description = "Controls whether the system is in bootstrap mode";
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "System setting: " << sut;

    CHECK(sut.name == "system.bootstrap_mode");
    CHECK(sut.value == "true");
    CHECK(sut.data_type == "boolean");
    CHECK(sut.description == "Controls whether the system is in bootstrap mode");
    CHECK(sut.modified_by == "admin");
}

TEST_CASE("create_integer_system_setting", tags) {
    auto lg(make_logger(test_suite));

    system_setting sut;
    sut.name = "system.max_retries";
    sut.value = "5";
    sut.data_type = "integer";
    sut.description = "Maximum number of retries for failed operations";
    sut.modified_by = "system";
    BOOST_LOG_SEV(lg, info) << "System setting: " << sut;

    CHECK(sut.name == "system.max_retries");
    CHECK(sut.value == "5");
    CHECK(sut.data_type == "integer");
}

TEST_CASE("create_string_system_setting", tags) {
    auto lg(make_logger(test_suite));

    system_setting sut;
    sut.name = "system.default_locale";
    sut.value = "en-GB";
    sut.data_type = "string";
    sut.description = "Default locale for the application";
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "System setting: " << sut;

    CHECK(sut.name == "system.default_locale");
    CHECK(sut.value == "en-GB");
    CHECK(sut.data_type == "string");
}

TEST_CASE("create_json_system_setting", tags) {
    auto lg(make_logger(test_suite));

    system_setting sut;
    sut.name = "system.rate_limits";
    sut.value = R"({"requests_per_minute": 100, "burst": 200})";
    sut.data_type = "json";
    sut.description = "Rate limiting configuration";
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "System setting: " << sut;

    CHECK(sut.name == "system.rate_limits");
    CHECK(sut.data_type == "json");
    CHECK(sut.value.find("100") != std::string::npos);
}

TEST_CASE("default_constructed_system_setting_has_zero_version", tags) {
    auto lg(make_logger(test_suite));

    const system_setting sut;
    BOOST_LOG_SEV(lg, info) << "Default system setting version: " << sut.version;

    CHECK(sut.version == 0);
    CHECK(sut.name.empty());
    CHECK(sut.value.empty());
    CHECK(sut.data_type.empty());
}

TEST_CASE("system_setting_version_is_set_correctly", tags) {
    auto lg(make_logger(test_suite));

    system_setting sut;
    sut.version = 7;
    sut.name = "system.user_signups";
    sut.value = "false";
    sut.data_type = "boolean";
    BOOST_LOG_SEV(lg, info) << "System setting version: " << sut.version;

    CHECK(sut.version == 7);
}

TEST_CASE("system_setting_json_output_contains_name_and_value", tags) {
    auto lg(make_logger(test_suite));

    system_setting sut;
    sut.version = 3;
    sut.name = "system.feature_x";
    sut.value = "true";
    sut.data_type = "boolean";
    sut.description = "Controls feature X";
    sut.modified_by = "admin";
    sut.recorded_at = std::chrono::system_clock::now();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("system.feature_x") != std::string::npos);
    CHECK(json_output.find("Controls feature X") != std::string::npos);
    CHECK(json_output.find("admin") != std::string::npos);
    CHECK(json_output.find("boolean") != std::string::npos);
}

TEST_CASE("system_setting_json_output_for_disabled_setting", tags) {
    auto lg(make_logger(test_suite));

    system_setting sut;
    sut.version = 1;
    sut.name = "system.experimental";
    sut.value = "false";
    sut.data_type = "boolean";
    sut.description = "Experimental feature";
    sut.modified_by = "system";
    sut.recorded_at = std::chrono::system_clock::now();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("false") != std::string::npos);
    CHECK(json_output.find("system.experimental") != std::string::npos);
}

TEST_CASE("system_setting_provenance_fields_are_stored", tags) {
    auto lg(make_logger(test_suite));

    system_setting sut;
    sut.name = "system.maintenance_mode";
    sut.value = "false";
    sut.data_type = "boolean";
    sut.change_reason_code = "SCHEDULED_MAINTENANCE";
    sut.change_commentary = "Disabling maintenance mode after upgrade";
    sut.performed_by = "ops_user";
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "System setting: " << sut;

    CHECK(sut.change_reason_code == "SCHEDULED_MAINTENANCE");
    CHECK(sut.change_commentary == "Disabling maintenance mode after upgrade");
    CHECK(sut.performed_by == "ops_user");
}
