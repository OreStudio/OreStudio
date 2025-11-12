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
#include "ores.accounts/domain/feature_flags.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "faker-cxx/faker.h" // IWYU pragma: keep.

namespace {

const std::string test_suite("ores.accounts.tests");
const std::string tags("[domain_feature_flags_tests]");

}

using ores::accounts::domain::feature_flags;
using namespace ores::utility::log;

TEST_CASE("create_feature_flag_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    feature_flags flag;
    flag.name = "enable_dark_mode";
    flag.enabled = true;
    flag.description = "Enables dark mode across the application";
    flag.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << flag;

    CHECK(flag.name == "enable_dark_mode");
    CHECK(flag.enabled == true);
    CHECK(flag.description == "Enables dark mode across the application");
    CHECK(flag.modified_by == "admin");
}

TEST_CASE("create_disabled_feature_flag", tags) {
    auto lg(make_logger(test_suite));

    feature_flags flag;
    flag.name = "experimental_feature";
    flag.enabled = false;
    flag.description = "Experimental feature under development";
    flag.modified_by = "developer";
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << flag;

    CHECK(flag.name == "experimental_feature");
    CHECK(flag.enabled == false);
    CHECK(!flag.description.empty());
    CHECK(flag.modified_by == "developer");
}

TEST_CASE("feature_flag_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    feature_flags flag;
    flag.name = "api_rate_limiting";
    flag.enabled = true;
    flag.description = "Controls API rate limiting behavior";
    flag.modified_by = "sysadmin";
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << flag;

    std::ostringstream oss;
    oss << flag;
    const std::string json_output = oss.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("api_rate_limiting") != std::string::npos);
    CHECK(json_output.find("sysadmin") != std::string::npos);
}

TEST_CASE("create_feature_flag_with_faker", tags) {
    auto lg(make_logger(test_suite));

    feature_flags flag;
    flag.name = std::string(faker::word::noun()) + "_" +
        std::string(faker::word::verb());
    flag.enabled = faker::datatype::boolean();
    flag.description = std::string(faker::lorem::sentence());
    flag.modified_by = std::string(faker::internet::username());
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << flag;

    CHECK(!flag.name.empty());
    CHECK(!flag.description.empty());
    CHECK(!flag.modified_by.empty());
}

TEST_CASE("create_multiple_random_feature_flags", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> feature_prefixes = {
        "enable", "disable", "toggle", "allow", "restrict"
    };
    const std::vector<std::string> feature_subjects = {
        "notifications", "caching", "logging", "authentication", "analytics"
    };

    for (int i = 0; i < 5; ++i) {
        feature_flags flag;

        const auto prefix = feature_prefixes[faker::number::integer<size_t>(0,
            feature_prefixes.size() - 1)];
        const auto subject = feature_subjects[faker::number::integer<size_t>(0,
            feature_subjects.size() - 1)];

        flag.name = prefix + "_" + subject;
        flag.enabled = faker::datatype::boolean();
        flag.description = std::string(faker::lorem::sentence());
        flag.modified_by =
            std::string(faker::person::firstName()) + " " + std::string(faker::person::lastName());
        BOOST_LOG_SEV(lg, info) << "Feature flag: " << flag;

        CHECK(!flag.name.empty());
        CHECK(flag.name.find("_") != std::string::npos);
        CHECK(!flag.description.empty());
        CHECK(!flag.modified_by.empty());
    }
}

TEST_CASE("create_system_feature_flags", tags) {
    auto lg(make_logger(test_suite));

    feature_flags flag;
    flag.name = "maintenance_mode";
    flag.enabled = faker::datatype::boolean();
    flag.description = "When enabled, puts the system into maintenance mode";
    flag.modified_by = "system";
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << flag;

    CHECK(flag.name == "maintenance_mode");
    CHECK(flag.modified_by == "system");
}
