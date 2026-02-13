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
#include "ores.variability/domain/feature_flags.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.variability/domain/feature_flags_json_io.hpp" // IWYU pragma: keep.
#include "ores.variability/domain/feature_flags_table.hpp"

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[domain]");

}

using ores::variability::domain::feature_flags;
using namespace ores::logging;

TEST_CASE("create_feature_flag_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    feature_flags sut;
    sut.name = "enable_dark_mode";
    sut.enabled = true;
    sut.description = "Enables dark mode across the application";
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << sut;

    CHECK(sut.name == "enable_dark_mode");
    CHECK(sut.enabled == true);
    CHECK(sut.description == "Enables dark mode across the application");
    CHECK(sut.modified_by == "admin");
}

TEST_CASE("create_disabled_feature_flag", tags) {
    auto lg(make_logger(test_suite));

    feature_flags sut;
    sut.name = "experimental_feature";
    sut.enabled = false;
    sut.description = "Experimental feature under development";
    sut.modified_by = "developer";
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << sut;

    CHECK(sut.name == "experimental_feature");
    CHECK(sut.enabled == false);
    CHECK(!sut.description.empty());
    CHECK(sut.modified_by == "developer");
}

TEST_CASE("feature_flag_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    feature_flags sut;
    sut.name = "api_rate_limiting";
    sut.enabled = true;
    sut.description = "Controls API rate limiting behavior";
    sut.modified_by = "sysadmin";
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("api_rate_limiting") != std::string::npos);
    CHECK(json_output.find("sysadmin") != std::string::npos);
}

TEST_CASE("create_feature_flag_with_faker", tags) {
    auto lg(make_logger(test_suite));

    feature_flags sut;
    sut.name = std::string(faker::word::noun()) + "_" +
        std::string(faker::word::verb());
    sut.enabled = faker::datatype::boolean();
    sut.description = std::string(faker::lorem::sentence());
    sut.modified_by = std::string(faker::internet::username());
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << sut;

    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
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
        feature_flags sut;

        const auto prefix =
            feature_prefixes[faker::number::integer<size_t>(0,
            feature_prefixes.size() - 1)];
        const auto subject =
            feature_subjects[faker::number::integer<size_t>(0,
            feature_subjects.size() - 1)];

        sut.name = prefix + "_" + subject;
        sut.enabled = faker::datatype::boolean();
        sut.description = std::string(faker::lorem::sentence());
        sut.modified_by = std::string(faker::internet::username());

        BOOST_LOG_SEV(lg, info) << "Feature flag: " << sut;

        CHECK(!sut.name.empty());
        CHECK(sut.name.find("_") != std::string::npos);
        CHECK(!sut.description.empty());
        CHECK(!sut.modified_by.empty());
    }
}

TEST_CASE("create_system_feature_flags", tags) {
    auto lg(make_logger(test_suite));

    feature_flags sut;
    sut.name = "maintenance_mode";
    sut.enabled = faker::datatype::boolean();
    sut.description = "When enabled, puts the system into maintenance mode";
    sut.modified_by = "system";
    BOOST_LOG_SEV(lg, info) << "Feature flag: " << sut;

    CHECK(sut.name == "maintenance_mode");
    CHECK(sut.modified_by == "system");
}

TEST_CASE("feature_flags_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    feature_flags ff;
    ff.name = "dark_mode";
    ff.enabled = true;
    ff.description = "Enables dark mode";
    ff.modified_by = "admin";

    std::vector<feature_flags> flags = {ff};
    auto table = convert_to_table(flags);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("dark_mode") != std::string::npos);
    CHECK(table.find("Enables dark mode") != std::string::npos);
}

TEST_CASE("feature_flags_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<feature_flags> flags;
    for (int i = 0; i < 3; ++i) {
        feature_flags ff;
        ff.name = "feature_" + std::to_string(i);
        ff.enabled = (i % 2 == 0);
        ff.description = "Description for feature " + std::to_string(i);
        ff.modified_by = "system";
        flags.push_back(ff);
    }

    auto table = convert_to_table(flags);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("feature_0") != std::string::npos);
    CHECK(table.find("feature_1") != std::string::npos);
    CHECK(table.find("feature_2") != std::string::npos);
}

TEST_CASE("feature_flags_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<feature_flags> flags;
    for (int i = 0; i < 5; ++i) {
        feature_flags ff;
        ff.name = std::string(faker::word::noun()) + "_feature";
        ff.enabled = faker::datatype::boolean();
        ff.description = std::string(faker::lorem::sentence());
        ff.modified_by = std::string(faker::internet::username());
        flags.push_back(ff);
    }

    auto table = convert_to_table(flags);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    // Verify all feature names appear in table
    for (const auto& ff : flags) {
        CHECK(table.find(ff.name) != std::string::npos);
    }
}
