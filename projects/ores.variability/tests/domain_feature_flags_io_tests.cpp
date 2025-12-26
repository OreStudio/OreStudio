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
#include "ores.utility/log/make_logger.hpp"
#include "ores.variability/domain/feature_flags_json_io.hpp" // IWYU pragma: keep.
#include "ores.variability/domain/feature_flags_table.hpp"
#include "ores.variability/domain/feature_flags_table_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[domain][io]");

}

using ores::variability::domain::feature_flags;
using namespace ores::utility::log;

TEST_CASE("feature_flags_json_output_contains_all_fields", tags) {
    auto lg(make_logger(test_suite));

    feature_flags sut;
    sut.version = 5;
    sut.name = "test_feature";
    sut.enabled = true;
    sut.description = "A test feature flag";
    sut.recorded_by = "admin";
    sut.recorded_at = std::chrono::system_clock::now();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("test_feature") != std::string::npos);
    CHECK(json_output.find("A test feature flag") != std::string::npos);
    CHECK(json_output.find("admin") != std::string::npos);
    CHECK(json_output.find("true") != std::string::npos);
}

TEST_CASE("feature_flags_json_output_with_disabled_flag", tags) {
    auto lg(make_logger(test_suite));

    feature_flags sut;
    sut.version = 1;
    sut.name = "disabled_feature";
    sut.enabled = false;
    sut.description = "A disabled feature";
    sut.recorded_by = "system";
    sut.recorded_at = std::chrono::system_clock::now();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("disabled_feature") != std::string::npos);
    CHECK(json_output.find("false") != std::string::npos);
}

TEST_CASE("feature_flags_json_output_with_faker", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 5; ++i) {
        feature_flags sut;
        sut.version = faker::number::integer(1, 100);
        sut.name = std::string(faker::word::noun()) + "_" +
            std::string(faker::word::verb());
        sut.enabled = faker::datatype::boolean();
        sut.description = std::string(faker::lorem::sentence());
        sut.recorded_by = std::string(faker::internet::username());
        sut.recorded_at = std::chrono::system_clock::now();

        std::ostringstream os;
        os << sut;
        const std::string json_output = os.str();

        BOOST_LOG_SEV(lg, info) << "Faker JSON " << i << ": "
                                << json_output.substr(0, 100);

        CHECK(!json_output.empty());
        CHECK(json_output.find(sut.name) != std::string::npos);
        CHECK(json_output.find(sut.recorded_by) != std::string::npos);
    }
}

TEST_CASE("feature_flags_vector_table_io_output", tags) {
    auto lg(make_logger(test_suite));

    std::vector<feature_flags> flags;
    for (int i = 0; i < 3; ++i) {
        feature_flags ff;
        ff.version = i + 1;
        ff.name = "feature_" + std::to_string(i);
        ff.enabled = (i % 2 == 0);
        ff.description = "Description " + std::to_string(i);
        ff.recorded_by = "user" + std::to_string(i);
        ff.recorded_at = std::chrono::system_clock::now();
        flags.push_back(ff);
    }

    std::ostringstream os;
    os << flags;
    const std::string table_output = os.str();

    BOOST_LOG_SEV(lg, info) << "Table IO output: " << table_output;

    CHECK(!table_output.empty());
    CHECK(table_output.find("feature_0") != std::string::npos);
    CHECK(table_output.find("feature_1") != std::string::npos);
    CHECK(table_output.find("feature_2") != std::string::npos);
}

TEST_CASE("feature_flags_empty_vector_table_io", tags) {
    auto lg(make_logger(test_suite));

    std::vector<feature_flags> empty_flags;

    std::ostringstream os;
    os << empty_flags;
    const std::string table_output = os.str();

    BOOST_LOG_SEV(lg, info) << "Empty table IO output: " << table_output;

    // Empty vector should still produce some output (header)
    CHECK(!table_output.empty());
}

TEST_CASE("feature_flags_table_io_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<feature_flags> flags;
    for (int i = 0; i < 5; ++i) {
        feature_flags ff;
        ff.version = faker::number::integer(1, 50);
        ff.name = std::string(faker::word::noun()) + "_flag";
        ff.enabled = faker::datatype::boolean();
        ff.description = std::string(faker::lorem::sentence());
        ff.recorded_by = std::string(faker::internet::username());
        ff.recorded_at = std::chrono::system_clock::now();
        flags.push_back(ff);
    }

    std::ostringstream os;
    os << flags;
    const std::string table_output = os.str();

    BOOST_LOG_SEV(lg, info) << "Faker table IO output:\n"
                            << table_output.substr(0, 500);

    CHECK(!table_output.empty());
    for (const auto& ff : flags) {
        CHECK(table_output.find(ff.name) != std::string::npos);
    }
}

TEST_CASE("feature_flags_print_table_function", tags) {
    auto lg(make_logger(test_suite));

    std::vector<feature_flags> flags;
    feature_flags ff;
    ff.version = 1;
    ff.name = "print_test_flag";
    ff.enabled = true;
    ff.description = "Testing print function";
    ff.recorded_by = "tester";
    ff.recorded_at = std::chrono::system_clock::now();
    flags.push_back(ff);

    std::ostringstream os;
    ores::variability::domain::print_feature_flags_table(os, flags);
    const std::string output = os.str();

    BOOST_LOG_SEV(lg, info) << "Print function output: " << output;

    CHECK(!output.empty());
    CHECK(output.find("print_test_flag") != std::string::npos);
    CHECK(output.find("Testing print function") != std::string::npos);
}

TEST_CASE("feature_flags_json_special_characters", tags) {
    auto lg(make_logger(test_suite));

    feature_flags sut;
    sut.version = 1;
    sut.name = "special_flag";
    sut.enabled = true;
    sut.description = "Description with \"quotes\" and 'apostrophes'";
    sut.recorded_by = "user@domain.com";
    sut.recorded_at = std::chrono::system_clock::now();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON with special chars: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("special_flag") != std::string::npos);
    CHECK(json_output.find("\\\"quotes\\\"") != std::string::npos);
}
