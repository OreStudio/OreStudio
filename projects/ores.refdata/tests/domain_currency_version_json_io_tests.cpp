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
#include "ores.refdata/domain/currency_version.hpp"
#include "ores.refdata/domain/currency_version_history.hpp"

#include <chrono>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/currency_version_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/currency_version_history_json_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.risk.tests");
const std::string tags("[domain][json]");

std::chrono::system_clock::time_point make_timepoint(int year, int month, int day) {
    std::tm tm = {};
    tm.tm_year = year - 1900;
    tm.tm_mon = month - 1;
    tm.tm_mday = day;
    tm.tm_hour = 12;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}

}

using ores::refdata::domain::currency;
using ores::refdata::domain::currency_version;
using ores::refdata::domain::currency_version_history;
using namespace ores::logging;

TEST_CASE("currency_version_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    currency_version cv;
    cv.data.iso_code = "EUR";
    cv.data.name = "Euro";
    cv.data.numeric_code = "978";
    cv.data.symbol = "€";
    cv.data.fraction_symbol = "cent";
    cv.data.fractions_per_unit = 100;
    cv.data.rounding_type = "Closest";
    cv.data.rounding_precision = 2;
    cv.data.format = "%3% %1$.2f";
    cv.data.currency_type = "Fiat";
    cv.data.recorded_by = "admin";
    cv.data.recorded_at = make_timepoint(2025, 1, 15);
    cv.version_number = 3;
    cv.recorded_by = "system";
    cv.recorded_at = make_timepoint(2025, 1, 20);
    cv.change_summary = "Updated symbol format";

    BOOST_LOG_SEV(lg, info) << "Currency version: " << cv;

    std::ostringstream os;
    os << cv;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("EUR") != std::string::npos);
    CHECK(json_output.find("Euro") != std::string::npos);
    CHECK(json_output.find("978") != std::string::npos);
    CHECK(json_output.find("system") != std::string::npos);
    CHECK(json_output.find("Updated symbol format") != std::string::npos);
}

TEST_CASE("currency_version_json_with_all_fields", tags) {
    auto lg(make_logger(test_suite));

    currency_version cv;
    cv.data.iso_code = "GBP";
    cv.data.name = "British Pound Sterling";
    cv.data.numeric_code = "826";
    cv.data.symbol = "£";
    cv.data.fraction_symbol = "pence";
    cv.data.fractions_per_unit = 100;
    cv.data.rounding_type = "Closest";
    cv.data.rounding_precision = 2;
    cv.data.format = "%3% %1$.2f";
    cv.data.currency_type = "Fiat";
    cv.data.recorded_by = "admin";
    cv.data.recorded_at = make_timepoint(2025, 2, 1);
    cv.version_number = 1;
    cv.recorded_by = "creator";
    cv.recorded_at = make_timepoint(2025, 2, 1);
    cv.change_summary = "Created currency";

    std::ostringstream os;
    os << cv;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("GBP") != std::string::npos);
    CHECK(json_output.find("British Pound Sterling") != std::string::npos);
    CHECK(json_output.find("826") != std::string::npos);
    CHECK(json_output.find("pence") != std::string::npos);
}

TEST_CASE("currency_version_json_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    currency_version cv;
    auto fakerCcy = faker::finance::currency();

    cv.data.iso_code = fakerCcy.code;
    cv.data.name = fakerCcy.name;
    cv.data.numeric_code = std::to_string(faker::number::integer(1, 999));
    cv.data.symbol = fakerCcy.symbol;
    cv.data.fraction_symbol = "";
    cv.data.fractions_per_unit = faker::number::integer(1, 1000);
    cv.data.rounding_type = "Closest";
    cv.data.rounding_precision = faker::number::integer(0, 4);
    cv.data.format = "%3% %1$.2f";
    cv.data.currency_type = "Fiat";
    cv.data.recorded_by = std::string(faker::internet::username());
    cv.data.recorded_at = std::chrono::system_clock::now();
    cv.version_number = faker::number::integer(1, 50);
    cv.recorded_by = std::string(faker::internet::username());
    cv.recorded_at = std::chrono::system_clock::now();
    cv.change_summary = std::string(faker::lorem::sentence());

    std::ostringstream os;
    os << cv;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "Faker JSON: " << json_output.substr(0, 200);

    CHECK(!json_output.empty());
    CHECK(json_output.find(cv.data.iso_code) != std::string::npos);
    CHECK(json_output.find(cv.data.name) != std::string::npos);
}

TEST_CASE("currency_version_history_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    currency_version_history cvh;
    cvh.iso_code = "USD";

    currency_version v1;
    v1.data.iso_code = "USD";
    v1.data.name = "United States Dollar";
    v1.data.numeric_code = "840";
    v1.data.symbol = "$";
    v1.data.fraction_symbol = "cent";
    v1.data.fractions_per_unit = 100;
    v1.data.rounding_type = "Closest";
    v1.data.rounding_precision = 2;
    v1.data.format = "%3% %1$.2f";
    v1.data.currency_type = "Fiat";
    v1.data.recorded_by = "admin";
    v1.data.recorded_at = make_timepoint(2025, 1, 1);
    v1.version_number = 1;
    v1.recorded_by = "admin";
    v1.recorded_at = make_timepoint(2025, 1, 1);
    v1.change_summary = "Created currency";
    cvh.versions.push_back(v1);

    currency_version v2;
    v2.data.iso_code = "USD";
    v2.data.name = "US Dollar";
    v2.data.numeric_code = "840";
    v2.data.symbol = "$";
    v2.data.fraction_symbol = "cent";
    v2.data.fractions_per_unit = 100;
    v2.data.rounding_type = "Closest";
    v2.data.rounding_precision = 2;
    v2.data.format = "%3% %1$.2f";
    v2.data.currency_type = "Fiat";
    v2.data.recorded_by = "editor";
    v2.data.recorded_at = make_timepoint(2025, 1, 15);
    v2.version_number = 2;
    v2.recorded_by = "editor";
    v2.recorded_at = make_timepoint(2025, 1, 15);
    v2.change_summary = "Updated name";
    cvh.versions.push_back(v2);

    BOOST_LOG_SEV(lg, info) << "Currency version history: " << cvh;

    std::ostringstream os;
    os << cvh;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("USD") != std::string::npos);
    CHECK(json_output.find("United States Dollar") != std::string::npos);
    CHECK(json_output.find("US Dollar") != std::string::npos);
    CHECK(json_output.find("Created currency") != std::string::npos);
    CHECK(json_output.find("Updated name") != std::string::npos);
}

TEST_CASE("currency_version_history_json_with_empty_versions", tags) {
    auto lg(make_logger(test_suite));

    currency_version_history cvh;
    cvh.iso_code = "XYZ";

    BOOST_LOG_SEV(lg, info) << "Empty currency version history: " << cvh;

    std::ostringstream os;
    os << cvh;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("XYZ") != std::string::npos);
    CHECK(json_output.find("versions") != std::string::npos);
}

TEST_CASE("currency_version_history_json_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    currency_version_history cvh;
    auto fakerCcy = faker::finance::currency();
    cvh.iso_code = fakerCcy.code;

    int num_versions = faker::number::integer(1, 5);
    for (int i = 0; i < num_versions; ++i) {
        currency_version cv;
        cv.data.iso_code = cvh.iso_code;
        cv.data.name = fakerCcy.name;
        cv.data.numeric_code = std::to_string(faker::number::integer(1, 999));
        cv.data.symbol = fakerCcy.symbol;
        cv.data.fraction_symbol = "";
        cv.data.fractions_per_unit = faker::number::integer(1, 1000);
        cv.data.rounding_type = "Closest";
        cv.data.rounding_precision = faker::number::integer(0, 4);
        cv.data.format = "%3% %1$.2f";
        cv.data.currency_type = "Fiat";
        cv.data.recorded_by = std::string(faker::internet::username());
        cv.data.recorded_at = std::chrono::system_clock::now();
        cv.version_number = i + 1;
        cv.recorded_by = std::string(faker::internet::username());
        cv.recorded_at = std::chrono::system_clock::now();
        cv.change_summary = std::string(faker::lorem::sentence());
        cvh.versions.push_back(cv);
    }

    std::ostringstream os;
    os << cvh;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "Faker JSON history (truncated): "
                            << json_output.substr(0, 300);

    CHECK(!json_output.empty());
    CHECK(json_output.find(cvh.iso_code) != std::string::npos);
}

TEST_CASE("multiple_currency_versions_json_output", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        currency_version cv;
        auto fakerCcy = faker::finance::currency();

        cv.data.iso_code = fakerCcy.code;
        cv.data.name = fakerCcy.name;
        cv.data.numeric_code = std::to_string(faker::number::integer(1, 999));
        cv.data.symbol = fakerCcy.symbol;
        cv.data.fraction_symbol = "";
        cv.data.fractions_per_unit = 100;
        cv.data.rounding_type = "Closest";
        cv.data.rounding_precision = 2;
        cv.data.format = "%3% %1$.2f";
        cv.data.currency_type = "Fiat";
        cv.data.recorded_by = "user" + std::to_string(i);
        cv.data.recorded_at = std::chrono::system_clock::now();
        cv.version_number = i + 1;
        cv.recorded_by = "user" + std::to_string(i);
        cv.recorded_at = std::chrono::system_clock::now();
        cv.change_summary = "Version " + std::to_string(i + 1);

        std::ostringstream os;
        os << cv;
        const std::string json_output = os.str();

        BOOST_LOG_SEV(lg, info) << "Currency version " << i << ": "
                                << json_output.substr(0, 100);

        CHECK(!json_output.empty());
        CHECK(json_output.find(cv.data.iso_code) != std::string::npos);
        CHECK(json_output.find("user" + std::to_string(i)) != std::string::npos);
    }
}
