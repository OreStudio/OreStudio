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

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/faker/datetime.hpp"
#include "ores.refdata/domain/currency_version_table.hpp"
#include "ores.refdata/domain/currency_version_history_table.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[domain]");

using ores::utility::faker::datetime;

}

using ores::refdata::domain::currency;
using ores::refdata::domain::currency_version;
using ores::refdata::domain::currency_version_history;
using namespace ores::logging;

TEST_CASE("create_currency_version_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    currency_version cv;
    cv.data.iso_code = "USD";
    cv.data.name = "United States Dollar";
    cv.data.numeric_code = "840";
    cv.data.symbol = "$";
    cv.data.fraction_symbol = "¢";
    cv.data.fractions_per_unit = 100;
    cv.data.rounding_type = "Closest";
    cv.data.rounding_precision = 2;
    cv.data.format = "%3% %1$.2f";
    cv.data.currency_type = "Fiat";
    cv.data.recorded_by = "admin";
    cv.data.recorded_at = datetime::make_timepoint(2025, 1, 1);
    cv.version_number = 1;
    cv.recorded_by = "admin";
    cv.recorded_at = datetime::make_timepoint(2025, 1, 1, 10);
    cv.change_summary = "Created currency";

    BOOST_LOG_SEV(lg, debug) << "Currency version: " << cv.version_number;

    CHECK(cv.version_number == 1);
    CHECK(cv.data.iso_code == "USD");
    CHECK(cv.recorded_by == "admin");
    CHECK(cv.change_summary == "Created currency");
}

TEST_CASE("create_currency_version_with_faker", tags) {
    auto lg(make_logger(test_suite));

    currency_version cv;
    auto fakerCcy = faker::finance::currency();

    cv.data.iso_code = fakerCcy.code;
    cv.data.name = fakerCcy.name;
    cv.data.symbol = fakerCcy.symbol;
    cv.data.numeric_code = std::to_string(faker::number::integer(1, 999));
    cv.data.fraction_symbol = "";
    cv.data.fractions_per_unit = faker::number::integer(1, 10000);
    cv.data.rounding_type = "Closest";
    cv.data.rounding_precision = faker::number::integer(0, 5);
    cv.data.format = "%3% %1$.2f";
    cv.data.currency_type = "Fiat";
    cv.data.recorded_by = std::string(faker::internet::username());
    cv.data.recorded_at = {};
    cv.version_number = faker::number::integer(1, 100);
    cv.recorded_by = std::string(faker::internet::username());
    cv.recorded_at = datetime::make_timepoint(2025, faker::number::integer(1, 12), faker::number::integer(1, 28));
    cv.change_summary = std::string(faker::lorem::sentence());

    BOOST_LOG_SEV(lg, debug) << "Currency version: " << cv.version_number;

    CHECK(cv.version_number >= 1);
    CHECK(!cv.data.iso_code.empty());
    CHECK(!cv.recorded_by.empty());
}

TEST_CASE("currency_version_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<currency_version> versions;
    for (int i = 0; i < 3; ++i) {
        currency_version cv;
        cv.data.iso_code = "USD";
        cv.data.name = "United States Dollar";
        cv.data.numeric_code = "840";
        cv.data.symbol = "$";
        cv.data.fraction_symbol = "";
        cv.data.fractions_per_unit = 100;
        cv.data.rounding_type = "Closest";
        cv.data.rounding_precision = 2;
        cv.data.format = "%3% %1$.2f";
        cv.data.currency_type = "Fiat";
        cv.data.recorded_by = "admin";
        cv.data.recorded_at = {};
        cv.version_number = i + 1;
        cv.recorded_by = "user" + std::to_string(i);
        cv.recorded_at = datetime::make_timepoint(2025, 1, i + 1);
        cv.change_summary = "Change " + std::to_string(i);
        versions.push_back(cv);
    }

    auto table = convert_to_table(versions);

    BOOST_LOG_SEV(lg, debug) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("USD") != std::string::npos);
    CHECK(table.find("user0") != std::string::npos);
    CHECK(table.find("user1") != std::string::npos);
    CHECK(table.find("user2") != std::string::npos);
}

TEST_CASE("currency_version_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<currency_version> versions;
    for (int i = 0; i < 5; ++i) {
        currency_version cv;
        auto fakerCcy = faker::finance::currency();

        cv.data.iso_code = fakerCcy.code;
        cv.data.name = fakerCcy.name;
        cv.data.symbol = fakerCcy.symbol;
        cv.data.numeric_code = std::to_string(faker::number::integer(1, 999));
        cv.data.fraction_symbol = "";
        cv.data.fractions_per_unit = faker::number::integer(1, 10000);
        cv.data.rounding_type = "Closest";
        cv.data.rounding_precision = faker::number::integer(0, 5);
        cv.data.format = "%3% %1$.2f";
        cv.data.currency_type = "Fiat";
        cv.data.recorded_by = std::string(faker::internet::username());
        cv.data.recorded_at = {};
        cv.version_number = i + 1;
        cv.recorded_by = std::string(faker::internet::username());
        cv.recorded_at = datetime::make_timepoint(2025, faker::number::integer(1, 12), faker::number::integer(1, 28));
        cv.change_summary = std::string(faker::lorem::sentence());
        versions.push_back(cv);
    }

    auto table = convert_to_table(versions);

    BOOST_LOG_SEV(lg, debug) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& cv : versions) {
        CHECK(table.find(cv.data.iso_code) != std::string::npos);
    }
}

TEST_CASE("create_currency_version_history_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    currency_version_history cvh;
    cvh.iso_code = "USD";

    currency_version cv1;
    cv1.data.iso_code = "USD";
    cv1.data.name = "United States Dollar";
    cv1.version_number = 1;
    cv1.recorded_by = "admin";
    cv1.recorded_at = datetime::make_timepoint(2025, 1, 1);
    cv1.change_summary = "Created currency";

    currency_version cv2;
    cv2.data.iso_code = "USD";
    cv2.data.name = "US Dollar";
    cv2.version_number = 2;
    cv2.recorded_by = "admin";
    cv2.recorded_at = datetime::make_timepoint(2025, 1, 2);
    cv2.change_summary = "Updated name";

    cvh.versions.push_back(cv2);
    cvh.versions.push_back(cv1);

    BOOST_LOG_SEV(lg, debug) << "Currency version history for: " << cvh.iso_code;

    CHECK(cvh.iso_code == "USD");
    CHECK(cvh.versions.size() == 2);
    CHECK(cvh.versions[0].version_number == 2);
    CHECK(cvh.versions[1].version_number == 1);
}

TEST_CASE("currency_version_history_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<currency_version_history> histories;
    for (int i = 0; i < 3; ++i) {
        currency_version_history cvh;
        cvh.iso_code = std::string("XX") + std::to_string(i);

        for (int j = 0; j < 2; ++j) {
            currency_version cv;
            cv.data.iso_code = cvh.iso_code;
            cv.data.name = "Currency " + std::to_string(i);
            cv.version_number = j + 1;
            cv.recorded_by = "user";
            cv.recorded_at = datetime::make_timepoint(2025, 1, 1);
            cv.change_summary = "Version " + std::to_string(j + 1);
            cvh.versions.push_back(cv);
        }
        histories.push_back(cvh);
    }

    auto table = convert_to_table(histories);

    BOOST_LOG_SEV(lg, debug) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("XX0") != std::string::npos);
    CHECK(table.find("XX1") != std::string::npos);
    CHECK(table.find("XX2") != std::string::npos);
}

TEST_CASE("currency_version_history_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<currency_version_history> histories;
    for (int i = 0; i < 5; ++i) {
        currency_version_history cvh;
        auto fakerCcy = faker::finance::currency();
        cvh.iso_code = fakerCcy.code;

        int num_versions = faker::number::integer(1, 10);
        for (int j = 0; j < num_versions; ++j) {
            currency_version cv;
            cv.data.iso_code = cvh.iso_code;
            cv.data.name = fakerCcy.name;
            cv.version_number = j + 1;
            cv.recorded_by = std::string(faker::internet::username());
            cv.recorded_at = datetime::make_timepoint(2025, faker::number::integer(1, 12), faker::number::integer(1, 28));
            cv.change_summary = std::string(faker::lorem::sentence());
            cvh.versions.push_back(cv);
        }
        histories.push_back(cvh);
    }

    auto table = convert_to_table(histories);

    BOOST_LOG_SEV(lg, debug) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& cvh : histories) {
        CHECK(table.find(cvh.iso_code) != std::string::npos);
    }
}
