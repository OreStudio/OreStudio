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
#include "ores.iam/domain/role.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <sstream>
#include <iomanip>
#include <chrono>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.iam/domain/role_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/permission.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[domain]");

std::chrono::system_clock::time_point make_timepoint(int year, int month, int day, int hour = 0, int min = 0) {
    std::tm tm = {};
    tm.tm_year = year - 1900;
    tm.tm_mon = month - 1;
    tm.tm_mday = day;
    tm.tm_hour = hour;
    tm.tm_min = min;
    tm.tm_sec = 0;
    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}

}

using ores::iam::domain::role;
using namespace ores::iam::domain::roles;
using namespace ores::iam::domain::permissions;
using namespace ores::telemetry::log;

TEST_CASE("create_role_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = admin;
    sut.description = "Administrator role with full system access";
    sut.recorded_by = "system";
    sut.recorded_at = make_timepoint(2025, 1, 1);
    sut.permission_codes = {all};
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.name == "Admin");
    CHECK(!sut.description.empty());
    CHECK(sut.recorded_by == "system");
    CHECK(sut.permission_codes.size() == 1);
    CHECK(sut.permission_codes[0] == "*");
}

TEST_CASE("create_trading_role", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = trading;
    sut.description = "Trading role with currency management permissions";
    sut.recorded_by = "admin";
    sut.recorded_at = make_timepoint(2025, 1, 15, 10, 30);
    sut.permission_codes = {
        currencies_create, currencies_read, currencies_update, currencies_delete
    };
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name == "Trading");
    CHECK(sut.permission_codes.size() == 4);
}

TEST_CASE("create_sales_role", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = sales;
    sut.description = "Sales role with read-only access";
    sut.recorded_by = "admin";
    sut.recorded_at = make_timepoint(2025, 1, 15, 11);
    sut.permission_codes = {currencies_read, accounts_read};
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name == "Sales");
    CHECK(sut.permission_codes.size() == 2);
}

TEST_CASE("create_operations_role", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = operations;
    sut.description = "Operations role with system monitoring capabilities";
    sut.recorded_by = "admin";
    sut.recorded_at = make_timepoint(2025, 1, 15, 11, 30);
    sut.permission_codes = {accounts_read, flags_read, login_info_read};
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name == "Operations");
    CHECK(sut.permission_codes.size() == 3);
}

TEST_CASE("create_support_role", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = support;
    sut.description = "Support role with account management capabilities";
    sut.recorded_by = "admin";
    sut.recorded_at = make_timepoint(2025, 1, 15, 12);
    sut.permission_codes = {
        accounts_read, accounts_unlock, accounts_reset_password, login_info_read
    };
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name == "Support");
    CHECK(sut.permission_codes.size() == 4);
}

TEST_CASE("role_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440000");

    role sut;
    sut.version = 2;
    sut.id = specific_id;
    sut.name = "CustomRole";
    sut.description = "A custom role for testing";
    sut.recorded_by = "tester";
    sut.recorded_at = make_timepoint(2025, 2, 1, 9);
    sut.permission_codes = {currencies_read};
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.version == 2);
    CHECK(sut.name == "CustomRole");
}

TEST_CASE("role_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = "TestRole";
    sut.description = "Role for serialization testing";
    sut.recorded_by = "serializer";
    sut.recorded_at = make_timepoint(2025, 1, 20, 14);
    sut.permission_codes = {accounts_read, currencies_read};
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("TestRole") != std::string::npos);
    CHECK(json_output.find("serialization testing") != std::string::npos);
}

TEST_CASE("role_with_empty_permissions", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = "EmptyRole";
    sut.description = "Role with no permissions";
    sut.recorded_by = "admin";
    sut.recorded_at = make_timepoint(2025, 1, 25, 8);
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name == "EmptyRole");
    CHECK(sut.permission_codes.empty());
}

TEST_CASE("create_role_with_faker", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = faker::number::integer(1, 10);
    sut.id = boost::uuids::random_generator()();
    sut.name = std::string(faker::word::noun());
    sut.description = std::string(faker::lorem::sentence());
    sut.recorded_by = std::string(faker::internet::username());
    sut.recorded_at = make_timepoint(2025, 1, faker::number::integer(1, 28));

    const std::vector<std::string> available_permissions = {
        accounts_read, currencies_read, flags_read
    };
    const int num_permissions = faker::number::integer(1, 3);
    for (int i = 0; i < num_permissions; ++i) {
        sut.permission_codes.push_back(
            available_permissions[faker::number::integer(0, 2)]);
    }
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.permission_codes.empty());
}

TEST_CASE("create_multiple_random_roles", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> role_names = {admin, trading, sales, operations, support};

    for (int i = 0; i < 3; ++i) {
        role sut;
        sut.version = faker::number::integer(1, 100);
        sut.id = boost::uuids::random_generator()();
        sut.name = role_names[faker::number::integer(0, 4)];
        sut.description = std::string(faker::lorem::sentence());
        sut.recorded_by = std::string(faker::person::firstName()) + " " +
            std::string(faker::person::lastName());
        sut.recorded_at = make_timepoint(2025, 1, 15, 12);
        sut.permission_codes = {accounts_read};
        BOOST_LOG_SEV(lg, info) << "Role " << i << ":" << sut;

        CHECK(sut.version >= 1);
        CHECK(!sut.name.empty());
        CHECK(!sut.description.empty());
    }
}

TEST_CASE("well_known_role_names_are_consistent", tags) {
    auto lg(make_logger(test_suite));

    CHECK(std::string(admin) == "Admin");
    CHECK(std::string(trading) == "Trading");
    CHECK(std::string(sales) == "Sales");
    CHECK(std::string(operations) == "Operations");
    CHECK(std::string(support) == "Support");

    BOOST_LOG_SEV(lg, info) << "All well-known role names validated";
}

TEST_CASE("role_version_defaults_to_zero", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    BOOST_LOG_SEV(lg, info) << "Default role version: " << sut.version;

    CHECK(sut.version == 0);
}
