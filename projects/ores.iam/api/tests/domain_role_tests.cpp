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
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.api/domain/role_codes.hpp"
#include "ores.iam.api/domain/role_json_io.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/faker/datetime.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <sstream>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[domain]");

using ores::utility::faker::datetime;

}

using ores::iam::domain::role;
using namespace ores::iam::domain::roles;
using namespace ores::logging;

TEST_CASE("create_role_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = super_admin;
    sut.description = "SuperAdmin role with full system access";
    sut.modified_by = "system";
    sut.recorded_at = datetime::make_timepoint(2025, 1, 1);
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.name == "SuperAdmin");
    CHECK(!sut.description.empty());
    CHECK(sut.modified_by == "system");
}

TEST_CASE("create_trading_role", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = trading;
    sut.description = "Trading role with currency management permissions";
    sut.modified_by = "admin";
    sut.recorded_at = datetime::make_timepoint(2025, 1, 15, 10, 30);
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name == "Trading");
}

TEST_CASE("create_sales_role", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = sales;
    sut.description = "Sales role with read-only access";
    sut.modified_by = "admin";
    sut.recorded_at = datetime::make_timepoint(2025, 1, 15, 11);
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name == "Sales");
}

TEST_CASE("create_operations_role", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = operations;
    sut.description = "Operations role with system monitoring capabilities";
    sut.modified_by = "admin";
    sut.recorded_at = datetime::make_timepoint(2025, 1, 15, 11, 30);
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name == "Operations");
}

TEST_CASE("create_support_role", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = support;
    sut.description = "Support role with account management capabilities";
    sut.modified_by = "admin";
    sut.recorded_at = datetime::make_timepoint(2025, 1, 15, 12);
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name == "Support");
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
    sut.modified_by = "tester";
    sut.recorded_at = datetime::make_timepoint(2025, 2, 1, 9);
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
    sut.modified_by = "serializer";
    sut.recorded_at = datetime::make_timepoint(2025, 1, 20, 14);
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("TestRole") != std::string::npos);
    CHECK(json_output.find("serialization testing") != std::string::npos);
}

TEST_CASE("role_fields_default_to_empty", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.name.empty());
    CHECK(sut.description.empty());
    CHECK(sut.modified_by.empty());
    CHECK(sut.change_reason_code.empty());
    CHECK(sut.change_commentary.empty());
}

TEST_CASE("create_role_with_faker", tags) {
    auto lg(make_logger(test_suite));

    role sut;
    sut.version = faker::number::integer(1, 10);
    sut.id = boost::uuids::random_generator()();
    sut.name = std::string(faker::word::noun());
    sut.description = std::string(faker::lorem::sentence());
    sut.modified_by = std::string(faker::internet::username());
    sut.recorded_at = datetime::make_timepoint(2025, 1, faker::number::integer(1, 28));
    BOOST_LOG_SEV(lg, info) << "Role: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
}

TEST_CASE("create_multiple_random_roles", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> role_names = {super_admin, trading, sales, operations, support};

    for (int i = 0; i < 3; ++i) {
        role sut;
        sut.version = faker::number::integer(1, 100);
        sut.id = boost::uuids::random_generator()();
        sut.name = role_names[faker::number::integer(0, 4)];
        sut.description = std::string(faker::lorem::sentence());
        sut.modified_by =
            std::string(faker::person::firstName()) + " " + std::string(faker::person::lastName());
        sut.recorded_at = datetime::make_timepoint(2025, 1, 15, 12);
        BOOST_LOG_SEV(lg, info) << "Role " << i << ":" << sut;

        CHECK(sut.version >= 1);
        CHECK(!sut.name.empty());
        CHECK(!sut.description.empty());
    }
}

TEST_CASE("well_known_role_names_are_consistent", tags) {
    auto lg(make_logger(test_suite));

    CHECK(std::string(super_admin) == "SuperAdmin");
    CHECK(std::string(tenant_admin) == "TenantAdmin");
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
