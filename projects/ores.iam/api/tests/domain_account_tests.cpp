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
#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.api/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/domain/account_table.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <sstream>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[domain]");

}

using ores::iam::domain::account;
using namespace ores::logging;

TEST_CASE("create_account_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 1;
    sut.modified_by = "admin";
    sut.id = boost::uuids::random_generator()();
    sut.username = "john.doe";
    sut.email = "john.doe@example.com";
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "admin");
    CHECK(sut.username == "john.doe");
    CHECK(sut.email == "john.doe@example.com");
}

TEST_CASE("default_constructed_account_has_no_image_id", tags) {
    auto lg(make_logger(test_suite));

    const account sut;
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(!sut.image_id.has_value());
}

TEST_CASE("account_image_id_can_be_set", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.image_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.image_id.has_value());
}

TEST_CASE("create_admin_account", tags) {
    auto lg(make_logger(test_suite));

    // Note: Admin privileges are now managed via RBAC roles
    account sut;
    sut.version = 1;
    sut.modified_by = "system";
    sut.id = boost::uuids::random_generator()();
    sut.username = "admin";
    sut.email = "admin@example.com";
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "system");
    CHECK(sut.username == "admin");
    CHECK(sut.email == "admin@example.com");
}

TEST_CASE("account_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440000");

    account sut;
    sut.version = 2;
    sut.modified_by = "updater";
    sut.id = specific_id;
    sut.username = "test.user";
    sut.email = "test@example.com";
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version == 2);
    CHECK(sut.username == "test.user");
}

TEST_CASE("account_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 3;
    sut.modified_by = "developer";
    sut.id = boost::uuids::random_generator()();
    sut.username = "serialization.test";
    sut.email = "serialize@test.com";
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("serialization.test") != std::string::npos);
    CHECK(json_output.find("serialize@test.com") != std::string::npos);
}

TEST_CASE("create_account_with_faker", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = faker::number::integer(1, 10);
    sut.modified_by = std::string(faker::internet::username());
    sut.id = boost::uuids::random_generator()();
    sut.username = std::string(faker::internet::username());
    sut.email = std::string(faker::internet::email());

    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.username.empty());
    CHECK(!sut.email.empty());
}

TEST_CASE("create_multiple_random_accounts", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        account sut;
        sut.version = faker::number::integer(1, 100);
        sut.modified_by =
            std::string(faker::person::firstName()) + " " + std::string(faker::person::lastName());
        sut.id = boost::uuids::random_generator()();
        sut.username = std::string(faker::internet::username());
        sut.email = std::string(faker::internet::email());
        BOOST_LOG_SEV(lg, info) << "Account " << i << ":" << sut;

        CHECK(sut.version >= 1);
        CHECK(!sut.username.empty());
        CHECK(!sut.email.empty());
    }
}

TEST_CASE("account_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    account acc;
    acc.version = 1;
    acc.modified_by = "admin";
    acc.id = boost::uuids::random_generator()();
    acc.username = "john.doe";
    acc.email = "john.doe@example.com";

    std::vector<account> accounts = {acc};
    auto table = convert_to_table(accounts);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("john.doe") != std::string::npos);
    CHECK(table.find("john.doe@example.com") != std::string::npos);
}

TEST_CASE("account_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<account> accounts;
    for (int i = 0; i < 3; ++i) {
        account acc;
        acc.version = i + 1;
        acc.modified_by = "system";
        acc.id = boost::uuids::random_generator()();
        acc.username = "user" + std::to_string(i);
        acc.email = "user" + std::to_string(i) + "@example.com";
        accounts.push_back(acc);
    }

    auto table = convert_to_table(accounts);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("user0") != std::string::npos);
    CHECK(table.find("user1") != std::string::npos);
    CHECK(table.find("user2") != std::string::npos);
    CHECK(table.find("user0@example.com") != std::string::npos);
    CHECK(table.find("user1@example.com") != std::string::npos);
    CHECK(table.find("user2@example.com") != std::string::npos);
}

TEST_CASE("account_convert_single_to_json", tags) {
    auto lg(make_logger(test_suite));

    account acc;
    acc.version = 1;
    acc.modified_by = "admin";
    acc.id = boost::uuids::random_generator()();
    acc.username = "john.doe";
    acc.email = "john.doe@example.com";

    std::ostringstream os;
    os << acc;
    const auto json = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON: " << json;

    CHECK(!json.empty());
    CHECK(json.find("john.doe") != std::string::npos);
    CHECK(json.find("john.doe@example.com") != std::string::npos);
}

TEST_CASE("account_convert_multiple_to_json", tags) {
    auto lg(make_logger(test_suite));

    std::vector<account> accounts;
    for (int i = 0; i < 3; ++i) {
        account acc;
        acc.version = i + 1;
        acc.modified_by = "system";
        acc.id = boost::uuids::random_generator()();
        acc.username = "user" + std::to_string(i);
        acc.email = "user" + std::to_string(i) + "@example.com";
        accounts.push_back(acc);
    }

    for (const auto& acc : accounts) {
        std::ostringstream os;
        os << acc;
        const auto json = os.str();

        BOOST_LOG_SEV(lg, info) << "JSON: " << json;

        CHECK(!json.empty());
        CHECK(json.find(acc.username) != std::string::npos);
        CHECK(json.find(acc.email) != std::string::npos);
    }
}

TEST_CASE("account_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<account> accounts;
    auto table = convert_to_table(accounts);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    // Even with no rows the table still has headers.
    CHECK(!table.empty());
}

TEST_CASE("account_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<account> accounts;
    for (int i = 0; i < 5; ++i) {
        account acc;
        acc.version = faker::number::integer(1, 10);
        acc.modified_by = std::string(faker::internet::username());
        acc.id = boost::uuids::random_generator()();
        acc.username = std::string(faker::internet::username());
        acc.email = std::string(faker::internet::email());
        accounts.push_back(acc);
    }

    auto table = convert_to_table(accounts);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    // Verify all usernames appear in table
    for (const auto& acc : accounts) {
        CHECK(table.find(acc.username) != std::string::npos);
    }
}
