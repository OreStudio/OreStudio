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
#include "ores.iam/domain/account.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"
#include "ores.iam/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/account_json.hpp"
#include "ores.iam/domain/account_table.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[domain]");

}

using ores::iam::domain::account;
using namespace ores::utility::log;

TEST_CASE("create_account_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 1;
    sut.recorded_by = "admin";
    sut.id = boost::uuids::random_generator()();
    sut.username = "john.doe";
    sut.password_hash = "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8";
    sut.password_salt = "randomly_generated_salt_value";
    sut.totp_secret = "JBSWY3DPEHPK3PXP";
    sut.email = "john.doe@example.com";
    sut.is_admin = false;
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.recorded_by == "admin");
    CHECK(sut.username == "john.doe");
    CHECK(sut.password_hash == "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8");
    CHECK(sut.password_salt == "randomly_generated_salt_value");
    CHECK(sut.totp_secret == "JBSWY3DPEHPK3PXP");
    CHECK(sut.email == "john.doe@example.com");
    CHECK(sut.is_admin == false);
}

TEST_CASE("create_admin_account", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 1;
    sut.recorded_by = "system";
    sut.id = boost::uuids::random_generator()();
    sut.username = "admin";
    sut.password_hash = "admin_hash_value";
    sut.password_salt = "admin_salt_value";
    sut.totp_secret = "ADMIN_TOTP_SECRET";
    sut.email = "admin@example.com";
    sut.is_admin = true;
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.recorded_by == "system");
    CHECK(sut.username == "admin");
    CHECK(sut.is_admin == true);
    CHECK(sut.email == "admin@example.com");
}

TEST_CASE("account_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440000");

    account sut;
    sut.version = 2;
    sut.recorded_by = "updater";
    sut.id = specific_id;
    sut.username = "test.user";
    sut.password_hash = "test_hash";
    sut.password_salt = "test_salt";
    sut.totp_secret = "";
    sut.email = "test@example.com";
    sut.is_admin = false;
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version == 2);
    CHECK(sut.username == "test.user");
}

TEST_CASE("account_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 3;
    sut.recorded_by = "developer";
    sut.id = boost::uuids::random_generator()();
    sut.username = "serialization.test";
    sut.password_hash = "hash123";
    sut.password_salt = "salt456";
    sut.totp_secret = "TOTP789";
    sut.email = "serialize@test.com";
    sut.is_admin = true;
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
    sut.recorded_by = std::string(faker::internet::username());
    sut.id = boost::uuids::random_generator()();
    sut.username = std::string(faker::internet::username());
    sut.password_hash = faker::number::hexadecimal(64);
    sut.password_salt = faker::number::hexadecimal(32);
    sut.totp_secret = faker::string::alphanumeric(16);
    sut.email = std::string(faker::internet::email());
    sut.is_admin = faker::datatype::boolean();

    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.username.empty());
    CHECK(sut.password_hash.length() == 66);
    CHECK(sut.password_salt.length() == 34);
    CHECK(sut.totp_secret.length() == 16);
    CHECK(!sut.email.empty());
}

TEST_CASE("create_multiple_random_accounts", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        account sut;
        sut.version = faker::number::integer(1, 100);
        sut.recorded_by = std::string(faker::person::firstName()) + " " +
            std::string(faker::person::lastName());
        sut.id = boost::uuids::random_generator()();
        sut.username = std::string(faker::internet::username());
        sut.password_hash = std::string(faker::crypto::sha256());
        sut.password_salt = std::string(faker::crypto::sha256());
        sut.totp_secret = faker::string::alphanumeric(20);
        sut.email = std::string(faker::internet::email());
        sut.is_admin = faker::datatype::boolean();
        BOOST_LOG_SEV(lg, info) << "Account " << i << ":" <<  sut;

        CHECK(sut.version >= 1);
        CHECK(!sut.username.empty());
        CHECK(!sut.email.empty());
    }
}

TEST_CASE("account_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    account acc;
    acc.version = 1;
    acc.recorded_by = "admin";
    acc.id = boost::uuids::random_generator()();
    acc.username = "john.doe";
    acc.password_hash = "hash123";
    acc.password_salt = "salt456";
    acc.totp_secret = "TOTP789";
    acc.email = "john.doe@example.com";
    acc.is_admin = false;

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
        acc.recorded_by = "system";
        acc.id = boost::uuids::random_generator()();
        acc.username = "user" + std::to_string(i);
        acc.password_hash = "hash" + std::to_string(i);
        acc.password_salt = "salt" + std::to_string(i);
        acc.totp_secret = "TOTP" + std::to_string(i);
        acc.email = "user" + std::to_string(i) + "@example.com";
        acc.is_admin = (i == 0);
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
    acc.recorded_by = "admin";
    acc.id = boost::uuids::random_generator()();
    acc.username = "john.doe";
    acc.password_hash = "hash123";
    acc.password_salt = "salt456";
    acc.totp_secret = "TOTP789";
    acc.email = "john.doe@example.com";
    acc.is_admin = false;

    std::vector<account> accounts = {acc};
    auto json = convert_to_json(accounts);

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
        acc.recorded_by = "system";
        acc.id = boost::uuids::random_generator()();
        acc.username = "user" + std::to_string(i);
        acc.password_hash = "hash" + std::to_string(i);
        acc.password_salt = "salt" + std::to_string(i);
        acc.totp_secret = "TOTP" + std::to_string(i);
        acc.email = "user" + std::to_string(i) + "@example.com";
        acc.is_admin = (i == 0);
        accounts.push_back(acc);
    }

    auto json = convert_to_json(accounts);

    BOOST_LOG_SEV(lg, info) << "JSON: " << json;

    CHECK(!json.empty());
    CHECK(json.find("user0") != std::string::npos);
    CHECK(json.find("user1") != std::string::npos);
    CHECK(json.find("user2") != std::string::npos);
    CHECK(json.find("user0@example.com") != std::string::npos);
    CHECK(json.find("user1@example.com") != std::string::npos);
    CHECK(json.find("user2@example.com") != std::string::npos);
}

TEST_CASE("account_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<account> accounts;
    auto table = convert_to_table(accounts);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("account_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<account> accounts;
    for (int i = 0; i < 5; ++i) {
        account acc;
        acc.version = faker::number::integer(1, 10);
        acc.recorded_by = std::string(faker::internet::username());
        acc.id = boost::uuids::random_generator()();
        acc.username = std::string(faker::internet::username());
        acc.password_hash = faker::number::hexadecimal(64);
        acc.password_salt = faker::number::hexadecimal(32);
        acc.totp_secret = faker::string::alphanumeric(16);
        acc.email = std::string(faker::internet::email());
        acc.is_admin = faker::datatype::boolean();
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
