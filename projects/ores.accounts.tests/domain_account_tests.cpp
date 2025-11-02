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
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include "ores.accounts/domain/account.hpp"

namespace {

std::string test_suite("ores.accounts.tests");

}

using ores::accounts::domain::account;

TEST_CASE("create_account_with_valid_fields", "[domain_account_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    account acc;
    acc.version = 1;
    acc.modified_by = "admin";
    acc.id = boost::uuids::random_generator()();
    acc.username = "john.doe";
    acc.password_hash = "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8";
    acc.password_salt = "randomly_generated_salt_value";
    acc.totp_secret = "JBSWY3DPEHPK3PXP";
    acc.email = "john.doe@example.com";
    acc.is_admin = false;
    BOOST_LOG_SEV(lg, info) << "Account: " << acc;

    CHECK(acc.version == 1);
    CHECK(acc.modified_by == "admin");
    CHECK(acc.username == "john.doe");
    CHECK(acc.password_hash == "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8");
    CHECK(acc.password_salt == "randomly_generated_salt_value");
    CHECK(acc.totp_secret == "JBSWY3DPEHPK3PXP");
    CHECK(acc.email == "john.doe@example.com");
    CHECK(acc.is_admin == false);
}

TEST_CASE("create_admin_account", "[domain_account_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    account admin_acc;
    admin_acc.version = 1;
    admin_acc.modified_by = "system";
    admin_acc.id = boost::uuids::random_generator()();
    admin_acc.username = "admin";
    admin_acc.password_hash = "admin_hash_value";
    admin_acc.password_salt = "admin_salt_value";
    admin_acc.totp_secret = "ADMIN_TOTP_SECRET";
    admin_acc.email = "admin@example.com";
    admin_acc.is_admin = true;
    BOOST_LOG_SEV(lg, info) << "Account: " << admin_acc;

    CHECK(admin_acc.version == 1);
    CHECK(admin_acc.modified_by == "system");
    CHECK(admin_acc.username == "admin");
    CHECK(admin_acc.is_admin == true);
    CHECK(admin_acc.email == "admin@example.com");
}

TEST_CASE("account_with_specific_uuid", "[domain_account_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440000");

    account acc;
    acc.version = 2;
    acc.modified_by = "updater";
    acc.id = specific_id;
    acc.username = "test.user";
    acc.password_hash = "test_hash";
    acc.password_salt = "test_salt";
    acc.totp_secret = "";
    acc.email = "test@example.com";
    acc.is_admin = false;
    BOOST_LOG_SEV(lg, info) << "Account: " << acc;

    CHECK(acc.version == 2);
    CHECK(acc.username == "test.user");
}

TEST_CASE("account_serialization_to_json", "[domain_account_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    account acc;
    acc.version = 3;
    acc.modified_by = "developer";
    acc.id = boost::uuids::random_generator()();
    acc.username = "serialization.test";
    acc.password_hash = "hash123";
    acc.password_salt = "salt456";
    acc.totp_secret = "TOTP789";
    acc.email = "serialize@test.com";
    acc.is_admin = true;
    BOOST_LOG_SEV(lg, info) << "Account: " << acc;

    std::ostringstream oss;
    oss << acc;
    const std::string json_output = oss.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("serialization.test") != std::string::npos);
    CHECK(json_output.find("serialize@test.com") != std::string::npos);
}

TEST_CASE("create_account_with_faker", "[domain_account_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    account acc;
    acc.version = faker::number::integer(1, 10);
    acc.modified_by = std::string(faker::internet::username());
    acc.id = boost::uuids::random_generator()();
    acc.username = std::string(faker::internet::username());
    acc.password_hash = faker::number::hexadecimal(64);
    acc.password_salt = faker::number::hexadecimal(32);
    acc.totp_secret = faker::string::alphanumeric(16);
    acc.email = std::string(faker::internet::email());
    acc.is_admin = faker::datatype::boolean();

    BOOST_LOG_SEV(lg, info) << "Account: " << acc;

    CHECK(acc.version >= 1);
    CHECK(acc.version <= 10);
    CHECK(!acc.modified_by.empty());
    CHECK(!acc.username.empty());
    CHECK(acc.password_hash.length() == 66);
    CHECK(acc.password_salt.length() == 34);
    CHECK(acc.totp_secret.length() == 16);
    CHECK(!acc.email.empty());
}

TEST_CASE("create_multiple_random_accounts", "[domain_account_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        account acc;
        acc.version = faker::number::integer(1, 100);
        acc.modified_by = std::string(faker::person::firstName()) + " " +
            std::string(faker::person::lastName());
        acc.id = boost::uuids::random_generator()();
        acc.username = std::string(faker::internet::username());
        acc.password_hash = std::string(faker::crypto::sha256());
        acc.password_salt = std::string(faker::crypto::sha256());
        acc.totp_secret = faker::string::alphanumeric(20);
        acc.email = std::string(faker::internet::email());
        acc.is_admin = faker::datatype::boolean();
        BOOST_LOG_SEV(lg, info) << "Account " << i << ":" <<  acc;

        CHECK(acc.version >= 1);
        CHECK(!acc.username.empty());
        CHECK(!acc.email.empty());
    }
}
