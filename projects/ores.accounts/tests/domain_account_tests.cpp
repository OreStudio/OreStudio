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
#include "ores.accounts/domain/account.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/log/make_logger.hpp"
#include <faker-cxx/faker.h> // IWYU pragma: keep.

namespace {

const std::string test_suite("ores.accounts.tests");
const std::string tags("[domain_account_tests]");

}

using ores::accounts::domain::account;
using namespace ores::utility::log;

TEST_CASE("create_account_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 1;
    sut.modified_by = "admin";
    sut.id = boost::uuids::random_generator()();
    sut.username = "john.doe";
    sut.password_hash = "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8";
    sut.password_salt = "randomly_generated_salt_value";
    sut.totp_secret = "JBSWY3DPEHPK3PXP";
    sut.email = "john.doe@example.com";
    sut.is_admin = false;
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "admin");
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
    sut.modified_by = "system";
    sut.id = boost::uuids::random_generator()();
    sut.username = "admin";
    sut.password_hash = "admin_hash_value";
    sut.password_salt = "admin_salt_value";
    sut.totp_secret = "ADMIN_TOTP_SECRET";
    sut.email = "admin@example.com";
    sut.is_admin = true;
    BOOST_LOG_SEV(lg, info) << "Account: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "system");
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
    sut.modified_by = "updater";
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

TEST_CASE("account_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 3;
    sut.modified_by = "developer";
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
    sut.modified_by = std::string(faker::internet::username());
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
    CHECK(!sut.modified_by.empty());
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
        sut.modified_by = std::string(faker::person::firstName()) + " " +
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
