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
#include <boost/test/unit_test.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/test/logging.hpp"
#include "ores.accounts/domain/account.hpp"

namespace {

const std::string test_module("ores.accounts.tests");
const std::string test_suite("domain_account_tests");

}

using ores::accounts::domain::account;

BOOST_AUTO_TEST_SUITE(domain_account_tests)

BOOST_AUTO_TEST_CASE(create_account_with_valid_fields) {
    SETUP_TEST_LOG_SOURCE_DEBUG("create_account_with_valid_fields");

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

    BOOST_LOG_SEV(lg, debug) << "Created account: " << acc;

    BOOST_CHECK_EQUAL(acc.version, 1);
    BOOST_CHECK_EQUAL(acc.modified_by, "admin");
    BOOST_CHECK_EQUAL(acc.username, "john.doe");
    BOOST_CHECK_EQUAL(acc.password_hash, "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8");
    BOOST_CHECK_EQUAL(acc.password_salt, "randomly_generated_salt_value");
    BOOST_CHECK_EQUAL(acc.totp_secret, "JBSWY3DPEHPK3PXP");
    BOOST_CHECK_EQUAL(acc.email, "john.doe@example.com");
    BOOST_CHECK_EQUAL(acc.is_admin, false);
}

BOOST_AUTO_TEST_CASE(create_admin_account) {
    SETUP_TEST_LOG_SOURCE_DEBUG("create_admin_account");

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

    BOOST_LOG_SEV(lg, debug) << "Created admin account: " << admin_acc;

    BOOST_CHECK_EQUAL(admin_acc.version, 1);
    BOOST_CHECK_EQUAL(admin_acc.modified_by, "system");
    BOOST_CHECK_EQUAL(admin_acc.username, "admin");
    BOOST_CHECK_EQUAL(admin_acc.is_admin, true);
    BOOST_CHECK_EQUAL(admin_acc.email, "admin@example.com");
}

BOOST_AUTO_TEST_CASE(account_with_specific_uuid) {
    SETUP_TEST_LOG_SOURCE_DEBUG("account_with_specific_uuid");

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

    BOOST_LOG_SEV(lg, debug) << "Account with specific UUID: " << acc;

    BOOST_CHECK_EQUAL(acc.version, 2);
    BOOST_CHECK_EQUAL(acc.username, "test.user");
}

BOOST_AUTO_TEST_CASE(account_serialization_to_json) {
    SETUP_TEST_LOG_SOURCE_DEBUG("account_serialization_to_json");

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

    BOOST_LOG_SEV(lg, debug) << "Account before serialization: " << acc;

    std::ostringstream oss;
    oss << acc;
    const std::string json_output = oss.str();

    BOOST_LOG_SEV(lg, debug) << "Serialized JSON: " << json_output;

    BOOST_CHECK(!json_output.empty());
    BOOST_CHECK(json_output.find("serialization.test") != std::string::npos);
    BOOST_CHECK(json_output.find("serialize@test.com") != std::string::npos);
}

BOOST_AUTO_TEST_SUITE_END()
