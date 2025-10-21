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
#include "ores.accounts/security/password_manager.hpp"

using ores::accounts::security::PasswordManager;

BOOST_AUTO_TEST_SUITE(security_password_manager_tests)

BOOST_AUTO_TEST_CASE(verify_password_hash) {
    const std::string password = "correct_password";
    const std::string hash = PasswordManager::createPasswordHash(password);

    BOOST_CHECK(!hash.empty());
    BOOST_CHECK(PasswordManager::verifyPasswordHash(password, hash));
}

BOOST_AUTO_TEST_CASE(verify_password_hash_with_wrong_password) {
    const std::string password = "correct_password";
    const std::string wrong_password = "wrong_password";
    const std::string hash = PasswordManager::createPasswordHash(password);

    BOOST_CHECK(!hash.empty());
    BOOST_CHECK(!PasswordManager::verifyPasswordHash(wrong_password, hash));
}

BOOST_AUTO_TEST_CASE(hash_is_not_deterministic) {
    const std::string password = "a_simple_password";
    const std::string hash1 = PasswordManager::createPasswordHash(password);
    const std::string hash2 = PasswordManager::createPasswordHash(password);

    BOOST_CHECK(!hash1.empty());
    BOOST_CHECK(!hash2.empty());
    BOOST_CHECK_NE(hash1, hash2);
}

BOOST_AUTO_TEST_CASE(invalid_hash_format_fails_verification) {
    const std::string password = "any_password";
    const std::string malformed_hash = "this_is_not_a_valid_hash_format";

    BOOST_CHECK(!PasswordManager::verifyPasswordHash(password, malformed_hash));
}

BOOST_AUTO_TEST_SUITE_END()
