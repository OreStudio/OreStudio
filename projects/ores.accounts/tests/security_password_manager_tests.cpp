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
#include "ores.accounts/security/password_manager.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.accounts.tests");
const std::string tags("[security]");

}

using ores::accounts::security::password_manager;
using namespace ores::utility::log;

TEST_CASE("verify_password_hash_with_correct_password", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "correct_password";
    BOOST_LOG_SEV(lg, info) << "Password: " << password;

    const std::string hash = password_manager::create_password_hash(password);
    BOOST_LOG_SEV(lg, info) << "Hash: " << hash;

    CHECK(!hash.empty());
    CHECK(password_manager::verify_password_hash(password, hash));
}

TEST_CASE("verify_password_hash_with_incorrect_password", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "correct_password";
    BOOST_LOG_SEV(lg, info) << "Password: " << password;

    const std::string wrong_password = "wrong_password";
    BOOST_LOG_SEV(lg, info) << "Wrong password: " << wrong_password;

    const std::string hash = password_manager::create_password_hash(password);
    BOOST_LOG_SEV(lg, info) << "Hash: " << hash;

    CHECK(!hash.empty());
    CHECK(!password_manager::verify_password_hash(wrong_password, hash));
}

TEST_CASE("hash_is_not_deterministic", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "a_simple_password";
    BOOST_LOG_SEV(lg, info) << "Password: " << password;

    const std::string hash1 = password_manager::create_password_hash(password);
    BOOST_LOG_SEV(lg, info) << "Hash 1: " << hash1;

    const std::string hash2 = password_manager::create_password_hash(password);
    BOOST_LOG_SEV(lg, info) << "Hash 2: " << hash2;

    CHECK(!hash1.empty());
    CHECK(!hash2.empty());
    CHECK(hash1 != hash2);
}

TEST_CASE("invalid_hash_format_fails_verification", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "any_password";
    BOOST_LOG_SEV(lg, info) << "Password: " << password;

    const std::string malformed_hash = "this_is_not_a_valid_hash_format";
    BOOST_LOG_SEV(lg, info) << "Malformed hash: " << malformed_hash;

    CHECK(!password_manager::verify_password_hash(password, malformed_hash));
}

TEST_CASE("empty_password_throws", tags) {
    auto lg(make_logger(test_suite));

    const std::string empty_password = "";
    CHECK_THROWS_AS(password_manager::create_password_hash(empty_password),
        std::invalid_argument);
}
