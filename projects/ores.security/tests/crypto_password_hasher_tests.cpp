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
#include "ores.logging/make_logger.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.security/crypto/password_hasher.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include <catch2/catch_test_macros.hpp>
#include <openssl/evp.h>
#include <sstream>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.security.tests");
const std::string tags("[crypto]");

const std::string fast_flag("ORES_TEST_PASSWORD_FAST");

/**
 * Builds a scrypt hash string at an arbitrary cost, independently of the
 * class under test. The salt is fixed, because the point is the cost and not
 * the salt.
 */
std::string make_scrypt_hash(const std::string& password, int ln) {
    std::vector<unsigned char> salt(16, 0x42);
    std::vector<unsigned char> digest(64);

    if (EVP_PBE_scrypt(password.c_str(),
                       password.size(),
                       salt.data(),
                       salt.size(),
                       1ULL << ln,
                       8,
                       1,
                       0,
                       digest.data(),
                       digest.size()) != 1) {
        return {};
    }

    std::stringstream ss;
    ss << "$scrypt$ln=" << ln << ",r=8,p=1$"
       << ores::utility::convert::base64_converter::convert(salt) << "$"
       << ores::utility::convert::base64_converter::convert(digest);
    return ss.str();
}

}

using ores::security::crypto::password_hasher;
using namespace ores::logging;

TEST_CASE("verify_hash_with_correct_password", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "correct_password";
    BOOST_LOG_SEV(lg, info) << "Password: " << password;

    const std::string hash = password_hasher::hash(password);
    BOOST_LOG_SEV(lg, info) << "Hash: " << hash;

    CHECK(!hash.empty());
    CHECK(password_hasher::verify(password, hash));
}

TEST_CASE("verify_hash_with_incorrect_password", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "correct_password";
    BOOST_LOG_SEV(lg, info) << "Password: " << password;

    const std::string wrong_password = "wrong_password";
    BOOST_LOG_SEV(lg, info) << "Wrong password: " << wrong_password;

    const std::string hash = password_hasher::hash(password);
    BOOST_LOG_SEV(lg, info) << "Hash: " << hash;

    CHECK(!hash.empty());
    CHECK(!password_hasher::verify(wrong_password, hash));
}

TEST_CASE("hash_is_not_deterministic", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "a_simple_password";
    BOOST_LOG_SEV(lg, info) << "Password: " << password;

    const std::string hash1 = password_hasher::hash(password);
    BOOST_LOG_SEV(lg, info) << "Hash 1: " << hash1;

    const std::string hash2 = password_hasher::hash(password);
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

    CHECK(!password_hasher::verify(password, malformed_hash));
}

TEST_CASE("empty_password_throws", tags) {
    auto lg(make_logger(test_suite));

    const std::string empty_password = "";
    CHECK_THROWS_AS(password_hasher::hash(empty_password), std::invalid_argument);
}

TEST_CASE("verify_rejects_an_empty_password_or_hash", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "a_password";
    const std::string hash = password_hasher::hash(password);
    REQUIRE_FALSE(hash.empty());

    CHECK_FALSE(password_hasher::verify("", hash));
    CHECK_FALSE(password_hasher::verify(password, ""));
    CHECK_FALSE(password_hasher::verify("", ""));
}

TEST_CASE("verify_rejects_a_downgraded_scrypt_cost", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "a_password";

    // The suite runs with ORES_TEST_PASSWORD_FAST set, so the cost this
    // build produces, and therefore the floor, is ln=10.
    const auto at_the_floor = make_scrypt_hash(password, 10);
    const auto below_the_floor = make_scrypt_hash(password, 1);
    REQUIRE_FALSE(at_the_floor.empty());
    REQUIRE_FALSE(below_the_floor.empty());

    BOOST_LOG_SEV(lg, info) << "Floor hash: " << at_the_floor;

    // The control proves the hand-built hash is a real hash of the password,
    // so the case below cannot pass merely because the construction is wrong.
    CHECK(password_hasher::verify(password, at_the_floor));
    CHECK_FALSE(password_hasher::verify(password, below_the_floor));
}

TEST_CASE("stored_hash_pins_the_format_and_the_production_cost", tags) {
    auto lg(make_logger(test_suite));
    using ores::platform::environment::environment;

    const std::string password = "a_password";

    // With the fast flag absent this build produces the production cost, and
    // the stored string has to say so. Reading the flag on every call is what
    // makes this reachable after the other cases have run.
    environment::unset_value(fast_flag);
    const std::string production_hash = password_hasher::hash(password);
    environment::set_value(fast_flag, "1");

    BOOST_LOG_SEV(lg, info) << "Production hash: " << production_hash;

    CHECK(production_hash.starts_with("$scrypt$ln=14,r=8,p=1$"));
    CHECK(password_hasher::verify(password, production_hash));

    const std::string fast_hash = password_hasher::hash(password);
    CHECK(fast_hash.starts_with("$scrypt$ln=10,r=8,p=1$"));
    CHECK(password_hasher::verify(password, fast_hash));
}
