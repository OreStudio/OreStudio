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
#include "ores.connections/service/encryption_service.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.connections.tests");
const std::string tags("[service]");

}

using namespace ores::connections::service;
using namespace ores::logging;

TEST_CASE("encrypt_and_decrypt_simple_password", tags) {
    auto lg(make_logger(test_suite));

    const std::string plaintext = "my_secret_password";
    const std::string master_password = "master123";

    const auto encrypted = encryption_service::encrypt(plaintext, master_password);
    BOOST_LOG_SEV(lg, debug) << "Encrypted: " << encrypted;

    CHECK(!encrypted.empty());
    CHECK(encrypted != plaintext);

    const auto decrypted = encryption_service::decrypt(encrypted, master_password);
    BOOST_LOG_SEV(lg, debug) << "Decrypted: " << decrypted;

    CHECK(decrypted == plaintext);
}

TEST_CASE("encrypt_produces_different_output_each_time", tags) {
    auto lg(make_logger(test_suite));

    const std::string plaintext = "same_password";
    const std::string master_password = "master123";

    const auto encrypted1 = encryption_service::encrypt(plaintext, master_password);
    const auto encrypted2 = encryption_service::encrypt(plaintext, master_password);

    BOOST_LOG_SEV(lg, debug) << "Encrypted 1: " << encrypted1;
    BOOST_LOG_SEV(lg, debug) << "Encrypted 2: " << encrypted2;

    // Same plaintext should produce different ciphertext due to random salt/IV
    CHECK(encrypted1 != encrypted2);

    // But both should decrypt to the same plaintext
    CHECK(encryption_service::decrypt(encrypted1, master_password) == plaintext);
    CHECK(encryption_service::decrypt(encrypted2, master_password) == plaintext);
}

TEST_CASE("decrypt_with_wrong_password_throws", tags) {
    auto lg(make_logger(test_suite));

    const std::string plaintext = "secret";
    const std::string correct_password = "correct";
    const std::string wrong_password = "wrong";

    const auto encrypted = encryption_service::encrypt(plaintext, correct_password);
    BOOST_LOG_SEV(lg, debug) << "Encrypted: " << encrypted;

    CHECK_THROWS_AS(
        encryption_service::decrypt(encrypted, wrong_password),
        std::runtime_error);
}

TEST_CASE("verify_password_with_correct_password", tags) {
    auto lg(make_logger(test_suite));

    const std::string plaintext = "test_data";
    const std::string master_password = "correct_password";

    const auto encrypted = encryption_service::encrypt(plaintext, master_password);
    BOOST_LOG_SEV(lg, debug) << "Encrypted: " << encrypted;

    CHECK(encryption_service::verify_password(encrypted, master_password));
}

TEST_CASE("verify_password_with_wrong_password", tags) {
    auto lg(make_logger(test_suite));

    const std::string plaintext = "test_data";
    const std::string correct_password = "correct";
    const std::string wrong_password = "wrong";

    const auto encrypted = encryption_service::encrypt(plaintext, correct_password);
    BOOST_LOG_SEV(lg, debug) << "Encrypted: " << encrypted;

    CHECK_FALSE(encryption_service::verify_password(encrypted, wrong_password));
}

TEST_CASE("encrypt_and_decrypt_empty_string", tags) {
    auto lg(make_logger(test_suite));

    const std::string plaintext = "";
    const std::string master_password = "master";

    const auto encrypted = encryption_service::encrypt(plaintext, master_password);
    BOOST_LOG_SEV(lg, debug) << "Encrypted empty: " << encrypted;

    // Empty plaintext returns empty encrypted string (by design)
    CHECK(encrypted.empty());

    const auto decrypted = encryption_service::decrypt(encrypted, master_password);
    CHECK(decrypted == plaintext);
}

TEST_CASE("encrypt_and_decrypt_unicode_password", tags) {
    auto lg(make_logger(test_suite));

    const std::string plaintext = "密码パスワード🔐";
    const std::string master_password = "主密码マスター";

    const auto encrypted = encryption_service::encrypt(plaintext, master_password);
    BOOST_LOG_SEV(lg, debug) << "Encrypted unicode: " << encrypted;

    const auto decrypted = encryption_service::decrypt(encrypted, master_password);
    BOOST_LOG_SEV(lg, debug) << "Decrypted: " << decrypted;

    CHECK(decrypted == plaintext);
}

TEST_CASE("encrypt_and_decrypt_long_password", tags) {
    auto lg(make_logger(test_suite));

    // Generate a long random password
    std::string plaintext;
    for (int i = 0; i < 100; ++i) {
        plaintext += std::string(faker::word::noun()) + "_";
    }
    const std::string master_password = "master";

    BOOST_LOG_SEV(lg, debug) << "Long password length: " << plaintext.length();

    const auto encrypted = encryption_service::encrypt(plaintext, master_password);
    const auto decrypted = encryption_service::decrypt(encrypted, master_password);

    CHECK(decrypted == plaintext);
}
