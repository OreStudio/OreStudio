/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.logging/logger.hpp"
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using ores::security::crypto::sealed_box;
using ores::utility::convert::base64_converter;

/// Changes one byte of the decoded sealed value, so that the tests tamper with
/// what the box produced rather than with a value they built. The position is
/// counted in the decoded bytes, not in the Base64 characters.
std::string alter_sealed_byte(const std::string& sealed, std::size_t at) {
    auto raw = base64_converter::convert(sealed);
    REQUIRE(at < raw.size());
    raw[at] ^= 0x01;
    return base64_converter::convert(raw);
}

/// Changes the last byte of the ciphertext, which is the last decoded byte.
std::string alter_last_sealed_byte(const std::string& sealed) {
    auto raw = base64_converter::convert(sealed);
    REQUIRE_FALSE(raw.empty());
    raw.back() ^= 0x01;
    return base64_converter::convert(raw);
}

}

TEST_CASE("a_sealed_value_opens_with_the_recipients_private_key", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto recipient = sealed_box::generate();
    const std::string secret = "the-vault-private-key";
    ORES_LOG_SEV(lg, info) << "Recipient public key length: " << recipient.public_key.size();

    const std::string sealed = sealed_box::seal(recipient.public_key, secret);
    ORES_LOG_SEV(lg, info) << "Sealed: " << sealed;

    REQUIRE_FALSE(sealed.empty());
    CHECK(sealed != secret);
    CHECK(sealed_box::open(recipient.private_key, sealed) == secret);
}

TEST_CASE("a_sealed_value_is_never_the_same_twice", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto recipient = sealed_box::generate();
    const std::string secret = "the-vault-private-key";

    const std::string first = sealed_box::seal(recipient.public_key, secret);
    const std::string second = sealed_box::seal(recipient.public_key, secret);

    CHECK(first != second);
    CHECK(sealed_box::open(recipient.private_key, first) == secret);
    CHECK(sealed_box::open(recipient.private_key, second) == secret);
}

TEST_CASE("another_recipients_private_key_does_not_open", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto recipient = sealed_box::generate();
    const auto other = sealed_box::generate();
    const std::string sealed = sealed_box::seal(recipient.public_key, "the-vault-private-key");

    CHECK_THROWS_AS(sealed_box::open(other.private_key, sealed), std::runtime_error);
}

TEST_CASE("a_changed_sealed_value_does_not_open", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto recipient = sealed_box::generate();
    const std::string sealed = sealed_box::seal(recipient.public_key, "the-vault-private-key");

    // The last byte belongs to the ciphertext; the first belongs to the
    // sender's public key, which the derivation and the authentication bind.
    const std::string in_the_body = alter_last_sealed_byte(sealed);
    const std::string in_the_header = alter_sealed_byte(sealed, 0);

    CHECK(in_the_body != sealed);
    CHECK(in_the_header != sealed);
    CHECK_THROWS_AS(sealed_box::open(recipient.private_key, in_the_body), std::runtime_error);
    CHECK_THROWS_AS(sealed_box::open(recipient.private_key, in_the_header), std::runtime_error);
}

TEST_CASE("a_truncated_sealed_value_does_not_open", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto recipient = sealed_box::generate();
    const std::string sealed = sealed_box::seal(recipient.public_key, "the-vault-private-key");

    CHECK_THROWS(sealed_box::open(recipient.private_key, sealed.substr(0, sealed.size() / 2)));
}

TEST_CASE("an_empty_plaintext_seals_and_opens", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto recipient = sealed_box::generate();
    const std::string sealed = sealed_box::seal(recipient.public_key, "");

    REQUIRE_FALSE(sealed.empty());
    CHECK(sealed_box::open(recipient.private_key, sealed) == "");
}

TEST_CASE("the_public_key_is_derivable_from_the_private_key", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto pair = sealed_box::generate();

    REQUIRE(pair.private_key.size() == sealed_box::KEY_LEN);
    REQUIRE(pair.public_key.size() == sealed_box::KEY_LEN);
    CHECK(sealed_box::public_key_of(pair.private_key) == pair.public_key);
}

TEST_CASE("a_sealed_box_key_of_the_wrong_length_is_refused", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const auto pair = sealed_box::generate();
    const std::vector<unsigned char> short_key(sealed_box::KEY_LEN - 1, 0x42);
    const std::string sealed = sealed_box::seal(pair.public_key, "the-vault-private-key");

    CHECK_THROWS_AS(sealed_box::seal(short_key, "the-vault-private-key"), std::invalid_argument);
    CHECK_THROWS_AS(sealed_box::open(short_key, sealed), std::invalid_argument);
    CHECK_THROWS_AS(sealed_box::public_key_of(short_key), std::invalid_argument);
}
