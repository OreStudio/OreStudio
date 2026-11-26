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
#include "ores.security/crypto/secret_sealer.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using ores::security::crypto::secret_sealer;
using ores::utility::convert::base64_converter;

/// The row a value belongs to, as the vault states it when it seals one.
const std::string row = "tenant|name|ores.http|1";

/**
 * A key of the length the sealer takes. The bytes are a counter, so a key and
 * the key one bit away from it are a single edit apart in the test.
 */
std::vector<unsigned char> make_key(unsigned char seed = 0) {
    std::vector<unsigned char> key(secret_sealer::KEY_LEN);
    for (std::size_t i = 0; i < key.size(); ++i) {
        key[i] = static_cast<unsigned char>(seed + i);
    }
    return key;
}

/**
 * Changes one byte of a sealed secret, so that the test tampers with the
 * value the sealer produced rather than with a value the test built.
 */
std::string flip_byte(const std::string& sealed, std::size_t at) {
    auto raw = base64_converter::convert(sealed);
    raw[at] ^= 0x01;
    return base64_converter::convert(raw);
}

}

TEST_CASE("a_sealed_secret_opens_again", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const secret_sealer sealer(make_key());
    const std::string secret = "the-provider-client-secret";
    ORES_LOG_SEV(lg, info) << "Secret length: " << secret.size();

    const std::string sealed = sealer.seal(secret, row);
    ORES_LOG_SEV(lg, info) << "Sealed: " << sealed;

    REQUIRE_FALSE(sealed.empty());
    CHECK(sealed != secret);
    CHECK(sealer.open(sealed, row) == secret);
}

TEST_CASE("a_sealed_secret_is_never_the_same_twice", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const secret_sealer sealer(make_key());
    const std::string secret = "the-provider-client-secret";

    const std::string first = sealer.seal(secret, row);
    const std::string second = sealer.seal(secret, row);

    CHECK(first != second);
    CHECK(sealer.open(first, row) == secret);
    CHECK(sealer.open(second, row) == secret);
}

TEST_CASE("a_changed_ciphertext_does_not_open", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const secret_sealer sealer(make_key());
    const std::string sealed = sealer.seal("the-provider-client-secret", row);

    const std::string tampered = flip_byte(sealed, 0);

    CHECK(tampered != sealed);
    CHECK_THROWS_AS(sealer.open(tampered, row), std::runtime_error);
}

TEST_CASE("a_changed_key_does_not_open", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const secret_sealer sealer(make_key());
    const secret_sealer other(make_key(1));
    const std::string sealed = sealer.seal("the-provider-client-secret", row);

    CHECK_THROWS_AS(other.open(sealed, row), std::runtime_error);
}

TEST_CASE("a_secret_opened_under_another_row_does_not_open", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const secret_sealer sealer(make_key());
    const std::string sealed = sealer.seal("the-provider-client-secret", row);

    // The value is genuine and the key is right; only the row it is claimed to
    // belong to has changed, which is what a copy moved into another table row
    // looks like.
    CHECK_THROWS_AS(sealer.open(sealed, "tenant|other_name|ores.http|1"), std::runtime_error);
    CHECK_THROWS_AS(sealer.open(sealed, "other_tenant|name|ores.http|1"), std::runtime_error);
    CHECK_THROWS_AS(sealer.open(sealed, "tenant|name|ores.http|2"), std::runtime_error);
}

TEST_CASE("a_truncated_sealed_secret_does_not_open", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    const secret_sealer sealer(make_key());
    const std::string sealed = sealer.seal("the-provider-client-secret", row);

    const auto half = sealed.substr(0, sealed.size() / 2);

    CHECK_THROWS(sealer.open(half, row));
}

TEST_CASE("a_key_of_the_wrong_length_is_refused", "[crypto]") {
    auto lg(ores::logging::make_logger(test_suite));

    std::vector<unsigned char> short_key(secret_sealer::KEY_LEN - 1, 0x42);

    CHECK_THROWS_AS(secret_sealer(short_key), std::invalid_argument);
}
