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
#include "ores.utility/faker/totp.hpp"

#include <set>
#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[faker]");

}

using ores::utility::faker::totp;
using namespace ores::telemetry::log;

TEST_CASE("totp_secret_default_size", tags) {
    auto lg(make_logger(test_suite));

    auto secret = totp::totp_secret();

    BOOST_LOG_SEV(lg, info) << "Generated TOTP secret: " << secret;

    // Default is 20 bytes, which encodes to 32 Base32 characters
    CHECK(!secret.empty());
    CHECK(secret.length() == 32);
}

TEST_CASE("totp_secret_custom_size_10", tags) {
    auto lg(make_logger(test_suite));

    auto secret = totp::totp_secret(10);

    BOOST_LOG_SEV(lg, info) << "Generated 10-byte TOTP secret: " << secret;

    // 10 bytes encodes to 16 Base32 characters
    CHECK(secret.length() == 16);
}

TEST_CASE("totp_secret_custom_size_5", tags) {
    auto lg(make_logger(test_suite));

    auto secret = totp::totp_secret(5);

    BOOST_LOG_SEV(lg, info) << "Generated 5-byte TOTP secret: " << secret;

    // 5 bytes encodes to 8 Base32 characters
    CHECK(secret.length() == 8);
}

TEST_CASE("totp_secret_custom_size_32", tags) {
    auto lg(make_logger(test_suite));

    auto secret = totp::totp_secret(32);

    BOOST_LOG_SEV(lg, info) << "Generated 32-byte TOTP secret: " << secret;

    // 32 bytes encodes to approximately 52 Base32 characters (32 * 8 / 5 = 51.2, rounded up)
    CHECK(secret.length() >= 51);
    CHECK(secret.length() <= 52);
}

TEST_CASE("totp_secret_is_valid_base32", tags) {
    auto lg(make_logger(test_suite));

    auto secret = totp::totp_secret();

    BOOST_LOG_SEV(lg, info) << "Validating Base32 format: " << secret;

    // Base32 alphabet: A-Z and 2-7
    const std::string valid_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
    for (char c : secret) {
        CHECK(valid_chars.find(c) != std::string::npos);
    }
}

TEST_CASE("totp_secret_generates_unique_values", tags) {
    auto lg(make_logger(test_suite));

    std::set<std::string> secrets;
    const size_t count = 100;

    for (size_t i = 0; i < count; ++i) {
        secrets.insert(totp::totp_secret());
    }

    BOOST_LOG_SEV(lg, info) << "Generated " << count << " secrets, "
                            << secrets.size() << " unique";

    // All secrets should be unique
    CHECK(secrets.size() == count);
}

TEST_CASE("totp_secret_different_sizes_produce_different_lengths", tags) {
    auto lg(make_logger(test_suite));

    auto secret5 = totp::totp_secret(5);
    auto secret10 = totp::totp_secret(10);
    auto secret20 = totp::totp_secret(20);
    auto secret30 = totp::totp_secret(30);

    BOOST_LOG_SEV(lg, info) << "5 bytes: " << secret5 << " (len: " << secret5.length() << ")";
    BOOST_LOG_SEV(lg, info) << "10 bytes: " << secret10 << " (len: " << secret10.length() << ")";
    BOOST_LOG_SEV(lg, info) << "20 bytes: " << secret20 << " (len: " << secret20.length() << ")";
    BOOST_LOG_SEV(lg, info) << "30 bytes: " << secret30 << " (len: " << secret30.length() << ")";

    // Each size should produce different length output
    CHECK(secret5.length() < secret10.length());
    CHECK(secret10.length() < secret20.length());
    CHECK(secret20.length() < secret30.length());
}

TEST_CASE("totp_secret_standard_size_16", tags) {
    auto lg(make_logger(test_suite));

    // 16 bytes (128 bits) is another common TOTP secret size
    auto secret = totp::totp_secret(16);

    BOOST_LOG_SEV(lg, info) << "Generated 16-byte TOTP secret: " << secret;

    // 16 bytes should encode to approximately 26 Base32 characters
    CHECK(secret.length() >= 25);
    CHECK(secret.length() <= 26);
}

TEST_CASE("totp_secret_very_small_size", tags) {
    auto lg(make_logger(test_suite));

    auto secret = totp::totp_secret(1);

    BOOST_LOG_SEV(lg, info) << "Generated 1-byte TOTP secret: " << secret;

    // 1 byte should encode to 2 Base32 characters
    CHECK(secret.length() == 2);
}

TEST_CASE("totp_secret_multiple_calls_different_results", tags) {
    auto lg(make_logger(test_suite));

    auto secret1 = totp::totp_secret();
    auto secret2 = totp::totp_secret();
    auto secret3 = totp::totp_secret();

    BOOST_LOG_SEV(lg, info) << "Secret 1: " << secret1;
    BOOST_LOG_SEV(lg, info) << "Secret 2: " << secret2;
    BOOST_LOG_SEV(lg, info) << "Secret 3: " << secret3;

    // Each call should produce different results
    CHECK(secret1 != secret2);
    CHECK(secret2 != secret3);
    CHECK(secret1 != secret3);
}
