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
#include "ores.logging/make_logger.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <catch2/catch_test_macros.hpp>
#include <fstream>

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[crypto]");

}

using ores::utility::crypto::sha256;
using namespace ores::logging;

TEST_CASE("sha256_hex_digest_of_empty_string", tags) {
    auto lg(make_logger(test_suite));

    const auto result = sha256::hex_digest("");
    BOOST_LOG_SEV(lg, info) << "Empty digest: " << result;

    CHECK(result ==
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
}

TEST_CASE("sha256_hex_digest_of_known_string", tags) {
    auto lg(make_logger(test_suite));

    const auto result = sha256::hex_digest("hello world");
    BOOST_LOG_SEV(lg, info) << "Digest: " << result;

    CHECK(result ==
          "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9");
}

TEST_CASE("sha256_hex_digest_of_file_matches_hex_digest", tags) {
    auto lg(make_logger(test_suite));

    const auto tmp = std::filesystem::temp_directory_path() / "ores_utility_sha256_test.txt";
    {
        std::ofstream f(tmp, std::ios::binary | std::ios::trunc);
        f << "hello world";
    }

    const auto result = sha256::hex_digest_of_file(tmp);
    BOOST_LOG_SEV(lg, info) << "File digest: " << result;

    CHECK(result == sha256::hex_digest("hello world"));

    std::filesystem::remove(tmp);
}
