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
#include "ores.iam.core/service/agent_key_secrets.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <array>
#include <openssl/crypto.h>
#include <openssl/rand.h>
#include <stdexcept>

namespace ores::iam::service {

std::string generate_agent_key_secret() {
    std::array<unsigned char, 32> bytes{};
    if (RAND_bytes(bytes.data(), static_cast<int>(bytes.size())) != 1)
        throw std::runtime_error("The secure random source failed; no agent key was made.");
    static constexpr std::array<char, 16> digits = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
    std::string out;
    out.reserve(bytes.size() * 2);
    for (const auto byte : bytes) {
        out.push_back(digits[byte >> 4]);
        out.push_back(digits[byte & 0x0F]);
    }
    OPENSSL_cleanse(bytes.data(), bytes.size());
    return out;
}

std::string hash_agent_key_secret(std::string_view secret) {
    return utility::crypto::sha256::hex_digest(secret);
}

bool agent_key_secret_matches(std::string_view secret, std::string_view stored_hash) {
    const auto presented = hash_agent_key_secret(secret);
    if (presented.size() != stored_hash.size())
        return false;
    return CRYPTO_memcmp(presented.data(), stored_hash.data(), presented.size()) == 0;
}

}
