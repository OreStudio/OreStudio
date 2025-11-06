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
#include "ores.accounts/generators/account_generator.hpp"

#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/string_generator.hpp>

namespace {

// Minimal RFC 4648 Base32 encoder (no padding)
std::string base32_encode(const std::vector<uint8_t>& data) {
    static const std::array<char, 33> alphabet("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567");
    std::string result;
    size_t bit_index = 0;
    size_t current_byte = 0;

    for (uint8_t byte : data) {
        current_byte = (current_byte << 8) | byte;
        bit_index += 8;
        while (bit_index >= 5) {
            bit_index -= 5;
            size_t idx = (current_byte >> bit_index) & 0x1F;
            result += alphabet[idx];
        }
    }
    if (bit_index > 0) {
        result += alphabet[(current_byte << (5 - bit_index)) & 0x1F];
    }
    return result;
}

std::string generate_totp_secret(size_t num_bytes = 20) {
    std::vector<uint8_t> bytes(num_bytes);
    for (auto& b : bytes) {
        // faker::Number::integer<uint8_t>() works, but simpler:
        b = static_cast<uint8_t>(faker::number::integer(0, 255));
    }
    return base32_encode(bytes);
}

}

namespace ores::accounts::generators {


domain::account generate_fake_account() {
    domain::account r;
    r.version = 1;
    r.modified_by = faker::internet::username();

    boost::uuids::string_generator gen;
    r.id = gen(faker::string::uuidV4());

    auto first = std::string(faker::person::firstName());
    auto last  = std::string(faker::person::lastName());
    r.username = faker::internet::username(first, last);
    r.email = faker::internet::email(first, last);

    r.password_hash = faker::crypto::sha256();
    r.password_salt = faker::crypto::sha256();
    r.totp_secret = generate_totp_secret();

    r.is_admin = false;
    return r;
}

}
