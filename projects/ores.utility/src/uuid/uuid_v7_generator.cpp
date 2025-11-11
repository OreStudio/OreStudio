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
 * FOR A PARTICULAR PURPOSE. Seethe GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General PublicLicense along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include <chrono>
#include <cstdint>
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::utility::uuid {

boost::uuids::uuid uuid_v7_generator::operator()() {
    boost::uuids::uuid new_uuid;

    // Get the current time as a 48-bit Unix timestamp in milliseconds
    auto now = std::chrono::system_clock::now();
    auto timestamp_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()).count();

    // Ensure timestamp fits in 48 bits (it will until the year 10889)
    uint64_t ts_48 = static_cast<uint64_t>(timestamp_ms) & 0x0000FFFFFFFFFFFF;

    // Fill the UUID with random data first. This is simpler than generating
    // random bits for specific fields. We use a pre-seeded engine for
    // efficiency.
    boost::uuids::basic_random_generator<std::mt19937_64> gen(&random_engine);
    new_uuid = gen();

    // Overwrite the relevant fields with the timestamp and version/variant bits.
    // Place the 48-bit timestamp into the first 6 bytes (big-endian)
    new_uuid.data[0] = static_cast<unsigned char>((ts_48 >> 40) & 0xFF);
    new_uuid.data[1] = static_cast<unsigned char>((ts_48 >> 32) & 0xFF);
    new_uuid.data[2] = static_cast<unsigned char>((ts_48 >> 24) & 0xFF);
    new_uuid.data[3] = static_cast<unsigned char>((ts_48 >> 16) & 0xFF);
    new_uuid.data[4] = static_cast<unsigned char>((ts_48 >> 8) & 0xFF);
    new_uuid.data[5] = static_cast<unsigned char>(ts_48 & 0xFF);

    // Set the 4-bit version to 0111 (v7) in the most significant bits of byte 6
    // The 4 least significant bits of this byte are part of rand_a, so we preserve them.
    new_uuid.data[6] = (0x70) | (new_uuid.data[6] & 0x0F);

    // Set the 2-bit variant to 10 (RFC 4122) in the most significant bits of byte 8
    // The 6 least significant bits of this byte are part of rand_b, so we preserve them.
    new_uuid.data[8] = (0x80) | (new_uuid.data[8] & 0x3F);

    return new_uuid;
}

}
