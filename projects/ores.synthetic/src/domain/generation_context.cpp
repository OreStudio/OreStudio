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
#include "ores.synthetic/domain/generation_context.hpp"

namespace ores::synthetic::domain {

generation_context::generation_context(std::uint64_t seed)
    : seed_(seed), engine_(seed) {}

generation_context::generation_context()
    : seed_(std::random_device{}()), engine_(seed_) {}

int generation_context::random_int(int min, int max) {
    std::uniform_int_distribution<int> dist(min, max);
    return dist(engine_);
}

bool generation_context::random_bool(double probability) {
    std::bernoulli_distribution dist(probability);
    return dist(engine_);
}

boost::uuids::uuid generation_context::generate_uuid() {
    boost::uuids::uuid uuid;
    auto now = std::chrono::system_clock::now();
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()).count();

    // Set timestamp (48 bits)
    uuid.data[0] = static_cast<uint8_t>((ms >> 40) & 0xFF);
    uuid.data[1] = static_cast<uint8_t>((ms >> 32) & 0xFF);
    uuid.data[2] = static_cast<uint8_t>((ms >> 24) & 0xFF);
    uuid.data[3] = static_cast<uint8_t>((ms >> 16) & 0xFF);
    uuid.data[4] = static_cast<uint8_t>((ms >> 8) & 0xFF);
    uuid.data[5] = static_cast<uint8_t>(ms & 0xFF);

    // Generate random bits for the rest
    std::uniform_int_distribution<uint64_t> dist;
    uint64_t random_bits = dist(engine_);

    uuid.data[6] = static_cast<uint8_t>(0x70 | ((random_bits >> 56) & 0x0F)); // Version 7
    uuid.data[7] = static_cast<uint8_t>((random_bits >> 48) & 0xFF);
    uuid.data[8] = static_cast<uint8_t>(0x80 | ((random_bits >> 40) & 0x3F)); // Variant
    uuid.data[9] = static_cast<uint8_t>((random_bits >> 32) & 0xFF);
    uuid.data[10] = static_cast<uint8_t>((random_bits >> 24) & 0xFF);
    uuid.data[11] = static_cast<uint8_t>((random_bits >> 16) & 0xFF);
    uuid.data[12] = static_cast<uint8_t>((random_bits >> 8) & 0xFF);
    uuid.data[13] = static_cast<uint8_t>(random_bits & 0xFF);

    random_bits = dist(engine_);
    uuid.data[14] = static_cast<uint8_t>((random_bits >> 8) & 0xFF);
    uuid.data[15] = static_cast<uint8_t>(random_bits & 0xFF);

    return uuid;
}

std::chrono::system_clock::time_point generation_context::past_timepoint(int years_back) {
    auto now = std::chrono::system_clock::now();
    auto days_back = random_int(1, years_back * 365);
    return now - std::chrono::hours(days_back * 24);
}

std::string generation_context::alphanumeric(std::size_t length) {
    static const char chars[] =
        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    std::string result;
    result.reserve(length);
    std::uniform_int_distribution<std::size_t> dist(0, sizeof(chars) - 2);
    for (std::size_t i = 0; i < length; ++i) {
        result += chars[dist(engine_)];
    }
    return result;
}

}
