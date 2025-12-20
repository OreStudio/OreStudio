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
#include "ores.telemetry/generators/span_id_generator.hpp"
#include <chrono>

namespace ores::telemetry::generators {

span_id_generator::span_id_generator()
    : sequence_(0),
      last_timestamp_ms_(0),
      random_engine_(std::random_device{}()) {}

domain::span_id span_id_generator::operator()() {
    std::lock_guard<std::mutex> lock(mutex_);

    domain::span_id result;

    // Get current timestamp in milliseconds
    const auto now = std::chrono::system_clock::now();
    const auto ms = static_cast<std::uint64_t>(
        std::chrono::duration_cast<std::chrono::milliseconds>(
            now.time_since_epoch()).count());

    // Manage sequence: reset if timestamp changed, increment otherwise
    std::uint16_t seq;
    if (ms != last_timestamp_ms_) {
        last_timestamp_ms_ = ms;
        // Add some randomness to the initial sequence to avoid
        // collisions across processes starting at the same millisecond
        seq = static_cast<std::uint16_t>(random_engine_() & 0x0FFF);
        sequence_.store(seq);
    } else {
        seq = sequence_.fetch_add(1);
        // If we overflow the sequence in a single millisecond, add random bits
        if (seq == 0xFFFF) {
            seq = static_cast<std::uint16_t>(random_engine_() & 0xFFFF);
        }
    }

    // Bytes 0-5: 48-bit timestamp (big-endian for sortability)
    result.bytes[0] = static_cast<std::byte>((ms >> 40) & 0xFF);
    result.bytes[1] = static_cast<std::byte>((ms >> 32) & 0xFF);
    result.bytes[2] = static_cast<std::byte>((ms >> 24) & 0xFF);
    result.bytes[3] = static_cast<std::byte>((ms >> 16) & 0xFF);
    result.bytes[4] = static_cast<std::byte>((ms >> 8) & 0xFF);
    result.bytes[5] = static_cast<std::byte>(ms & 0xFF);

    // Bytes 6-7: 16-bit sequence (big-endian)
    result.bytes[6] = static_cast<std::byte>((seq >> 8) & 0xFF);
    result.bytes[7] = static_cast<std::byte>(seq & 0xFF);

    return result;
}

}
