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
#include "ores.telemetry/generators/trace_id_generator.hpp"
#include "ores.platform/net/network_info.hpp"
#include <chrono>

namespace ores::telemetry::generators {

std::uint16_t trace_id_generator::derive_machine_id() {
    return platform::net::derive_machine_id_hash();
}

trace_id_generator::trace_id_generator()
    : machine_id_(derive_machine_id()),
      random_engine_(std::random_device{}()) {}

trace_id_generator::trace_id_generator(std::uint16_t machine_id)
    : machine_id_(machine_id),
      random_engine_(std::random_device{}()) {}

domain::trace_id trace_id_generator::operator()() {
    std::lock_guard<std::mutex> lock(mutex_);

    domain::trace_id result;

    // Get current timestamp in milliseconds
    const auto now = std::chrono::system_clock::now();
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()).count();

    // Bytes 0-5: 48-bit timestamp (big-endian for sortability)
    result.bytes[0] = static_cast<std::byte>((ms >> 40) & 0xFF);
    result.bytes[1] = static_cast<std::byte>((ms >> 32) & 0xFF);
    result.bytes[2] = static_cast<std::byte>((ms >> 24) & 0xFF);
    result.bytes[3] = static_cast<std::byte>((ms >> 16) & 0xFF);
    result.bytes[4] = static_cast<std::byte>((ms >> 8) & 0xFF);
    result.bytes[5] = static_cast<std::byte>(ms & 0xFF);

    // Bytes 6-7: 16-bit machine ID (big-endian)
    result.bytes[6] = static_cast<std::byte>((machine_id_ >> 8) & 0xFF);
    result.bytes[7] = static_cast<std::byte>(machine_id_ & 0xFF);

    // Bytes 8-15: 64-bit random
    const std::uint64_t random_value = random_engine_();
    result.bytes[8] = static_cast<std::byte>((random_value >> 56) & 0xFF);
    result.bytes[9] = static_cast<std::byte>((random_value >> 48) & 0xFF);
    result.bytes[10] = static_cast<std::byte>((random_value >> 40) & 0xFF);
    result.bytes[11] = static_cast<std::byte>((random_value >> 32) & 0xFF);
    result.bytes[12] = static_cast<std::byte>((random_value >> 24) & 0xFF);
    result.bytes[13] = static_cast<std::byte>((random_value >> 16) & 0xFF);
    result.bytes[14] = static_cast<std::byte>((random_value >> 8) & 0xFF);
    result.bytes[15] = static_cast<std::byte>(random_value & 0xFF);

    return result;
}

std::uint16_t trace_id_generator::machine_id() const {
    return machine_id_;
}

}
