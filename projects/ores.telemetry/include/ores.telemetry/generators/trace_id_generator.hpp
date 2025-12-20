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
#ifndef ORES_TELEMETRY_GENERATORS_TRACE_ID_GENERATOR_HPP
#define ORES_TELEMETRY_GENERATORS_TRACE_ID_GENERATOR_HPP

#include <cstdint>
#include <mutex>
#include <random>
#include "ores.telemetry/domain/trace_id.hpp"

namespace ores::telemetry::generators {

/**
 * @brief Generator for 128-bit trace IDs with embedded metadata.
 *
 * The generated trace_id has the following structure:
 * - Bytes 0-5 (48 bits): Timestamp in milliseconds since Unix epoch
 * - Bytes 6-7 (16 bits): Machine identifier (locally derived hash)
 * - Bytes 8-15 (64 bits): Random component for uniqueness
 *
 * This structure provides:
 * - Time-sortability (traces can be ordered by creation time)
 * - Machine identification (for debugging in distributed systems)
 * - High uniqueness (64-bit random + timestamp + machine = very low collision)
 * - OpenTelemetry compatibility (128-bit size)
 */
class trace_id_generator final {
public:
    /**
     * @brief Constructs the generator with locally derived machine ID.
     *
     * The machine ID is derived from system properties (hostname, MAC address)
     * and remains constant for the lifetime of the process.
     */
    trace_id_generator();

    /**
     * @brief Constructs the generator with an explicit machine ID.
     *
     * @param machine_id The 16-bit machine identifier.
     */
    explicit trace_id_generator(std::uint16_t machine_id);

    /**
     * @brief Generates a new trace_id.
     *
     * Thread-safe: Multiple threads can call this concurrently.
     *
     * @return A unique trace_id with embedded timestamp and machine ID.
     */
    domain::trace_id operator()();

    /**
     * @brief Gets the machine ID being used by this generator.
     */
    std::uint16_t machine_id() const;

private:
    std::uint16_t machine_id_;
    std::mt19937_64 random_engine_;
    mutable std::mutex mutex_;

    static std::uint16_t derive_machine_id();
};

}

#endif
