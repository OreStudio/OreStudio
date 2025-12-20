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
#ifndef ORES_TELEMETRY_GENERATORS_SPAN_ID_GENERATOR_HPP
#define ORES_TELEMETRY_GENERATORS_SPAN_ID_GENERATOR_HPP

#include <atomic>
#include <cstdint>
#include <random>
#include <mutex>
#include "ores.telemetry/domain/span_id.hpp"

namespace ores::telemetry::generators {

/**
 * @brief Generator for 64-bit span IDs with embedded timestamp.
 *
 * The generated span_id has the following structure:
 * - Bytes 0-5 (48 bits): Timestamp in milliseconds since Unix epoch
 * - Bytes 6-7 (16 bits): Sequence counter / random component
 *
 * This structure provides:
 * - Time-sortability within a trace
 * - Uniqueness via timestamp + sequence
 * - OpenTelemetry compatibility (64-bit size)
 */
class span_id_generator final {
public:
    /**
     * @brief Constructs the generator.
     */
    span_id_generator();

    /**
     * @brief Generates a new span_id.
     *
     * Thread-safe: Multiple threads can call this concurrently.
     *
     * @return A unique span_id with embedded timestamp.
     */
    domain::span_id operator()();

private:
    std::atomic<std::uint16_t> sequence_;
    std::uint64_t last_timestamp_ms_;
    std::mt19937 random_engine_;
    mutable std::mutex mutex_;
};

}

#endif
