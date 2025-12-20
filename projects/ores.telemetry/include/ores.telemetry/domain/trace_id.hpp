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
#ifndef ORES_TELEMETRY_DOMAIN_TRACE_ID_HPP
#define ORES_TELEMETRY_DOMAIN_TRACE_ID_HPP

#include <array>
#include <cstddef>
#include <cstdint>
#include <string>

namespace ores::telemetry::domain {

/**
 * @brief A 128-bit trace identifier, compatible with OpenTelemetry.
 *
 * The trace_id uniquely identifies a distributed trace. All spans belonging
 * to the same trace share the same trace_id. The 128-bit size matches the
 * OpenTelemetry and W3C Trace Context specifications.
 *
 * The internal structure embeds metadata for efficient local generation:
 * - Bytes 0-5: 48-bit timestamp in milliseconds
 * - Bytes 6-7: 16-bit machine identifier (locally derived)
 * - Bytes 8-15: 64-bit random component for uniqueness
 */
struct trace_id final {
    /**
     * @brief The raw 128-bit identifier.
     */
    std::array<std::byte, 16> bytes{};

    /**
     * @brief Checks if the trace_id is valid (not all zeros).
     */
    bool is_valid() const;

    /**
     * @brief Converts the trace_id to a 32-character lowercase hex string.
     */
    std::string to_hex() const;

    /**
     * @brief Creates a trace_id from a 32-character hex string.
     * @param hex The hex string representation.
     * @return The parsed trace_id, or an invalid trace_id if parsing fails.
     */
    static trace_id from_hex(std::string_view hex);

    /**
     * @brief Default comparison operator.
     */
    auto operator<=>(const trace_id&) const = default;
};

/**
 * @brief Stream output operator for trace_id.
 */
std::ostream& operator<<(std::ostream& os, const trace_id& id);

}

#endif
