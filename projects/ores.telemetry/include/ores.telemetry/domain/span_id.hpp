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
#ifndef ORES_TELEMETRY_DOMAIN_SPAN_ID_HPP
#define ORES_TELEMETRY_DOMAIN_SPAN_ID_HPP

#include <array>
#include <cstddef>
#include <cstdint>
#include <string>

namespace ores::telemetry::domain {

/**
 * @brief A 64-bit span identifier, compatible with OpenTelemetry.
 *
 * The span_id uniquely identifies a single operation within a trace. Each
 * span has its own span_id and may reference a parent span_id to form the
 * trace tree structure. The 64-bit size matches the OpenTelemetry and W3C
 * Trace Context specifications.
 *
 * The internal structure embeds a timestamp for time-sortability:
 * - Bytes 0-5: 48-bit timestamp in milliseconds
 * - Bytes 6-7: 16-bit sequence/random component
 */
struct span_id final {
    /**
     * @brief The raw 64-bit identifier.
     */
    std::array<std::byte, 8> bytes{};

    /**
     * @brief Checks if the span_id is valid (not all zeros).
     */
    bool is_valid() const;

    /**
     * @brief Converts the span_id to a 16-character lowercase hex string.
     */
    std::string to_hex() const;

    /**
     * @brief Creates a span_id from a 16-character hex string.
     * @param hex The hex string representation.
     * @return The parsed span_id, or an invalid span_id if parsing fails.
     */
    static span_id from_hex(std::string_view hex);

    /**
     * @brief Default comparison operator.
     */
    auto operator<=>(const span_id&) const = default;
};

/**
 * @brief Stream output operator for span_id.
 */
std::ostream& operator<<(std::ostream& os, const span_id& id);

}

#endif
