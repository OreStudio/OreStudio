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
#include "ores.telemetry/domain/trace_id.hpp"
#include <algorithm>
#include <iomanip>
#include <ostream>
#include <sstream>

namespace ores::telemetry::domain {

bool trace_id::is_valid() const {
    return std::any_of(bytes.begin(), bytes.end(),
                       [](std::byte b) { return b != std::byte{0}; });
}

std::string trace_id::to_hex() const {
    std::ostringstream oss;
    oss << std::hex << std::setfill('0');
    for (const auto b : bytes) {
        oss << std::setw(2) << static_cast<unsigned>(b);
    }
    return oss.str();
}

trace_id trace_id::from_hex(std::string_view hex) {
    trace_id result;
    if (hex.size() != 32) {
        return result; // Invalid, returns all zeros
    }

    for (std::size_t i = 0; i < 16; ++i) {
        const auto byte_str = hex.substr(i * 2, 2);
        unsigned int value = 0;
        std::istringstream iss{std::string(byte_str)};
        iss >> std::hex >> value;
        if (iss.fail()) {
            return trace_id{}; // Invalid hex
        }
        result.bytes[i] = static_cast<std::byte>(value);
    }
    return result;
}

std::ostream& operator<<(std::ostream& os, const trace_id& id) {
    return os << id.to_hex();
}

}
