/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_COMMS_MESSAGING_COMPRESSION_TYPE_HPP
#define ORES_COMMS_MESSAGING_COMPRESSION_TYPE_HPP

#include <cstdint>
#include <ostream>
#include <string_view>

namespace ores::comms::messaging {

/**
 * @brief Compression algorithm used for payload compression.
 *
 * The compression type is stored in the high byte of the former reserved1
 * field in the frame header. Using none (0x00) maintains backward
 * compatibility with uncompressed payloads.
 */
enum class compression_type : std::uint8_t {
    // No compression (default)
    none = 0x00,
    // zlib/deflate compression
    zlib = 0x01,
    // gzip compression
    gzip = 0x02,
    // bzip2 compression
    bzip2 = 0x03
};

/**
 * @brief Convert compression_type to string representation.
 *
 * @param v The enum value to convert.
 * @return String view of the enum name, or empty for unknown values.
 */
[[nodiscard]] constexpr std::string_view to_string(compression_type v) noexcept {
    switch (v) {
    case compression_type::none: return "none";
    case compression_type::zlib: return "zlib";
    case compression_type::gzip: return "gzip";
    case compression_type::bzip2: return "bzip2";
    default: return {};
    }
}

/**
 * @brief Stream output operator for compression_type.
 *
 * Outputs the enum name followed by the hex value in parentheses.
 * Example: "get_currencies_request (0x1001)"
 * If the value is unknown, outputs "[unknown]" prefix.
 */
inline std::ostream& operator<<(std::ostream& os, compression_type v) {
    const auto name = to_string(v);
    if (name.empty()) {
        os << "[unknown]";
    } else {
        os << name;
    }
    return os << " (0x" << std::hex
              << static_cast<std::uint8_t>(v)
              << std::dec << ")";
}

}

#endif
