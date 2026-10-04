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
#ifndef ORES_ASSETS_CORE_TESTS_IMAGE_BYTES_HPP
#define ORES_ASSETS_CORE_TESTS_IMAGE_BYTES_HPP

#include <cstddef>
#include <cstdint>
#include <vector>

namespace ores::assets::tests {

inline void append_be16(std::vector<std::uint8_t>& bytes, std::uint16_t value) {
    bytes.push_back(static_cast<std::uint8_t>(value >> 8));
    bytes.push_back(static_cast<std::uint8_t>(value & 0xFF));
}

inline void append_be32(std::vector<std::uint8_t>& bytes, std::uint32_t value) {
    bytes.push_back(static_cast<std::uint8_t>(value >> 24));
    bytes.push_back(static_cast<std::uint8_t>((value >> 16) & 0xFF));
    bytes.push_back(static_cast<std::uint8_t>((value >> 8) & 0xFF));
    bytes.push_back(static_cast<std::uint8_t>(value & 0xFF));
}

inline void append_le24(std::vector<std::uint8_t>& bytes, std::uint32_t value) {
    bytes.push_back(static_cast<std::uint8_t>(value & 0xFF));
    bytes.push_back(static_cast<std::uint8_t>((value >> 8) & 0xFF));
    bytes.push_back(static_cast<std::uint8_t>((value >> 16) & 0xFF));
}

inline void append_le32(std::vector<std::uint8_t>& bytes, std::uint32_t value) {
    bytes.push_back(static_cast<std::uint8_t>(value & 0xFF));
    bytes.push_back(static_cast<std::uint8_t>((value >> 8) & 0xFF));
    bytes.push_back(static_cast<std::uint8_t>((value >> 16) & 0xFF));
    bytes.push_back(static_cast<std::uint8_t>((value >> 24) & 0xFF));
}

/**
 * @brief The bytes of a PNG that states the given size.
 *
 * The validator reads the size from the IHDR chunk, which these bytes carry
 * whole; nothing under test decodes pixels, so those are not built.
 */
inline std::vector<std::uint8_t> png_bytes(std::size_t width, std::size_t height) {
    std::vector<std::uint8_t> bytes{0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
    append_be32(bytes, 13);
    bytes.insert(bytes.end(), {0x49, 0x48, 0x44, 0x52});
    append_be32(bytes, static_cast<std::uint32_t>(width));
    append_be32(bytes, static_cast<std::uint32_t>(height));
    bytes.insert(bytes.end(), {8, 6, 0, 0, 0});
    append_be32(bytes, 0);
    return bytes;
}

/**
 * @brief The bytes of a JPEG that states the given size.
 *
 * The validator reads the size from the SOF0 segment, which these bytes carry
 * whole; the scan that would follow is not built.
 */
inline std::vector<std::uint8_t> jpeg_bytes(std::size_t width, std::size_t height) {
    std::vector<std::uint8_t> bytes{0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46, 0x49, 0x46,
                                    0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00};
    bytes.insert(bytes.end(), {0xFF, 0xC0, 0x00, 0x11, 0x08});
    append_be16(bytes, static_cast<std::uint16_t>(height));
    append_be16(bytes, static_cast<std::uint16_t>(width));
    bytes.insert(bytes.end(), {0x03, 0x01, 0x11, 0x00, 0x02, 0x11, 0x01, 0x03, 0x11, 0x01});
    bytes.insert(bytes.end(), {0xFF, 0xD9});
    return bytes;
}

/**
 * @brief The bytes of a WebP that states the given size through a VP8X chunk.
 */
inline std::vector<std::uint8_t> webp_bytes(std::size_t width, std::size_t height) {
    std::vector<std::uint8_t> bytes{0x52, 0x49, 0x46, 0x46};
    append_le32(bytes, 22);
    bytes.insert(bytes.end(), {0x57, 0x45, 0x42, 0x50});
    bytes.insert(bytes.end(), {0x56, 0x50, 0x38, 0x58});
    append_le32(bytes, 10);
    bytes.insert(bytes.end(), {0x00, 0x00, 0x00, 0x00});
    append_le24(bytes, static_cast<std::uint32_t>(width) - 1);
    append_le24(bytes, static_cast<std::uint32_t>(height) - 1);
    return bytes;
}

}

#endif
