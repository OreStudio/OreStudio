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
#ifndef ORES_UTILITY_SERIALIZATION_READER_HPP
#define ORES_UTILITY_SERIALIZATION_READER_HPP

#include <span>
#include <vector>
#include <string>
#include <cstdint>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/serialization/error_code.hpp"

namespace ores::utility::serialization {

/**
 * @brief Helper class to read binary data in network byte order.
 *
 * Provides static methods to deserialize various data types from a byte span.
 * All multi-byte integers are read in big-endian (network) byte order.
 * The span is advanced past the read data on success.
 */
class reader {
public:
    /**
     * @brief Read a single byte.
     */
    static std::expected<std::uint8_t, error_code>
    read_uint8(std::span<const std::byte>& data);

    /**
     * @brief Read a 16-bit integer in network byte order.
     */
    static std::expected<std::uint16_t, error_code>
    read_uint16(std::span<const std::byte>& data);

    /**
     * @brief Read a 32-bit integer in network byte order.
     */
    static std::expected<std::uint32_t, error_code>
    read_uint32(std::span<const std::byte>& data);

    /**
     * @brief Read a signed 64-bit integer in network byte order.
     */
    static std::expected<std::int64_t, error_code>
    read_int64(std::span<const std::byte>& data);

    /**
     * @brief Read an unsigned 64-bit integer in network byte order.
     */
    static std::expected<std::uint64_t, error_code>
    read_uint64(std::span<const std::byte>& data);

    /**
     * @brief Read a string with 16-bit length prefix.
     *
     * @note Maximum string length is 65535 bytes. For larger strings, use read_string32.
     */
    static std::expected<std::string, error_code>
    read_string(std::span<const std::byte>& data);

    /**
     * @brief Read a string with 32-bit length prefix.
     *
     * Use this for strings that may exceed 65535 bytes (e.g., SVG data, large text).
     */
    static std::expected<std::string, error_code>
    read_string32(std::span<const std::byte>& data);

    /**
     * @brief Read a UUID (16 bytes).
     */
    static std::expected<boost::uuids::uuid, error_code>
    read_uuid(std::span<const std::byte>& data);

    /**
     * @brief Read a boolean (1 byte).
     */
    static std::expected<bool, error_code>
    read_bool(std::span<const std::byte>& data);
};

}

#endif
