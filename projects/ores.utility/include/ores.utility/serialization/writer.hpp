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
#ifndef ORES_UTILITY_SERIALIZATION_WRITER_HPP
#define ORES_UTILITY_SERIALIZATION_WRITER_HPP

#include <vector>
#include <string>
#include <cstdint>
#include <boost/uuid/uuid.hpp>

namespace ores::utility::serialization {

/**
 * @brief Helper class to write binary data in network byte order.
 *
 * Provides static methods to serialize various data types into a byte buffer.
 * All multi-byte integers are written in big-endian (network) byte order.
 */
class writer {
public:
    /**
     * @brief Write a single byte.
     */
    static void write_uint8(std::vector<std::byte>& buffer, std::uint8_t value);

    /**
     * @brief Write a 16-bit integer in network byte order.
     */
    static void write_uint16(std::vector<std::byte>& buffer, std::uint16_t value);

    /**
     * @brief Write a 32-bit integer in network byte order.
     */
    static void write_uint32(std::vector<std::byte>& buffer, std::uint32_t value);

    /**
     * @brief Write a signed 64-bit integer in network byte order.
     */
    static void write_int64(std::vector<std::byte>& buffer, std::int64_t value);

    /**
     * @brief Write an unsigned 64-bit integer in network byte order.
     */
    static void write_uint64(std::vector<std::byte>& buffer, std::uint64_t value);

    /**
     * @brief Write a string with 16-bit length prefix.
     */
    static void write_string(std::vector<std::byte>& buffer, const std::string& str);

    /**
     * @brief Write a boolean (1 byte).
     */
    static void write_bool(std::vector<std::byte>& buffer, bool value);

    /**
     * @brief Write a UUID (16 bytes).
     */
    static void write_uuid(std::vector<std::byte>& buffer,
        const boost::uuids::uuid& uuid);
};

}

#endif
