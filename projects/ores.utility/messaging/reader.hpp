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
#ifndef ORES_UTILITY_MESSAGING_READER_HPP
#define ORES_UTILITY_MESSAGING_READER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <span>
#include <vector>
#include <string>
#include <cstdint>
#include <expected>
#include <boost/uuid/uuid_io.hpp>
#include "ores.comms/protocol/message_types.hpp"

namespace ores::utility::messaging {

/**
 * @brief Helper to read network data.
 */
class reader {
public:
    /**
     * @brief Helper to read a 16-bit integer in network byte order.
     */
    static std::expected<std::uint16_t, comms::protocol::error_code>
    read_uint16(std::span<const std::uint8_t>& data);

    /**
     * @brief Helper to read a 32-bit integer in network byte order.
     */
    static std::expected<std::uint32_t, comms::protocol::error_code>
    read_uint32(std::span<const std::uint8_t>& data);

    /**
     * @brief Helper to write a UUID (16 bytes).
     */
    static void write_uuid(std::vector<std::uint8_t>& buffer,
        const boost::uuids::uuid& uuid);

    /**
     * @brief Helper to read a string with 16-bit length prefix.
     */
    static std::expected<std::string, comms::protocol::error_code>
    read_string(std::span<const std::uint8_t>& data);

    /**
     * @brief Helper to read a UUID (16 bytes).
     */
    static std::expected<boost::uuids::uuid, comms::protocol::error_code>
    read_uuid(std::span<const std::uint8_t>& data);

    /**
     * @brief Helper to read a boolean (1 byte).
     */
    static std::expected<bool, comms::protocol::error_code>
    read_bool(std::span<const std::uint8_t>& data);
};

}

#endif
