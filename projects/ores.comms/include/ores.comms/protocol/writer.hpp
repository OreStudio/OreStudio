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
#ifndef ORES_COMMS_MESSAGING_WRITE_HPP
#define ORES_COMMS_MESSAGING_WRITE_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <vector>
#include <string>
#include <cstdint>
#include <expected>
#include <boost/uuid/uuid_io.hpp>


namespace ores::comms::protocol {

/**
 * @brief Helper to write network data.
 */
class writer {
public:
    /**
     * @brief Helper to write a 16-bit integer in network byte order.
     */
    static void write_uint16(std::vector<std::uint8_t>& buffer,
        std::uint16_t value);

    /**
     * @brief Helper to write a 32-bit integer in network byte order.
     */
    static void write_uint32(std::vector<std::uint8_t>& buffer,
        std::uint32_t value);

    /**
     * @brief Helper to write a string with 16-bit length prefix.
     */
    static void write_string(std::vector<std::uint8_t>& buffer,
        const std::string& str);

    /**
     * @brief Helper to write a boolean (1 byte).
     */
    static void write_bool(std::vector<std::uint8_t>& buffer, bool value);

    /**
     * @brief Helper to write a UUID (16 bytes).
     */
    static void write_uuid(std::vector<std::uint8_t>& buffer,
    const boost::uuids::uuid& uuid);
};

}

#endif
