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
#include <expected>
#include "ores.comms/protocol/reader.hpp"

namespace ores::comms::protocol {

std::expected<std::uint16_t, comms::protocol::error_code>
reader::read_uint16(std::span<const std::uint8_t>& data) {
    if (data.size() < 2) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    std::uint16_t value = (static_cast<std::uint16_t>(data[0]) << 8) |
                          static_cast<std::uint16_t>(data[1]);
    data = data.subspan(2);
    return value;
}

std::expected<std::uint32_t, comms::protocol::error_code>
reader::read_uint32(std::span<const std::uint8_t>& data) {
    if (data.size() < 4) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    std::uint32_t value = (static_cast<std::uint32_t>(data[0]) << 24) |
                          (static_cast<std::uint32_t>(data[1]) << 16) |
                          (static_cast<std::uint32_t>(data[2]) << 8) |
                          static_cast<std::uint32_t>(data[3]);
    data = data.subspan(4);
    return value;
}

std::expected<std::string, comms::protocol::error_code>
reader::read_string(std::span<const std::uint8_t>& data) {
    auto len_result = read_uint16(data);
    if (!len_result) {
        return std::unexpected(len_result.error());
    }
    auto len = *len_result;
    if (data.size() < len) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    std::string str(reinterpret_cast<const char*>(data.data()), len);
    data = data.subspan(len);
    return str;
}

std::expected<boost::uuids::uuid, comms::protocol::error_code>
reader::read_uuid(std::span<const std::uint8_t>& data) {
    if (data.size() < 16) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    boost::uuids::uuid uuid;
    std::copy_n(data.begin(), 16, uuid.begin());
    data = data.subspan(16);
    return uuid;
}

std::expected<bool, comms::protocol::error_code>
reader::read_bool(std::span<const std::uint8_t>& data) {
    if (data.size() < 1) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    bool value = data[0] != 0;
    data = data.subspan(1);
    return value;
}

}
