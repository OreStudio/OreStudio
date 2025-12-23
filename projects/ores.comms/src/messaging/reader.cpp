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
#include "ores.comms/messaging/reader.hpp"

#include <expected>

namespace ores::comms::messaging {

std::expected<std::uint16_t, error_code>
reader::read_uint16(std::span<const std::byte>& data) {
    if (data.size() < 2) {
        return std::unexpected(error_code::payload_too_large);
    }
    std::uint16_t value = (static_cast<std::uint16_t>(data[0]) << 8) |
                          static_cast<std::uint16_t>(data[1]);
    data = data.subspan(2);
    return value;
}

std::expected<std::uint32_t, error_code>
reader::read_uint32(std::span<const std::byte>& data) {
    if (data.size() < 4) {
        return std::unexpected(error_code::payload_too_large);
    }
    std::uint32_t value = (static_cast<std::uint32_t>(data[0]) << 24) |
                          (static_cast<std::uint32_t>(data[1]) << 16) |
                          (static_cast<std::uint32_t>(data[2]) << 8) |
                          static_cast<std::uint32_t>(data[3]);
    data = data.subspan(4);
    return value;
}

std::expected<std::int64_t, error_code>
reader::read_int64(std::span<const std::byte>& data) {
    if (data.size() < 8) {
        return std::unexpected(error_code::payload_too_large);
    }
    std::int64_t value = (static_cast<std::int64_t>(data[0]) << 56) |
                         (static_cast<std::int64_t>(data[1]) << 48) |
                         (static_cast<std::int64_t>(data[2]) << 40) |
                         (static_cast<std::int64_t>(data[3]) << 32) |
                         (static_cast<std::int64_t>(data[4]) << 24) |
                         (static_cast<std::int64_t>(data[5]) << 16) |
                         (static_cast<std::int64_t>(data[6]) << 8) |
                         static_cast<std::int64_t>(data[7]);
    data = data.subspan(8);
    return value;
}

std::expected<std::uint64_t, error_code>
reader::read_uint64(std::span<const std::byte>& data) {
    if (data.size() < 8) {
        return std::unexpected(error_code::payload_too_large);
    }
    std::uint64_t value = (static_cast<std::uint64_t>(data[0]) << 56) |
                          (static_cast<std::uint64_t>(data[1]) << 48) |
                          (static_cast<std::uint64_t>(data[2]) << 40) |
                          (static_cast<std::uint64_t>(data[3]) << 32) |
                          (static_cast<std::uint64_t>(data[4]) << 24) |
                          (static_cast<std::uint64_t>(data[5]) << 16) |
                          (static_cast<std::uint64_t>(data[6]) << 8) |
                          static_cast<std::uint64_t>(data[7]);
    data = data.subspan(8);
    return value;
}

std::expected<std::string, error_code>
reader::read_string(std::span<const std::byte>& data) {
    auto len_result = read_uint16(data);
    if (!len_result) {
        return std::unexpected(len_result.error());
    }
    auto len = *len_result;
    if (data.size() < len) {
        return std::unexpected(error_code::payload_too_large);
    }
    std::string str(reinterpret_cast<const char*>(data.data()), len);
    data = data.subspan(len);
    return str;
}

std::expected<boost::uuids::uuid, error_code>
reader::read_uuid(std::span<const std::byte>& data) {
    if (data.size() < 16) {
        return std::unexpected(error_code::payload_too_large);
    }
    boost::uuids::uuid uuid;
    std::memcpy(
        uuid.data(),
        reinterpret_cast<const unsigned char*>(data.data()),
        16
    );

    data = data.subspan(16);
    return uuid;
}

std::expected<bool, error_code>
reader::read_bool(std::span<const std::byte>& data) {
    if (data.size() < 1) {
        return std::unexpected(error_code::payload_too_large);
    }
    bool value = std::to_integer<uint8_t>(data[0]);
    data = data.subspan(1);
    return value;
}

}
