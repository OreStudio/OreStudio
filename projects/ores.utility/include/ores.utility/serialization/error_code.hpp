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
#ifndef ORES_UTILITY_SERIALIZATION_ERROR_CODE_HPP
#define ORES_UTILITY_SERIALIZATION_ERROR_CODE_HPP

#include <string>
#include <ostream>
#include <cstdint>
#include <magic_enum/magic_enum.hpp>

namespace ores::utility::serialization {

/**
 * @brief Error codes used during message serialization and deserialization.
 *
 * These codes indicate specific failures that can occur when reading or
 * writing binary protocol data, as well as higher-level protocol errors.
 */
enum class error_code : std::uint16_t {
    none = 0x0000,
    version_mismatch = 0x0001,
    crc_validation_failed = 0x0002,
    invalid_message_type = 0x0003,
    handshake_timeout = 0x0004,
    handshake_failed = 0x0005,
    payload_too_large = 0x0006,
    network_error = 0x0007,
    handler_error = 0x0008,
    database_error = 0x0009,
    authentication_failed = 0x000A,
    authorization_failed = 0x000B,
    invalid_request = 0x000C,
    bootstrap_mode_only = 0x000D,
    bootstrap_mode_forbidden = 0x000E,
    weak_password = 0x000F,
    not_localhost = 0x0010,
    database_unavailable = 0x0011,
    decompression_failed = 0x0012,
    unsupported_compression = 0x0013,
    compression_failed = 0x0014,
    signup_disabled = 0x0015,
    username_taken = 0x0016,
    email_taken = 0x0017,
    signup_requires_authorization = 0x0018,
    payload_incomplete = 0x0019,
    limit_exceeded = 0x001A,
    last_value
};

/**
 * @brief Stream output operator for error_code.
 *
 * Uses magic_enum to provide human-readable enum names with hex values in logs.
 * Example: "version_mismatch (0x0001)" instead of just "0x0001"
 */
inline std::ostream& operator<<(std::ostream& os, error_code ec) {
    return os << magic_enum::enum_name(ec)
              << " (0x" << std::hex << static_cast<std::uint16_t>(ec) << std::dec << ")";
}

/**
 * @brief Convert error_code to string for display.
 */
inline std::string to_string(error_code ec) {
    return std::string(magic_enum::enum_name(ec));
}

}

#endif
