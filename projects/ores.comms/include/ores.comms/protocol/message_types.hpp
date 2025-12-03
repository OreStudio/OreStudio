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
#ifndef ORES_COMMS_PROTOCOL_MESSAGE_TYPES_HPP
#define ORES_COMMS_PROTOCOL_MESSAGE_TYPES_HPP

#include <cstdint>
#include <iomanip>
#include <iosfwd>

// Configure magic_enum to support our enum value ranges
// Core protocol: 0x0000 - 0x0FFF
// Risk subsystem: 0x1000 - 0x1FFF
// Accounts subsystem: 0x2000 - 0x2FFF
// Variability subsystem: 0x3000 - 0x3FFF
#define MAGIC_ENUM_RANGE_MIN 0
#define MAGIC_ENUM_RANGE_MAX 0x4000

#include <magic_enum/magic_enum.hpp>

namespace ores::comms::protocol {

 // "ORES" in ASCII
constexpr std::uint32_t PROTOCOL_MAGIC = 0x4F524553;

// Protocol version 6 introduces variability subsystem range (0x3000-0x3FFF)
constexpr std::uint16_t PROTOCOL_VERSION_MAJOR = 6;
constexpr std::uint16_t PROTOCOL_VERSION_MINOR = 0;

// Subsystem message type ranges
constexpr std::uint16_t CORE_SUBSYSTEM_MIN = 0x0000;
constexpr std::uint16_t CORE_SUBSYSTEM_MAX = 0x0FFF;
constexpr std::uint16_t RISK_SUBSYSTEM_MIN = 0x1000;
constexpr std::uint16_t RISK_SUBSYSTEM_MAX = 0x1FFF;
constexpr std::uint16_t ACCOUNTS_SUBSYSTEM_MIN = 0x2000;
constexpr std::uint16_t ACCOUNTS_SUBSYSTEM_MAX = 0x2FFF;
constexpr std::uint16_t VARIABILITY_SUBSYSTEM_MIN = 0x3000;
constexpr std::uint16_t VARIABILITY_SUBSYSTEM_MAX = 0x3FFF;

enum class message_type {
    // Core protocol messages (0x0000 - 0x0FFF)
    handshake_request = 0x0001,
    handshake_response = 0x0002,
    handshake_ack = 0x0003,
    error_response = 0x0004,

    // Risk subsystem messages (0x1000 - 0x1FFF)
    get_currencies_request = 0x1001,
    get_currencies_response = 0x1002,
    save_currency_request = 0x1003,
    save_currency_response = 0x1004,
    delete_currency_request = 0x1005,
    delete_currency_response = 0x1006,
    get_currency_history_request = 0x1007,
    get_currency_history_response = 0x1008,

    // Accounts subsystem messages (0x2000 - 0x2FFF)
    create_account_request = 0x2001,
    create_account_response = 0x2002,
    list_accounts_request = 0x2003,
    list_accounts_response = 0x2004,
    login_request = 0x2005,
    login_response = 0x2006,
    unlock_account_request = 0x2007,
    unlock_account_response = 0x2008,
    delete_account_request = 0x2009,
    delete_account_response = 0x200A,
    list_login_info_request = 0x200B,
    list_login_info_response = 0x200C,
    create_initial_admin_request = 0x200F,
    create_initial_admin_response = 0x2010,
    bootstrap_status_request = 0x2011,
    bootstrap_status_response = 0x2012,

    // Variability subsystem messages (0x3000 - 0x3FFF)
    list_feature_flags_request = 0x3000,
    list_feature_flags_response = 0x3001,

    last_value
};

enum class error_code {
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
    last_value
};

/**
 * @brief Stream output operator for message_type.
 *
 * Uses magic_enum to provide human-readable enum names with hex values in logs.
 * Example: "get_currencies_request (0x1001)" instead of just "0x1001"
 */
inline std::ostream& operator<<(std::ostream& os, message_type mt) {
    return os << magic_enum::enum_name(mt)
              << " (0x" << std::hex << static_cast<std::uint16_t>(mt) << std::dec << ")";
}

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

}

#endif
