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
#ifndef ORES_COMMS_MESSAGING_MESSAGE_TYPES_HPP
#define ORES_COMMS_MESSAGING_MESSAGE_TYPES_HPP

#include <ostream>
#include <cstdint>

// Configure magic_enum to support our enum value ranges
#define MAGIC_ENUM_RANGE_MIN 0
#define MAGIC_ENUM_RANGE_MAX 0x4000

#include <magic_enum/magic_enum.hpp>

namespace ores::comms::messaging {

 // "ORES" in ASCII
constexpr std::uint32_t PROTOCOL_MAGIC = 0x4F524553;

// Protocol version 7 adds correlation_id field to frame header for
// request/response matching, enabling concurrent operations and server-push
// notifications.
//
// Version 7.1 adds subscription protocol messages (subscribe_request,
// subscribe_response, unsubscribe_request, unsubscribe_response, notification)
// for server-push event notifications.
//
// Version 7.2 changes logout_request to have empty payload. Account ID is now
// determined from session context to prevent clients from forging logout
// requests for other users.
//
// Version 7.3 adds database_status_notification message for server-push
// database health status updates, and database_unavailable error code.
// Version 8.0 removes requester_account_id from lock_account_request and
// unlock_account_request. Authorization is now handled via server-side session
// tracking instead of client-provided identity. This is a breaking change.
//
// Version 9.0 adds optional payload compression support. The reserved1 field
// in the frame header is now used for compression_type (1 byte) and flags
// (1 byte). Supported compression algorithms: none, zlib, gzip, bzip2.
// Uncompressed payloads remain fully supported (compression_type::none).
//
// Version 10.0 replaces valid_from/valid_to fields with recorded_by/recorded_at
// in domain types (currency, account, feature_flags). This is a breaking change
// affecting all entity serialization in the protocol.
//
// Version 10.1 adds update_account_request and update_account_response messages
// for editing existing account email and admin status.
//
// Version 11.0 changes lock_account_request and unlock_account_request to support
// batch operations with a vector of account IDs. Responses now return a vector of
// per-account results (lock_account_result/unlock_account_result). This is a
// breaking change as the wire format is incompatible with previous versions.
//
// Version 12.0 adds reset_password_request and reset_password_response messages
// for admin-initiated password reset. Adds password_reset_required field to
// login_response and list_login_info_response. This is a breaking change as
// existing serialization formats are extended with new fields.
constexpr std::uint16_t PROTOCOL_VERSION_MAJOR = 12;
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

/**
 * @brief Compression algorithm used for payload compression.
 *
 * The compression type is stored in the high byte of the former reserved1
 * field in the frame header. Using none (0x00) maintains backward
 * compatibility with uncompressed payloads.
 */
enum class compression_type : std::uint8_t {
    none = 0x00,   // No compression (default)
    zlib = 0x01,   // zlib/deflate compression
    gzip = 0x02,   // gzip compression
    bzip2 = 0x03   // bzip2 compression
};

enum class message_type {
    // Core protocol messages (0x0000 - 0x0FFF)
    handshake_request = 0x0001,
    handshake_response = 0x0002,
    handshake_ack = 0x0003,
    error_response = 0x0004,
    ping = 0x0005,
    pong = 0x0006,

    // Subscription/Notification messages (0x0010 - 0x001F)
    subscribe_request = 0x0010,
    subscribe_response = 0x0011,
    unsubscribe_request = 0x0012,
    unsubscribe_response = 0x0013,
    notification = 0x0014,

    // System status notifications (0x0020 - 0x002F)
    database_status_notification = 0x0020,

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
    logout_request = 0x200D,
    logout_response = 0x200E,
    create_initial_admin_request = 0x200F,
    create_initial_admin_response = 0x2010,
    bootstrap_status_request = 0x2011,
    bootstrap_status_response = 0x2012,
    lock_account_request = 0x2013,
    lock_account_response = 0x2014,
    update_account_request = 0x2015,
    update_account_response = 0x2016,
    get_account_history_request = 0x2017,
    get_account_history_response = 0x2018,
    reset_password_request = 0x2019,
    reset_password_response = 0x201A,
    change_password_request = 0x201B,
    change_password_response = 0x201C,
    update_my_email_request = 0x201D,
    update_my_email_response = 0x201E,

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
    database_unavailable = 0x0011,
    decompression_failed = 0x0012,
    unsupported_compression = 0x0013,
    compression_failed = 0x0014,
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

/**
 * @brief Stream output operator for compression_type.
 */
inline std::ostream& operator<<(std::ostream& os, compression_type ct) {
    return os << magic_enum::enum_name(ct)
              << " (0x" << std::hex << static_cast<std::uint8_t>(ct) << std::dec << ")";
}

}

#endif
