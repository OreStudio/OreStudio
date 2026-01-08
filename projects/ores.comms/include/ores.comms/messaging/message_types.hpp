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
#include "ores.utility/serialization/error_code.hpp"

// Configure magic_enum to support our enum value ranges
#ifdef MAGIC_ENUM_RANGE_MIN
#undef MAGIC_ENUM_RANGE_MIN
#endif
#ifdef MAGIC_ENUM_RANGE_MAX
#undef MAGIC_ENUM_RANGE_MAX
#endif
#define MAGIC_ENUM_RANGE_MIN 0
#define MAGIC_ENUM_RANGE_MAX 0x6000

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
//
// Version 13.0 adds role-based access control (RBAC) with full authorization
// system. New domain types: permission, role, account_role, role_permission.
// New RBAC messages: list_roles_request/response, list_permissions_request/response,
// assign_role_request/response, revoke_role_request/response,
// get_account_roles_request/response, get_account_permissions_request/response.
// Adds authorization checks to all protected endpoints. This is a breaking change
// as it introduces mandatory RBAC enforcement for administrative operations.
//
// Version 13.1 adds user self-registration (signup) workflow. New messages:
// signup_request/response for creating accounts without admin privileges.
// New error codes: signup_disabled, username_taken, email_taken,
// signup_requires_authorization. Signup is controlled by system.user_signups
// feature flag.
//
// Version 14.0 adds assets subsystem for dynamic image loading. New messages:
// get_currency_images_request/response for retrieving currency-image mappings,
// get_images_request/response for batched image retrieval (max 100 per request).
// This enables the UI to display flag icons for currencies.
//
// Version 15.0 adds session tracking and statistics. New messages:
// list_sessions_request/response for querying session history by account,
// get_session_statistics_request/response for aggregated session metrics.
// Sessions now track bytes sent/received, client version, and geolocation.
// Session data is stored in a TimescaleDB hypertable for time-series analysis.
//
// Version 15.1 adds telemetry subsystem for log record streaming. New messages:
// submit_log_records_request for fire-and-forget batched log record submission.
// Clients can stream telemetry data to the server for centralized storage and
// analysis. Controlled by telemetry.streaming.enabled feature flag.
//
// Version 16.0 adds entity_ids field to notification_message. This allows
// clients to identify which specific entities changed (e.g., currency ISO codes
// or account IDs) rather than just knowing that some entity of a type changed.
// This is a breaking change as the wire format for notifications is extended.
//
// Version 16.1 adds list_images_request/response to list all available flag images,
// and set_currency_image_request/response to assign or remove flags from currencies.
//
// Version 16.2 adds server-side telemetry persistence. New messages:
// submit_telemetry_response (0x5001) for acknowledging batch submissions.
// get_telemetry_logs_request/response (0x5010/0x5011) for querying raw logs.
// get_telemetry_stats_request/response (0x5020/0x5021) for querying aggregated stats.
// Telemetry logs are stored in a TimescaleDB hypertable with 30-day retention.
//
// Version 17.0 removes obsolete currency_image junction table messages. The
// image_id field is now directly on the currency entity, so currency->image
// mappings come from get_currencies_response. Flag changes appear in currency
// version history. Removed messages: get_currency_images_request/response,
// set_currency_image_request/response. Breaking change.
//
// Version 18.0 uses 32-bit length prefix for svg_data in get_images_response.
// Previously used 16-bit length which truncated SVGs larger than 65535 bytes.
// This is a breaking change affecting image serialization.
//
// Version 19.0 adds recorded_at field to list_feature_flags_response.
// Previously the timestamp was not serialized. Breaking change.
//
// Version 20.0 adds version field to list_feature_flags_response.
// Breaking change.
constexpr std::uint16_t PROTOCOL_VERSION_MAJOR = 20;
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
constexpr std::uint16_t ASSETS_SUBSYSTEM_MIN = 0x4000;
constexpr std::uint16_t ASSETS_SUBSYSTEM_MAX = 0x4FFF;
constexpr std::uint16_t TELEMETRY_SUBSYSTEM_MIN = 0x5000;
constexpr std::uint16_t TELEMETRY_SUBSYSTEM_MAX = 0x5FFF;

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

    // Authorization/RBAC messages (0x2020 - 0x202F)
    list_roles_request = 0x2020,
    list_roles_response = 0x2021,
    list_permissions_request = 0x2022,
    list_permissions_response = 0x2023,
    assign_role_request = 0x2024,
    assign_role_response = 0x2025,
    revoke_role_request = 0x2026,
    revoke_role_response = 0x2027,
    get_account_roles_request = 0x2028,
    get_account_roles_response = 0x2029,
    get_account_permissions_request = 0x202A,
    get_account_permissions_response = 0x202B,
    get_role_request = 0x202C,
    get_role_response = 0x202D,

    // User self-registration messages (0x2030 - 0x203F)
    signup_request = 0x2030,
    signup_response = 0x2031,

    // Session tracking messages (0x2040 - 0x204F)
    list_sessions_request = 0x2040,
    list_sessions_response = 0x2041,
    get_session_statistics_request = 0x2042,
    get_session_statistics_response = 0x2043,
    get_active_sessions_request = 0x2044,
    get_active_sessions_response = 0x2045,

    // Variability subsystem messages (0x3000 - 0x3FFF)
    list_feature_flags_request = 0x3000,
    list_feature_flags_response = 0x3001,
    save_feature_flag_request = 0x3002,
    save_feature_flag_response = 0x3003,
    delete_feature_flag_request = 0x3004,
    delete_feature_flag_response = 0x3005,

    // Assets subsystem messages (0x4000 - 0x4FFF)
    // Note: 0x4000-0x4001 and 0x4006-0x4007 removed in v17.0 (currency_image)
    get_images_request = 0x4002,
    get_images_response = 0x4003,
    list_images_request = 0x4004,
    list_images_response = 0x4005,

    // Telemetry subsystem messages (0x5000 - 0x5FFF)
    submit_log_records_request = 0x5000,
    submit_telemetry_response = 0x5001,
    get_telemetry_logs_request = 0x5010,
    get_telemetry_logs_response = 0x5011,
    get_telemetry_stats_request = 0x5020,
    get_telemetry_stats_response = 0x5021,

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
inline std::ostream& operator<<(std::ostream& os, ores::utility::serialization::error_code ec) {
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

/**
 * @brief Convert error_code to string for display.
 */
inline std::string to_string(ores::utility::serialization::error_code ec) {
    return std::string(magic_enum::enum_name(ec));
}

}

#endif
