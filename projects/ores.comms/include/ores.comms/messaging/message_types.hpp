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
#define MAGIC_ENUM_RANGE_MAX 0x7000

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
// Version 10.1 adds save_account_request and save_account_response messages
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
// Version 19.0 adds recorded_at field to get_feature_flags_response.
// Previously the timestamp was not serialized. Breaking change.
//
// Version 20.0 adds version field to get_feature_flags_response.
// Breaking change.
//
// Version 20.1 adds countries subsystem messages for CRUD operations on
// ISO 3166-1 country reference data. New messages: get_countries_request/response,
// save_country_request/response, delete_country_request/response,
// get_country_history_request/response.
//
// Version 20.2 adds image_id field to currency version history serialization.
// Previously the flag reference was missing from get_currency_history_response.
// This is a breaking change affecting currency history wire format.
//
// Version 20.3 adds feature flag history messages for querying version history.
// New messages: get_feature_flag_history_request (0x3006),
// get_feature_flag_history_response (0x3007).
//
// Version 21.0 standardizes naming and adds change tracking fields:
// - Renamed list_accounts_* to get_accounts_* for consistency with other entities
// - Renamed list_feature_flags_* to get_feature_flags_* for consistency
// - Merged create_account_* and update_account_* into save_account_* for consistency
//   with save_currency, save_country, save_feature_flag patterns
// - Added change_reason_code and change_commentary fields to all entity serialization
//   (currency, country, feature_flags, account, role, image) for audit trail support
// This is a breaking change affecting multiple message types and wire formats.
//
// Version 21.1 adds change management messages for querying the catalog of valid
// change reasons. New messages: get_change_reason_categories_request/response,
// get_change_reasons_request/response, get_change_reasons_by_category_request/response.
//
// Version 21.2 adds CRUD and history operations for change reasons and categories.
// New messages: save_change_reason_request/response, delete_change_reason_request/response,
// get_change_reason_history_request/response, save_change_reason_category_request/response,
// delete_change_reason_category_request/response, get_change_reason_category_history_request/response.
//
// Version 21.3 adds event channel discovery. New messages:
// list_event_channels_request/response (0x0015/0x0016) for querying available
// event channels that clients can subscribe to. This replaces hardcoded channel
// lists in clients with server-provided discovery.
//
// Version 21.4 adds Data Quality (DQ) subsystem messages for managing
// data organization, datasets, coding schemes, and dimension reference data.
// New subsystem range: DQ_SUBSYSTEM (0x6000-0x6FFF).
// New messages: catalog CRUD (0x6000-0x6007), data_domain CRUD (0x6008-0x600F),
// subject_area CRUD (0x6010-0x6019), dataset CRUD (0x6020-0x6027),
// methodology CRUD (0x6028-0x602F), coding_scheme CRUD (0x6030-0x6041),
// dimension CRUD (nature/origin/treatment) (0x6050-0x6067).
//
// Version 22.0 moves change_reason and change_reason_category messages from
// IAM subsystem (0x2050-0x2061) to DQ subsystem (0x6070-0x6081). This is a
// breaking change as the message type IDs have changed. Change management is
// now properly located in the Data Quality subsystem alongside other DQ types.
// The dq_message_handler now handles all DQ messages including change reasons.
constexpr std::uint16_t PROTOCOL_VERSION_MAJOR = 22;
constexpr std::uint16_t PROTOCOL_VERSION_MINOR = 0;

// Subsystem message type ranges
constexpr std::uint16_t CORE_SUBSYSTEM_MIN = 0x0000;
constexpr std::uint16_t CORE_SUBSYSTEM_MAX = 0x0FFF;
constexpr std::uint16_t RISK_SUBSYSTEM_MIN = 0x1000;
constexpr std::uint16_t RISK_SUBSYSTEM_MAX = 0x1FFF;
constexpr std::uint16_t IAM_SUBSYSTEM_MIN = 0x2000;
constexpr std::uint16_t IAM_SUBSYSTEM_MAX = 0x2FFF;
constexpr std::uint16_t VARIABILITY_SUBSYSTEM_MIN = 0x3000;
constexpr std::uint16_t VARIABILITY_SUBSYSTEM_MAX = 0x3FFF;
constexpr std::uint16_t ASSETS_SUBSYSTEM_MIN = 0x4000;
constexpr std::uint16_t ASSETS_SUBSYSTEM_MAX = 0x4FFF;
constexpr std::uint16_t TELEMETRY_SUBSYSTEM_MIN = 0x5000;
constexpr std::uint16_t TELEMETRY_SUBSYSTEM_MAX = 0x5FFF;
constexpr std::uint16_t DQ_SUBSYSTEM_MIN = 0x6000;
constexpr std::uint16_t DQ_SUBSYSTEM_MAX = 0x6FFF;

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
    list_event_channels_request = 0x0015,
    list_event_channels_response = 0x0016,

    // System status notifications (0x0020 - 0x002F)
    database_status_notification = 0x0020,

    // Risk subsystem messages (0x1000 - 0x1FFF)
    // Currency messages (0x1001 - 0x1008)
    get_currencies_request = 0x1001,
    get_currencies_response = 0x1002,
    save_currency_request = 0x1003,
    save_currency_response = 0x1004,
    delete_currency_request = 0x1005,
    delete_currency_response = 0x1006,
    get_currency_history_request = 0x1007,
    get_currency_history_response = 0x1008,

    // Country messages (0x1009 - 0x1010)
    get_countries_request = 0x1009,
    get_countries_response = 0x100A,
    save_country_request = 0x100B,
    save_country_response = 0x100C,
    delete_country_request = 0x100D,
    delete_country_response = 0x100E,
    get_country_history_request = 0x100F,
    get_country_history_response = 0x1010,

    // Accounts subsystem messages (0x2000 - 0x2FFF)
    // Note: 0x2001-0x2002 deprecated in v21.0 (create_account merged into save_account)
    get_accounts_request = 0x2003,  // Renamed from list_accounts in v21.0
    get_accounts_response = 0x2004,
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
    save_account_request = 0x2015,  // Merged create/update in v21.0
    save_account_response = 0x2016,
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
    get_feature_flags_request = 0x3000,  // Renamed from list_feature_flags in v21.0
    get_feature_flags_response = 0x3001,
    save_feature_flag_request = 0x3002,
    save_feature_flag_response = 0x3003,
    delete_feature_flag_request = 0x3004,
    delete_feature_flag_response = 0x3005,
    get_feature_flag_history_request = 0x3006,
    get_feature_flag_history_response = 0x3007,

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

    // Data Quality subsystem messages (0x6000 - 0x6FFF)
    // Data Organization (catalog, data_domain, subject_area) (0x6000 - 0x601F)
    get_catalogs_request = 0x6000,
    get_catalogs_response = 0x6001,
    save_catalog_request = 0x6002,
    save_catalog_response = 0x6003,
    delete_catalog_request = 0x6004,
    delete_catalog_response = 0x6005,
    get_catalog_history_request = 0x6006,
    get_catalog_history_response = 0x6007,

    get_data_domains_request = 0x6008,
    get_data_domains_response = 0x6009,
    save_data_domain_request = 0x600A,
    save_data_domain_response = 0x600B,
    delete_data_domain_request = 0x600C,
    delete_data_domain_response = 0x600D,
    get_data_domain_history_request = 0x600E,
    get_data_domain_history_response = 0x600F,

    get_subject_areas_request = 0x6010,
    get_subject_areas_response = 0x6011,
    get_subject_areas_by_domain_request = 0x6012,
    get_subject_areas_by_domain_response = 0x6013,
    save_subject_area_request = 0x6014,
    save_subject_area_response = 0x6015,
    delete_subject_area_request = 0x6016,
    delete_subject_area_response = 0x6017,
    get_subject_area_history_request = 0x6018,
    get_subject_area_history_response = 0x6019,

    // Dataset and Methodology (0x6020 - 0x602F)
    get_datasets_request = 0x6020,
    get_datasets_response = 0x6021,
    save_dataset_request = 0x6022,
    save_dataset_response = 0x6023,
    delete_dataset_request = 0x6024,
    delete_dataset_response = 0x6025,
    get_dataset_history_request = 0x6026,
    get_dataset_history_response = 0x6027,

    get_methodologies_request = 0x6028,
    get_methodologies_response = 0x6029,
    save_methodology_request = 0x602A,
    save_methodology_response = 0x602B,
    delete_methodology_request = 0x602C,
    delete_methodology_response = 0x602D,
    get_methodology_history_request = 0x602E,
    get_methodology_history_response = 0x602F,

    // Coding Scheme (0x6030 - 0x6047)
    get_coding_schemes_request = 0x6030,
    get_coding_schemes_response = 0x6031,
    get_coding_schemes_by_authority_type_request = 0x6032,
    get_coding_schemes_by_authority_type_response = 0x6033,
    save_coding_scheme_request = 0x6034,
    save_coding_scheme_response = 0x6035,
    delete_coding_scheme_request = 0x6036,
    delete_coding_scheme_response = 0x6037,
    get_coding_scheme_history_request = 0x6038,
    get_coding_scheme_history_response = 0x6039,

    get_coding_scheme_authority_types_request = 0x603A,
    get_coding_scheme_authority_types_response = 0x603B,
    save_coding_scheme_authority_type_request = 0x603C,
    save_coding_scheme_authority_type_response = 0x603D,
    delete_coding_scheme_authority_type_request = 0x603E,
    delete_coding_scheme_authority_type_response = 0x603F,
    get_coding_scheme_authority_type_history_request = 0x6040,
    get_coding_scheme_authority_type_history_response = 0x6041,

    // Dimensions (nature, origin, treatment) (0x6050 - 0x606F)
    get_nature_dimensions_request = 0x6050,
    get_nature_dimensions_response = 0x6051,
    save_nature_dimension_request = 0x6052,
    save_nature_dimension_response = 0x6053,
    delete_nature_dimension_request = 0x6054,
    delete_nature_dimension_response = 0x6055,
    get_nature_dimension_history_request = 0x6056,
    get_nature_dimension_history_response = 0x6057,

    get_origin_dimensions_request = 0x6058,
    get_origin_dimensions_response = 0x6059,
    save_origin_dimension_request = 0x605A,
    save_origin_dimension_response = 0x605B,
    delete_origin_dimension_request = 0x605C,
    delete_origin_dimension_response = 0x605D,
    get_origin_dimension_history_request = 0x605E,
    get_origin_dimension_history_response = 0x605F,

    get_treatment_dimensions_request = 0x6060,
    get_treatment_dimensions_response = 0x6061,
    save_treatment_dimension_request = 0x6062,
    save_treatment_dimension_response = 0x6063,
    delete_treatment_dimension_request = 0x6064,
    delete_treatment_dimension_response = 0x6065,
    get_treatment_dimension_history_request = 0x6066,
    get_treatment_dimension_history_response = 0x6067,

    // Change Management (change_reason, change_reason_category) (0x6070 - 0x6081)
    // Moved from IAM subsystem (0x2050-0x2061) in v22.0
    get_change_reason_categories_request = 0x6070,
    get_change_reason_categories_response = 0x6071,
    get_change_reasons_request = 0x6072,
    get_change_reasons_response = 0x6073,
    get_change_reasons_by_category_request = 0x6074,
    get_change_reasons_by_category_response = 0x6075,
    save_change_reason_request = 0x6076,
    save_change_reason_response = 0x6077,
    delete_change_reason_request = 0x6078,
    delete_change_reason_response = 0x6079,
    get_change_reason_history_request = 0x607A,
    get_change_reason_history_response = 0x607B,
    save_change_reason_category_request = 0x607C,
    save_change_reason_category_response = 0x607D,
    delete_change_reason_category_request = 0x607E,
    delete_change_reason_category_response = 0x607F,
    get_change_reason_category_history_request = 0x6080,
    get_change_reason_category_history_response = 0x6081,

    last_value
};

/**
 * @brief Stream output operator for message_type.
 *
 * Uses magic_enum to provide human-readable enum names with hex values in logs.
 * Example: "get_currencies_request (0x1001)" instead of just "0x1001"
 * If magic_enum cannot resolve the name, shows "[unknown]" prefix.
 */
inline std::ostream& operator<<(std::ostream& os, message_type mt) {
    auto name = magic_enum::enum_name(mt);
    if (name.empty()) {
        os << "[unknown]";
    } else {
        os << name;
    }
    return os << " (0x" << std::hex << static_cast<std::uint16_t>(mt) << std::dec << ")";
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
