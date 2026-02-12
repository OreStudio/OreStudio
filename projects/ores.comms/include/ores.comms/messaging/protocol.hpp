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
#ifndef ORES_COMMS_MESSAGING_PROTOCOL_HPP
#define ORES_COMMS_MESSAGING_PROTOCOL_HPP

#include <cstdint>

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
//
// Version 22.1 adds dataset dependency messages for querying dependencies
// between datasets using stable dataset codes. New messages:
// get_dataset_dependencies_request/response,
// get_dataset_dependencies_by_dataset_request/response.
// Replaces catalog dependency messages.
//
// Version 22.2 adds artefact_type field to dataset serialization. This field
// indicates which population function to call when publishing the dataset.
// Valid values: "images", "countries", "currencies", "ip2country".
//
// Version 22.3 adds publish_datasets_request/response messages (0x6090-0x6091)
// for publishing datasets from artefact tables to production tables.
//
// Version 22.4 adds get_publications_request/response messages (0x6092-0x6093)
// for querying publication history.
//
// Version 22.5 adds resolve_dependencies_request/response messages (0x6094-0x6095)
// for client-side dependency resolution before publishing. This allows clients
// to preview the dependency order without actually publishing.
//
// Version 22.6 adds dataset bundle messages for managing named collections of
// datasets. New messages: get_dataset_bundles_request/response (0x60A0-0x60A1),
// save_dataset_bundle_request/response (0x60A2-0x60A3),
// delete_dataset_bundle_request/response (0x60A4-0x60A5),
// get_dataset_bundle_history_request/response (0x60A6-0x60A7).
// Also adds dataset bundle member messages for managing bundle membership:
// get_dataset_bundle_members_request/response (0x60B0-0x60B1),
// get_dataset_bundle_members_by_bundle_request/response (0x60B2-0x60B3),
// save_dataset_bundle_member_request/response (0x60B4-0x60B5),
// delete_dataset_bundle_member_request/response (0x60B6-0x60B7).
//
// Version 23.0 adds publish_bundle_request/response (0x60B8-0x60B9) for atomic
// transactional publishing of dataset bundles. Also adds atomic field to
// publish_datasets_request for atomic individual dataset publishing. This is
// a breaking change due to the new field in publish_datasets_request.
//
// Version 24.0 adds available_bundles field to bootstrap_status_response to
// provide bundle choices during system provisioning without requiring auth.
// This is a breaking change due to the new field in bootstrap_status_response.
//
// Version 25.0 adds multi-tenancy support. New messages for tenant management:
// get_tenants_request/response, save_tenant_request/response,
// delete_tenant_request/response, get_tenant_history_request/response (0x2050-0x2057).
// Tenant type messages (0x2058-0x205F) and tenant status messages (0x2060-0x2067).
// Also adds hostname field to login_request for tenant identification during
// authentication. This is a breaking change due to the new field in login_request.
//
// Version 26.1 adds include_deleted optional field to get_tenants_request.
// When true, returns the latest version of all tenants including soft-deleted.
// Backward compatible: old clients sending empty payload get default (false).
//
// Version 26.2 adds assign_role_by_name_request/response and
// revoke_role_by_name_request/response for name-based role assignment.
// Allows using principal (username@hostname) and role name instead of UUIDs.
//
// Version 27.0 makes business_center_code optional in party protocol.
// Wire format changes from bare write_string to write_bool flag + conditional
// write_string. Adds account_type field to get_accounts_response serialization.
// Adds provision_tenant_request/response for tenant onboarding with LEI party
// population and admin account creation.
//
// Version 27.1 adds pagination support to get_parties_request (offset, limit)
// and get_parties_response (total_available_count). Backward compatible: empty
// payload from old clients returns defaults (offset=0, limit=100).
//
// Version 28.0 removes root_lei/lei_dataset_size from
// provision_tenant_request and parties_created from provision_tenant_response.
// Tenant provisioning now creates evaluation-only tenants (tenant record,
// system party, admin account). LEI party import deferred to Phase 2.
//
// Version 29.0 adds optional flag to dataset_bundle_member serialization.
// Optional bundle members require explicit opt-in during publication and
// are skipped unless selected by the user in the publish bundle wizard.
//
// Version 30.0 makes business_center_code mandatory (non-optional) for parties.
// Previously serialized as optional (bool + conditional string), now always
// present as a plain string. Defaults to 'WRLD' for unmapped countries.
constexpr std::uint16_t PROTOCOL_VERSION_MAJOR = 30;
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

}

#endif
