/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_COMMS_MESSAGING_MESSAGE_TYPE_HPP
#define ORES_COMMS_MESSAGING_MESSAGE_TYPE_HPP

#include <cstdint>
#include <ostream>
#include <string_view>

namespace ores::comms::messaging {

/**
 * @brief Protocol message type identifiers.
 *
 * Identifies the type of message being transmitted over the wire protocol.
 * Values are organized by subsystem with non-contiguous ranges to allow
 * future expansion within each subsystem.
 */
enum class message_type : std::uint16_t {
    // Core protocol
    handshake_request = 0x0001,
    handshake_response = 0x0002,
    handshake_ack = 0x0003,
    error_response = 0x0004,
    ping = 0x0005,
    pong = 0x0006,
    // Subscription/Notification
    subscribe_request = 0x0010,
    subscribe_response = 0x0011,
    unsubscribe_request = 0x0012,
    unsubscribe_response = 0x0013,
    notification = 0x0014,
    list_event_channels_request = 0x0015,
    list_event_channels_response = 0x0016,
    // System status
    database_status_notification = 0x0020,
    // Risk subsystem - Currency
    get_currencies_request = 0x1001,
    get_currencies_response = 0x1002,
    save_currency_request = 0x1003,
    save_currency_response = 0x1004,
    delete_currency_request = 0x1005,
    delete_currency_response = 0x1006,
    get_currency_history_request = 0x1007,
    get_currency_history_response = 0x1008,
    // Risk subsystem - Country
    get_countries_request = 0x1009,
    get_countries_response = 0x100A,
    save_country_request = 0x100B,
    save_country_response = 0x100C,
    delete_country_request = 0x100D,
    delete_country_response = 0x100E,
    get_country_history_request = 0x100F,
    get_country_history_response = 0x1010,
    // Refdata subsystem - Party types
    get_party_types_request = 0x1011,
    get_party_types_response = 0x1012,
    save_party_type_request = 0x1013,
    save_party_type_response = 0x1014,
    delete_party_type_request = 0x1015,
    delete_party_type_response = 0x1016,
    get_party_type_history_request = 0x1017,
    get_party_type_history_response = 0x1018,
    // Refdata subsystem - Party statuses
    get_party_statuses_request = 0x1019,
    get_party_statuses_response = 0x101A,
    save_party_status_request = 0x101B,
    save_party_status_response = 0x101C,
    delete_party_status_request = 0x101D,
    delete_party_status_response = 0x101E,
    get_party_status_history_request = 0x101F,
    get_party_status_history_response = 0x1020,
    // Refdata subsystem - Party ID schemes
    get_party_id_schemes_request = 0x1021,
    get_party_id_schemes_response = 0x1022,
    save_party_id_scheme_request = 0x1023,
    save_party_id_scheme_response = 0x1024,
    delete_party_id_scheme_request = 0x1025,
    delete_party_id_scheme_response = 0x1026,
    get_party_id_scheme_history_request = 0x1027,
    get_party_id_scheme_history_response = 0x1028,
    // Refdata subsystem - Contact types
    get_contact_types_request = 0x1029,
    get_contact_types_response = 0x102A,
    save_contact_type_request = 0x102B,
    save_contact_type_response = 0x102C,
    delete_contact_type_request = 0x102D,
    delete_contact_type_response = 0x102E,
    get_contact_type_history_request = 0x102F,
    get_contact_type_history_response = 0x1030,
    // Refdata subsystem - Parties
    get_parties_request = 0x1031,
    get_parties_response = 0x1032,
    save_party_request = 0x1033,
    save_party_response = 0x1034,
    delete_party_request = 0x1035,
    delete_party_response = 0x1036,
    get_party_history_request = 0x1037,
    get_party_history_response = 0x1038,
    // Refdata subsystem - Counterparties
    get_counterparties_request = 0x1039,
    get_counterparties_response = 0x103A,
    save_counterparty_request = 0x103B,
    save_counterparty_response = 0x103C,
    delete_counterparty_request = 0x103D,
    delete_counterparty_response = 0x103E,
    get_counterparty_history_request = 0x103F,
    get_counterparty_history_response = 0x1040,
    // Refdata subsystem - Party identifiers
    get_party_identifiers_request = 0x1041,
    get_party_identifiers_response = 0x1042,
    save_party_identifier_request = 0x1043,
    save_party_identifier_response = 0x1044,
    delete_party_identifier_request = 0x1045,
    delete_party_identifier_response = 0x1046,
    get_party_identifier_history_request = 0x1047,
    get_party_identifier_history_response = 0x1048,
    // Refdata subsystem - Counterparty identifiers
    get_counterparty_identifiers_request = 0x1049,
    get_counterparty_identifiers_response = 0x104A,
    save_counterparty_identifier_request = 0x104B,
    save_counterparty_identifier_response = 0x104C,
    delete_counterparty_identifier_request = 0x104D,
    delete_counterparty_identifier_response = 0x104E,
    get_counterparty_identifier_history_request = 0x104F,
    get_counterparty_identifier_history_response = 0x1050,
    // Refdata subsystem - Party contact informations
    get_party_contact_informations_request = 0x1051,
    get_party_contact_informations_response = 0x1052,
    save_party_contact_information_request = 0x1053,
    save_party_contact_information_response = 0x1054,
    delete_party_contact_information_request = 0x1055,
    delete_party_contact_information_response = 0x1056,
    get_party_contact_information_history_request = 0x1057,
    get_party_contact_information_history_response = 0x1058,
    // Refdata subsystem - Counterparty contact informations
    get_counterparty_contact_informations_request = 0x1059,
    get_counterparty_contact_informations_response = 0x105A,
    save_counterparty_contact_information_request = 0x105B,
    save_counterparty_contact_information_response = 0x105C,
    delete_counterparty_contact_information_request = 0x105D,
    delete_counterparty_contact_information_response = 0x105E,
    get_counterparty_contact_information_history_request = 0x105F,
    get_counterparty_contact_information_history_response = 0x1060,
    // IAM subsystem - Accounts
    get_accounts_request = 0x2003,
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
    save_account_request = 0x2015,
    save_account_response = 0x2016,
    get_account_history_request = 0x2017,
    get_account_history_response = 0x2018,
    reset_password_request = 0x2019,
    reset_password_response = 0x201A,
    change_password_request = 0x201B,
    change_password_response = 0x201C,
    update_my_email_request = 0x201D,
    update_my_email_response = 0x201E,
    // IAM subsystem - RBAC
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
    suggest_role_commands_request = 0x202E,
    suggest_role_commands_response = 0x202F,
    // IAM subsystem - RBAC (name-based)
    assign_role_by_name_request = 0x2032,
    assign_role_by_name_response = 0x2033,
    revoke_role_by_name_request = 0x2034,
    revoke_role_by_name_response = 0x2035,
    // IAM subsystem - Signup
    signup_request = 0x2030,
    signup_response = 0x2031,
    // IAM subsystem - Sessions
    list_sessions_request = 0x2040,
    list_sessions_response = 0x2041,
    get_session_statistics_request = 0x2042,
    get_session_statistics_response = 0x2043,
    get_active_sessions_request = 0x2044,
    get_active_sessions_response = 0x2045,
    // IAM subsystem - Tenants
    get_tenants_request = 0x2050,
    get_tenants_response = 0x2051,
    save_tenant_request = 0x2052,
    save_tenant_response = 0x2053,
    delete_tenant_request = 0x2054,
    delete_tenant_response = 0x2055,
    get_tenant_history_request = 0x2056,
    get_tenant_history_response = 0x2057,
    // IAM subsystem - Tenant types
    get_tenant_types_request = 0x2058,
    get_tenant_types_response = 0x2059,
    save_tenant_type_request = 0x205A,
    save_tenant_type_response = 0x205B,
    delete_tenant_type_request = 0x205C,
    delete_tenant_type_response = 0x205D,
    get_tenant_type_history_request = 0x205E,
    get_tenant_type_history_response = 0x205F,
    // IAM subsystem - Tenant statuses
    get_tenant_statuses_request = 0x2060,
    get_tenant_statuses_response = 0x2061,
    save_tenant_status_request = 0x2062,
    save_tenant_status_response = 0x2063,
    delete_tenant_status_request = 0x2064,
    delete_tenant_status_response = 0x2065,
    get_tenant_status_history_request = 0x2066,
    get_tenant_status_history_response = 0x2067,
    // Variability subsystem
    get_feature_flags_request = 0x3000,
    get_feature_flags_response = 0x3001,
    save_feature_flag_request = 0x3002,
    save_feature_flag_response = 0x3003,
    delete_feature_flag_request = 0x3004,
    delete_feature_flag_response = 0x3005,
    get_feature_flag_history_request = 0x3006,
    get_feature_flag_history_response = 0x3007,
    // Assets subsystem
    get_images_request = 0x4002,
    get_images_response = 0x4003,
    list_images_request = 0x4004,
    list_images_response = 0x4005,
    // Telemetry subsystem
    submit_log_records_request = 0x5000,
    submit_telemetry_response = 0x5001,
    get_telemetry_logs_request = 0x5010,
    get_telemetry_logs_response = 0x5011,
    get_telemetry_stats_request = 0x5020,
    get_telemetry_stats_response = 0x5021,
    // DQ subsystem - Catalogs
    get_catalogs_request = 0x6000,
    get_catalogs_response = 0x6001,
    save_catalog_request = 0x6002,
    save_catalog_response = 0x6003,
    delete_catalog_request = 0x6004,
    delete_catalog_response = 0x6005,
    get_catalog_history_request = 0x6006,
    get_catalog_history_response = 0x6007,
    // DQ subsystem - Data domains
    get_data_domains_request = 0x6008,
    get_data_domains_response = 0x6009,
    save_data_domain_request = 0x600A,
    save_data_domain_response = 0x600B,
    delete_data_domain_request = 0x600C,
    delete_data_domain_response = 0x600D,
    get_data_domain_history_request = 0x600E,
    get_data_domain_history_response = 0x600F,
    // DQ subsystem - Subject areas
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
    // DQ subsystem - Datasets
    get_datasets_request = 0x6020,
    get_datasets_response = 0x6021,
    save_dataset_request = 0x6022,
    save_dataset_response = 0x6023,
    delete_dataset_request = 0x6024,
    delete_dataset_response = 0x6025,
    get_dataset_history_request = 0x6026,
    get_dataset_history_response = 0x6027,
    // DQ subsystem - Methodologies
    get_methodologies_request = 0x6028,
    get_methodologies_response = 0x6029,
    save_methodology_request = 0x602A,
    save_methodology_response = 0x602B,
    delete_methodology_request = 0x602C,
    delete_methodology_response = 0x602D,
    get_methodology_history_request = 0x602E,
    get_methodology_history_response = 0x602F,
    // DQ subsystem - Coding schemes
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
    // DQ subsystem - Authority types
    get_coding_scheme_authority_types_request = 0x603A,
    get_coding_scheme_authority_types_response = 0x603B,
    save_coding_scheme_authority_type_request = 0x603C,
    save_coding_scheme_authority_type_response = 0x603D,
    delete_coding_scheme_authority_type_request = 0x603E,
    delete_coding_scheme_authority_type_response = 0x603F,
    get_coding_scheme_authority_type_history_request = 0x6040,
    get_coding_scheme_authority_type_history_response = 0x6041,
    // DQ subsystem - Nature dimensions
    get_nature_dimensions_request = 0x6050,
    get_nature_dimensions_response = 0x6051,
    save_nature_dimension_request = 0x6052,
    save_nature_dimension_response = 0x6053,
    delete_nature_dimension_request = 0x6054,
    delete_nature_dimension_response = 0x6055,
    get_nature_dimension_history_request = 0x6056,
    get_nature_dimension_history_response = 0x6057,
    // DQ subsystem - Origin dimensions
    get_origin_dimensions_request = 0x6058,
    get_origin_dimensions_response = 0x6059,
    save_origin_dimension_request = 0x605A,
    save_origin_dimension_response = 0x605B,
    delete_origin_dimension_request = 0x605C,
    delete_origin_dimension_response = 0x605D,
    get_origin_dimension_history_request = 0x605E,
    get_origin_dimension_history_response = 0x605F,
    // DQ subsystem - Treatment dimensions
    get_treatment_dimensions_request = 0x6060,
    get_treatment_dimensions_response = 0x6061,
    save_treatment_dimension_request = 0x6062,
    save_treatment_dimension_response = 0x6063,
    delete_treatment_dimension_request = 0x6064,
    delete_treatment_dimension_response = 0x6065,
    get_treatment_dimension_history_request = 0x6066,
    get_treatment_dimension_history_response = 0x6067,
    // DQ subsystem - Change reasons
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
    // DQ subsystem - Dependencies
    get_dataset_dependencies_request = 0x6082,
    get_dataset_dependencies_response = 0x6083,
    get_dataset_dependencies_by_dataset_request = 0x6084,
    get_dataset_dependencies_by_dataset_response = 0x6085,
    // DQ subsystem - Publication
    publish_datasets_request = 0x6090,
    publish_datasets_response = 0x6091,
    get_publications_request = 0x6092,
    get_publications_response = 0x6093,
    resolve_dependencies_request = 0x6094,
    resolve_dependencies_response = 0x6095,
    // DQ subsystem - Bundles
    get_dataset_bundles_request = 0x60A0,
    get_dataset_bundles_response = 0x60A1,
    save_dataset_bundle_request = 0x60A2,
    save_dataset_bundle_response = 0x60A3,
    delete_dataset_bundle_request = 0x60A4,
    delete_dataset_bundle_response = 0x60A5,
    get_dataset_bundle_history_request = 0x60A6,
    get_dataset_bundle_history_response = 0x60A7,
    // DQ subsystem - Bundle members
    get_dataset_bundle_members_request = 0x60B0,
    get_dataset_bundle_members_response = 0x60B1,
    get_dataset_bundle_members_by_bundle_request = 0x60B2,
    get_dataset_bundle_members_by_bundle_response = 0x60B3,
    save_dataset_bundle_member_request = 0x60B4,
    save_dataset_bundle_member_response = 0x60B5,
    delete_dataset_bundle_member_request = 0x60B6,
    delete_dataset_bundle_member_response = 0x60B7,
    // DQ subsystem - Bundle publication
    publish_bundle_request = 0x60B8,
    publish_bundle_response = 0x60B9,
    // Sentinel value
    last_value
};

/**
 * @brief Convert message_type to string representation.
 *
 * @param v The enum value to convert.
 * @return String view of the enum name, or empty for unknown values.
 */
[[nodiscard]] constexpr std::string_view to_string(message_type v) noexcept {
    switch (v) {
    case message_type::handshake_request: return "handshake_request";
    case message_type::handshake_response: return "handshake_response";
    case message_type::handshake_ack: return "handshake_ack";
    case message_type::error_response: return "error_response";
    case message_type::ping: return "ping";
    case message_type::pong: return "pong";
    case message_type::subscribe_request: return "subscribe_request";
    case message_type::subscribe_response: return "subscribe_response";
    case message_type::unsubscribe_request: return "unsubscribe_request";
    case message_type::unsubscribe_response: return "unsubscribe_response";
    case message_type::notification: return "notification";
    case message_type::list_event_channels_request: return "list_event_channels_request";
    case message_type::list_event_channels_response: return "list_event_channels_response";
    case message_type::database_status_notification: return "database_status_notification";
    case message_type::get_currencies_request: return "get_currencies_request";
    case message_type::get_currencies_response: return "get_currencies_response";
    case message_type::save_currency_request: return "save_currency_request";
    case message_type::save_currency_response: return "save_currency_response";
    case message_type::delete_currency_request: return "delete_currency_request";
    case message_type::delete_currency_response: return "delete_currency_response";
    case message_type::get_currency_history_request: return "get_currency_history_request";
    case message_type::get_currency_history_response: return "get_currency_history_response";
    case message_type::get_countries_request: return "get_countries_request";
    case message_type::get_countries_response: return "get_countries_response";
    case message_type::save_country_request: return "save_country_request";
    case message_type::save_country_response: return "save_country_response";
    case message_type::delete_country_request: return "delete_country_request";
    case message_type::delete_country_response: return "delete_country_response";
    case message_type::get_country_history_request: return "get_country_history_request";
    case message_type::get_country_history_response: return "get_country_history_response";
    case message_type::get_party_types_request: return "get_party_types_request";
    case message_type::get_party_types_response: return "get_party_types_response";
    case message_type::save_party_type_request: return "save_party_type_request";
    case message_type::save_party_type_response: return "save_party_type_response";
    case message_type::delete_party_type_request: return "delete_party_type_request";
    case message_type::delete_party_type_response: return "delete_party_type_response";
    case message_type::get_party_type_history_request: return "get_party_type_history_request";
    case message_type::get_party_type_history_response: return "get_party_type_history_response";
    case message_type::get_party_statuses_request: return "get_party_statuses_request";
    case message_type::get_party_statuses_response: return "get_party_statuses_response";
    case message_type::save_party_status_request: return "save_party_status_request";
    case message_type::save_party_status_response: return "save_party_status_response";
    case message_type::delete_party_status_request: return "delete_party_status_request";
    case message_type::delete_party_status_response: return "delete_party_status_response";
    case message_type::get_party_status_history_request: return "get_party_status_history_request";
    case message_type::get_party_status_history_response: return "get_party_status_history_response";
    case message_type::get_party_id_schemes_request: return "get_party_id_schemes_request";
    case message_type::get_party_id_schemes_response: return "get_party_id_schemes_response";
    case message_type::save_party_id_scheme_request: return "save_party_id_scheme_request";
    case message_type::save_party_id_scheme_response: return "save_party_id_scheme_response";
    case message_type::delete_party_id_scheme_request: return "delete_party_id_scheme_request";
    case message_type::delete_party_id_scheme_response: return "delete_party_id_scheme_response";
    case message_type::get_party_id_scheme_history_request: return "get_party_id_scheme_history_request";
    case message_type::get_party_id_scheme_history_response: return "get_party_id_scheme_history_response";
    case message_type::get_contact_types_request: return "get_contact_types_request";
    case message_type::get_contact_types_response: return "get_contact_types_response";
    case message_type::save_contact_type_request: return "save_contact_type_request";
    case message_type::save_contact_type_response: return "save_contact_type_response";
    case message_type::delete_contact_type_request: return "delete_contact_type_request";
    case message_type::delete_contact_type_response: return "delete_contact_type_response";
    case message_type::get_contact_type_history_request: return "get_contact_type_history_request";
    case message_type::get_contact_type_history_response: return "get_contact_type_history_response";
    case message_type::get_parties_request: return "get_parties_request";
    case message_type::get_parties_response: return "get_parties_response";
    case message_type::save_party_request: return "save_party_request";
    case message_type::save_party_response: return "save_party_response";
    case message_type::delete_party_request: return "delete_party_request";
    case message_type::delete_party_response: return "delete_party_response";
    case message_type::get_party_history_request: return "get_party_history_request";
    case message_type::get_party_history_response: return "get_party_history_response";
    case message_type::get_counterparties_request: return "get_counterparties_request";
    case message_type::get_counterparties_response: return "get_counterparties_response";
    case message_type::save_counterparty_request: return "save_counterparty_request";
    case message_type::save_counterparty_response: return "save_counterparty_response";
    case message_type::delete_counterparty_request: return "delete_counterparty_request";
    case message_type::delete_counterparty_response: return "delete_counterparty_response";
    case message_type::get_counterparty_history_request: return "get_counterparty_history_request";
    case message_type::get_counterparty_history_response: return "get_counterparty_history_response";
    case message_type::get_party_identifiers_request: return "get_party_identifiers_request";
    case message_type::get_party_identifiers_response: return "get_party_identifiers_response";
    case message_type::save_party_identifier_request: return "save_party_identifier_request";
    case message_type::save_party_identifier_response: return "save_party_identifier_response";
    case message_type::delete_party_identifier_request: return "delete_party_identifier_request";
    case message_type::delete_party_identifier_response: return "delete_party_identifier_response";
    case message_type::get_party_identifier_history_request: return "get_party_identifier_history_request";
    case message_type::get_party_identifier_history_response: return "get_party_identifier_history_response";
    case message_type::get_counterparty_identifiers_request: return "get_counterparty_identifiers_request";
    case message_type::get_counterparty_identifiers_response: return "get_counterparty_identifiers_response";
    case message_type::save_counterparty_identifier_request: return "save_counterparty_identifier_request";
    case message_type::save_counterparty_identifier_response: return "save_counterparty_identifier_response";
    case message_type::delete_counterparty_identifier_request: return "delete_counterparty_identifier_request";
    case message_type::delete_counterparty_identifier_response: return "delete_counterparty_identifier_response";
    case message_type::get_counterparty_identifier_history_request: return "get_counterparty_identifier_history_request";
    case message_type::get_counterparty_identifier_history_response: return "get_counterparty_identifier_history_response";
    case message_type::get_party_contact_informations_request: return "get_party_contact_informations_request";
    case message_type::get_party_contact_informations_response: return "get_party_contact_informations_response";
    case message_type::save_party_contact_information_request: return "save_party_contact_information_request";
    case message_type::save_party_contact_information_response: return "save_party_contact_information_response";
    case message_type::delete_party_contact_information_request: return "delete_party_contact_information_request";
    case message_type::delete_party_contact_information_response: return "delete_party_contact_information_response";
    case message_type::get_party_contact_information_history_request: return "get_party_contact_information_history_request";
    case message_type::get_party_contact_information_history_response: return "get_party_contact_information_history_response";
    case message_type::get_counterparty_contact_informations_request: return "get_counterparty_contact_informations_request";
    case message_type::get_counterparty_contact_informations_response: return "get_counterparty_contact_informations_response";
    case message_type::save_counterparty_contact_information_request: return "save_counterparty_contact_information_request";
    case message_type::save_counterparty_contact_information_response: return "save_counterparty_contact_information_response";
    case message_type::delete_counterparty_contact_information_request: return "delete_counterparty_contact_information_request";
    case message_type::delete_counterparty_contact_information_response: return "delete_counterparty_contact_information_response";
    case message_type::get_counterparty_contact_information_history_request: return "get_counterparty_contact_information_history_request";
    case message_type::get_counterparty_contact_information_history_response: return "get_counterparty_contact_information_history_response";
    case message_type::get_accounts_request: return "get_accounts_request";
    case message_type::get_accounts_response: return "get_accounts_response";
    case message_type::login_request: return "login_request";
    case message_type::login_response: return "login_response";
    case message_type::unlock_account_request: return "unlock_account_request";
    case message_type::unlock_account_response: return "unlock_account_response";
    case message_type::delete_account_request: return "delete_account_request";
    case message_type::delete_account_response: return "delete_account_response";
    case message_type::list_login_info_request: return "list_login_info_request";
    case message_type::list_login_info_response: return "list_login_info_response";
    case message_type::logout_request: return "logout_request";
    case message_type::logout_response: return "logout_response";
    case message_type::create_initial_admin_request: return "create_initial_admin_request";
    case message_type::create_initial_admin_response: return "create_initial_admin_response";
    case message_type::bootstrap_status_request: return "bootstrap_status_request";
    case message_type::bootstrap_status_response: return "bootstrap_status_response";
    case message_type::lock_account_request: return "lock_account_request";
    case message_type::lock_account_response: return "lock_account_response";
    case message_type::save_account_request: return "save_account_request";
    case message_type::save_account_response: return "save_account_response";
    case message_type::get_account_history_request: return "get_account_history_request";
    case message_type::get_account_history_response: return "get_account_history_response";
    case message_type::reset_password_request: return "reset_password_request";
    case message_type::reset_password_response: return "reset_password_response";
    case message_type::change_password_request: return "change_password_request";
    case message_type::change_password_response: return "change_password_response";
    case message_type::update_my_email_request: return "update_my_email_request";
    case message_type::update_my_email_response: return "update_my_email_response";
    case message_type::list_roles_request: return "list_roles_request";
    case message_type::list_roles_response: return "list_roles_response";
    case message_type::list_permissions_request: return "list_permissions_request";
    case message_type::list_permissions_response: return "list_permissions_response";
    case message_type::assign_role_request: return "assign_role_request";
    case message_type::assign_role_response: return "assign_role_response";
    case message_type::revoke_role_request: return "revoke_role_request";
    case message_type::revoke_role_response: return "revoke_role_response";
    case message_type::get_account_roles_request: return "get_account_roles_request";
    case message_type::get_account_roles_response: return "get_account_roles_response";
    case message_type::get_account_permissions_request: return "get_account_permissions_request";
    case message_type::get_account_permissions_response: return "get_account_permissions_response";
    case message_type::get_role_request: return "get_role_request";
    case message_type::get_role_response: return "get_role_response";
    case message_type::suggest_role_commands_request: return "suggest_role_commands_request";
    case message_type::suggest_role_commands_response: return "suggest_role_commands_response";
    case message_type::assign_role_by_name_request: return "assign_role_by_name_request";
    case message_type::assign_role_by_name_response: return "assign_role_by_name_response";
    case message_type::revoke_role_by_name_request: return "revoke_role_by_name_request";
    case message_type::revoke_role_by_name_response: return "revoke_role_by_name_response";
    case message_type::signup_request: return "signup_request";
    case message_type::signup_response: return "signup_response";
    case message_type::list_sessions_request: return "list_sessions_request";
    case message_type::list_sessions_response: return "list_sessions_response";
    case message_type::get_session_statistics_request: return "get_session_statistics_request";
    case message_type::get_session_statistics_response: return "get_session_statistics_response";
    case message_type::get_active_sessions_request: return "get_active_sessions_request";
    case message_type::get_active_sessions_response: return "get_active_sessions_response";
    case message_type::get_tenants_request: return "get_tenants_request";
    case message_type::get_tenants_response: return "get_tenants_response";
    case message_type::save_tenant_request: return "save_tenant_request";
    case message_type::save_tenant_response: return "save_tenant_response";
    case message_type::delete_tenant_request: return "delete_tenant_request";
    case message_type::delete_tenant_response: return "delete_tenant_response";
    case message_type::get_tenant_history_request: return "get_tenant_history_request";
    case message_type::get_tenant_history_response: return "get_tenant_history_response";
    case message_type::get_tenant_types_request: return "get_tenant_types_request";
    case message_type::get_tenant_types_response: return "get_tenant_types_response";
    case message_type::save_tenant_type_request: return "save_tenant_type_request";
    case message_type::save_tenant_type_response: return "save_tenant_type_response";
    case message_type::delete_tenant_type_request: return "delete_tenant_type_request";
    case message_type::delete_tenant_type_response: return "delete_tenant_type_response";
    case message_type::get_tenant_type_history_request: return "get_tenant_type_history_request";
    case message_type::get_tenant_type_history_response: return "get_tenant_type_history_response";
    case message_type::get_tenant_statuses_request: return "get_tenant_statuses_request";
    case message_type::get_tenant_statuses_response: return "get_tenant_statuses_response";
    case message_type::save_tenant_status_request: return "save_tenant_status_request";
    case message_type::save_tenant_status_response: return "save_tenant_status_response";
    case message_type::delete_tenant_status_request: return "delete_tenant_status_request";
    case message_type::delete_tenant_status_response: return "delete_tenant_status_response";
    case message_type::get_tenant_status_history_request: return "get_tenant_status_history_request";
    case message_type::get_tenant_status_history_response: return "get_tenant_status_history_response";
    case message_type::get_feature_flags_request: return "get_feature_flags_request";
    case message_type::get_feature_flags_response: return "get_feature_flags_response";
    case message_type::save_feature_flag_request: return "save_feature_flag_request";
    case message_type::save_feature_flag_response: return "save_feature_flag_response";
    case message_type::delete_feature_flag_request: return "delete_feature_flag_request";
    case message_type::delete_feature_flag_response: return "delete_feature_flag_response";
    case message_type::get_feature_flag_history_request: return "get_feature_flag_history_request";
    case message_type::get_feature_flag_history_response: return "get_feature_flag_history_response";
    case message_type::get_images_request: return "get_images_request";
    case message_type::get_images_response: return "get_images_response";
    case message_type::list_images_request: return "list_images_request";
    case message_type::list_images_response: return "list_images_response";
    case message_type::submit_log_records_request: return "submit_log_records_request";
    case message_type::submit_telemetry_response: return "submit_telemetry_response";
    case message_type::get_telemetry_logs_request: return "get_telemetry_logs_request";
    case message_type::get_telemetry_logs_response: return "get_telemetry_logs_response";
    case message_type::get_telemetry_stats_request: return "get_telemetry_stats_request";
    case message_type::get_telemetry_stats_response: return "get_telemetry_stats_response";
    case message_type::get_catalogs_request: return "get_catalogs_request";
    case message_type::get_catalogs_response: return "get_catalogs_response";
    case message_type::save_catalog_request: return "save_catalog_request";
    case message_type::save_catalog_response: return "save_catalog_response";
    case message_type::delete_catalog_request: return "delete_catalog_request";
    case message_type::delete_catalog_response: return "delete_catalog_response";
    case message_type::get_catalog_history_request: return "get_catalog_history_request";
    case message_type::get_catalog_history_response: return "get_catalog_history_response";
    case message_type::get_data_domains_request: return "get_data_domains_request";
    case message_type::get_data_domains_response: return "get_data_domains_response";
    case message_type::save_data_domain_request: return "save_data_domain_request";
    case message_type::save_data_domain_response: return "save_data_domain_response";
    case message_type::delete_data_domain_request: return "delete_data_domain_request";
    case message_type::delete_data_domain_response: return "delete_data_domain_response";
    case message_type::get_data_domain_history_request: return "get_data_domain_history_request";
    case message_type::get_data_domain_history_response: return "get_data_domain_history_response";
    case message_type::get_subject_areas_request: return "get_subject_areas_request";
    case message_type::get_subject_areas_response: return "get_subject_areas_response";
    case message_type::get_subject_areas_by_domain_request: return "get_subject_areas_by_domain_request";
    case message_type::get_subject_areas_by_domain_response: return "get_subject_areas_by_domain_response";
    case message_type::save_subject_area_request: return "save_subject_area_request";
    case message_type::save_subject_area_response: return "save_subject_area_response";
    case message_type::delete_subject_area_request: return "delete_subject_area_request";
    case message_type::delete_subject_area_response: return "delete_subject_area_response";
    case message_type::get_subject_area_history_request: return "get_subject_area_history_request";
    case message_type::get_subject_area_history_response: return "get_subject_area_history_response";
    case message_type::get_datasets_request: return "get_datasets_request";
    case message_type::get_datasets_response: return "get_datasets_response";
    case message_type::save_dataset_request: return "save_dataset_request";
    case message_type::save_dataset_response: return "save_dataset_response";
    case message_type::delete_dataset_request: return "delete_dataset_request";
    case message_type::delete_dataset_response: return "delete_dataset_response";
    case message_type::get_dataset_history_request: return "get_dataset_history_request";
    case message_type::get_dataset_history_response: return "get_dataset_history_response";
    case message_type::get_methodologies_request: return "get_methodologies_request";
    case message_type::get_methodologies_response: return "get_methodologies_response";
    case message_type::save_methodology_request: return "save_methodology_request";
    case message_type::save_methodology_response: return "save_methodology_response";
    case message_type::delete_methodology_request: return "delete_methodology_request";
    case message_type::delete_methodology_response: return "delete_methodology_response";
    case message_type::get_methodology_history_request: return "get_methodology_history_request";
    case message_type::get_methodology_history_response: return "get_methodology_history_response";
    case message_type::get_coding_schemes_request: return "get_coding_schemes_request";
    case message_type::get_coding_schemes_response: return "get_coding_schemes_response";
    case message_type::get_coding_schemes_by_authority_type_request: return "get_coding_schemes_by_authority_type_request";
    case message_type::get_coding_schemes_by_authority_type_response: return "get_coding_schemes_by_authority_type_response";
    case message_type::save_coding_scheme_request: return "save_coding_scheme_request";
    case message_type::save_coding_scheme_response: return "save_coding_scheme_response";
    case message_type::delete_coding_scheme_request: return "delete_coding_scheme_request";
    case message_type::delete_coding_scheme_response: return "delete_coding_scheme_response";
    case message_type::get_coding_scheme_history_request: return "get_coding_scheme_history_request";
    case message_type::get_coding_scheme_history_response: return "get_coding_scheme_history_response";
    case message_type::get_coding_scheme_authority_types_request: return "get_coding_scheme_authority_types_request";
    case message_type::get_coding_scheme_authority_types_response: return "get_coding_scheme_authority_types_response";
    case message_type::save_coding_scheme_authority_type_request: return "save_coding_scheme_authority_type_request";
    case message_type::save_coding_scheme_authority_type_response: return "save_coding_scheme_authority_type_response";
    case message_type::delete_coding_scheme_authority_type_request: return "delete_coding_scheme_authority_type_request";
    case message_type::delete_coding_scheme_authority_type_response: return "delete_coding_scheme_authority_type_response";
    case message_type::get_coding_scheme_authority_type_history_request: return "get_coding_scheme_authority_type_history_request";
    case message_type::get_coding_scheme_authority_type_history_response: return "get_coding_scheme_authority_type_history_response";
    case message_type::get_nature_dimensions_request: return "get_nature_dimensions_request";
    case message_type::get_nature_dimensions_response: return "get_nature_dimensions_response";
    case message_type::save_nature_dimension_request: return "save_nature_dimension_request";
    case message_type::save_nature_dimension_response: return "save_nature_dimension_response";
    case message_type::delete_nature_dimension_request: return "delete_nature_dimension_request";
    case message_type::delete_nature_dimension_response: return "delete_nature_dimension_response";
    case message_type::get_nature_dimension_history_request: return "get_nature_dimension_history_request";
    case message_type::get_nature_dimension_history_response: return "get_nature_dimension_history_response";
    case message_type::get_origin_dimensions_request: return "get_origin_dimensions_request";
    case message_type::get_origin_dimensions_response: return "get_origin_dimensions_response";
    case message_type::save_origin_dimension_request: return "save_origin_dimension_request";
    case message_type::save_origin_dimension_response: return "save_origin_dimension_response";
    case message_type::delete_origin_dimension_request: return "delete_origin_dimension_request";
    case message_type::delete_origin_dimension_response: return "delete_origin_dimension_response";
    case message_type::get_origin_dimension_history_request: return "get_origin_dimension_history_request";
    case message_type::get_origin_dimension_history_response: return "get_origin_dimension_history_response";
    case message_type::get_treatment_dimensions_request: return "get_treatment_dimensions_request";
    case message_type::get_treatment_dimensions_response: return "get_treatment_dimensions_response";
    case message_type::save_treatment_dimension_request: return "save_treatment_dimension_request";
    case message_type::save_treatment_dimension_response: return "save_treatment_dimension_response";
    case message_type::delete_treatment_dimension_request: return "delete_treatment_dimension_request";
    case message_type::delete_treatment_dimension_response: return "delete_treatment_dimension_response";
    case message_type::get_treatment_dimension_history_request: return "get_treatment_dimension_history_request";
    case message_type::get_treatment_dimension_history_response: return "get_treatment_dimension_history_response";
    case message_type::get_change_reason_categories_request: return "get_change_reason_categories_request";
    case message_type::get_change_reason_categories_response: return "get_change_reason_categories_response";
    case message_type::get_change_reasons_request: return "get_change_reasons_request";
    case message_type::get_change_reasons_response: return "get_change_reasons_response";
    case message_type::get_change_reasons_by_category_request: return "get_change_reasons_by_category_request";
    case message_type::get_change_reasons_by_category_response: return "get_change_reasons_by_category_response";
    case message_type::save_change_reason_request: return "save_change_reason_request";
    case message_type::save_change_reason_response: return "save_change_reason_response";
    case message_type::delete_change_reason_request: return "delete_change_reason_request";
    case message_type::delete_change_reason_response: return "delete_change_reason_response";
    case message_type::get_change_reason_history_request: return "get_change_reason_history_request";
    case message_type::get_change_reason_history_response: return "get_change_reason_history_response";
    case message_type::save_change_reason_category_request: return "save_change_reason_category_request";
    case message_type::save_change_reason_category_response: return "save_change_reason_category_response";
    case message_type::delete_change_reason_category_request: return "delete_change_reason_category_request";
    case message_type::delete_change_reason_category_response: return "delete_change_reason_category_response";
    case message_type::get_change_reason_category_history_request: return "get_change_reason_category_history_request";
    case message_type::get_change_reason_category_history_response: return "get_change_reason_category_history_response";
    case message_type::get_dataset_dependencies_request: return "get_dataset_dependencies_request";
    case message_type::get_dataset_dependencies_response: return "get_dataset_dependencies_response";
    case message_type::get_dataset_dependencies_by_dataset_request: return "get_dataset_dependencies_by_dataset_request";
    case message_type::get_dataset_dependencies_by_dataset_response: return "get_dataset_dependencies_by_dataset_response";
    case message_type::publish_datasets_request: return "publish_datasets_request";
    case message_type::publish_datasets_response: return "publish_datasets_response";
    case message_type::get_publications_request: return "get_publications_request";
    case message_type::get_publications_response: return "get_publications_response";
    case message_type::resolve_dependencies_request: return "resolve_dependencies_request";
    case message_type::resolve_dependencies_response: return "resolve_dependencies_response";
    case message_type::get_dataset_bundles_request: return "get_dataset_bundles_request";
    case message_type::get_dataset_bundles_response: return "get_dataset_bundles_response";
    case message_type::save_dataset_bundle_request: return "save_dataset_bundle_request";
    case message_type::save_dataset_bundle_response: return "save_dataset_bundle_response";
    case message_type::delete_dataset_bundle_request: return "delete_dataset_bundle_request";
    case message_type::delete_dataset_bundle_response: return "delete_dataset_bundle_response";
    case message_type::get_dataset_bundle_history_request: return "get_dataset_bundle_history_request";
    case message_type::get_dataset_bundle_history_response: return "get_dataset_bundle_history_response";
    case message_type::get_dataset_bundle_members_request: return "get_dataset_bundle_members_request";
    case message_type::get_dataset_bundle_members_response: return "get_dataset_bundle_members_response";
    case message_type::get_dataset_bundle_members_by_bundle_request: return "get_dataset_bundle_members_by_bundle_request";
    case message_type::get_dataset_bundle_members_by_bundle_response: return "get_dataset_bundle_members_by_bundle_response";
    case message_type::save_dataset_bundle_member_request: return "save_dataset_bundle_member_request";
    case message_type::save_dataset_bundle_member_response: return "save_dataset_bundle_member_response";
    case message_type::delete_dataset_bundle_member_request: return "delete_dataset_bundle_member_request";
    case message_type::delete_dataset_bundle_member_response: return "delete_dataset_bundle_member_response";
    case message_type::publish_bundle_request: return "publish_bundle_request";
    case message_type::publish_bundle_response: return "publish_bundle_response";
    default: return {};
    }
}

/**
 * @brief Stream output operator for message_type.
 *
 * Outputs the enum name followed by the hex value in parentheses.
 * Example: "get_currencies_request (0x1001)"
 * If the value is unknown, outputs "[unknown]" prefix.
 */
inline std::ostream& operator<<(std::ostream& os, message_type v) {
    const auto name = to_string(v);
    if (name.empty()) {
        os << "[unknown]";
    } else {
        os << name;
    }
    return os << " (0x" << std::hex
              << static_cast<std::uint16_t>(v)
              << std::dec << ")";
}

}

#endif
