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
#include "ores.dq/messaging/dq_message_handler.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.iam/domain/permission.hpp"
#include "ores.dq/messaging/dataset_dependency_protocol.hpp"
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.dq/messaging/dataset_protocol.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.dq/messaging/publication_protocol.hpp"

namespace ores::dq::messaging {

using namespace ores::logging;
using comms::messaging::message_type;

dq_message_handler::dq_message_handler(database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions,
    std::shared_ptr<iam::service::authorization_service> auth_service)
    : ctx_(ctx), sessions_(std::move(sessions)),
      auth_service_(std::move(auth_service)),
      change_management_service_(ctx),
      data_organization_service_(ctx),
      dataset_service_(ctx),
      coding_scheme_service_(ctx),
      dimension_service_(ctx),
      publication_service_(ctx) {}

dq_message_handler::handler_result
dq_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling DQ message type " << type;

    switch (type) {
    // Change management messages
    case message_type::get_change_reason_categories_request:
        co_return co_await handle_get_change_reason_categories_request(payload, remote_address);
    case message_type::get_change_reasons_request:
        co_return co_await handle_get_change_reasons_request(payload, remote_address);
    case message_type::get_change_reasons_by_category_request:
        co_return co_await handle_get_change_reasons_by_category_request(payload, remote_address);
    case message_type::save_change_reason_request:
        co_return co_await handle_save_change_reason_request(payload, remote_address);
    case message_type::delete_change_reason_request:
        co_return co_await handle_delete_change_reason_request(payload, remote_address);
    case message_type::get_change_reason_history_request:
        co_return co_await handle_get_change_reason_history_request(payload, remote_address);
    case message_type::save_change_reason_category_request:
        co_return co_await handle_save_change_reason_category_request(payload, remote_address);
    case message_type::delete_change_reason_category_request:
        co_return co_await handle_delete_change_reason_category_request(payload, remote_address);
    case message_type::get_change_reason_category_history_request:
        co_return co_await handle_get_change_reason_category_history_request(payload, remote_address);

    // Catalog messages
    case message_type::get_catalogs_request:
        co_return co_await handle_get_catalogs_request(payload, remote_address);
    case message_type::save_catalog_request:
        co_return co_await handle_save_catalog_request(payload, remote_address);
    case message_type::delete_catalog_request:
        co_return co_await handle_delete_catalog_request(payload, remote_address);
    case message_type::get_catalog_history_request:
        co_return co_await handle_get_catalog_history_request(payload, remote_address);

    // Dataset dependency messages
    case message_type::get_dataset_dependencies_request:
        co_return co_await handle_get_dataset_dependencies_request(payload, remote_address);
    case message_type::get_dataset_dependencies_by_dataset_request:
        co_return co_await handle_get_dataset_dependencies_by_dataset_request(payload, remote_address);

    // Data domain messages
    case message_type::get_data_domains_request:
        co_return co_await handle_get_data_domains_request(payload, remote_address);
    case message_type::save_data_domain_request:
        co_return co_await handle_save_data_domain_request(payload, remote_address);
    case message_type::delete_data_domain_request:
        co_return co_await handle_delete_data_domain_request(payload, remote_address);
    case message_type::get_data_domain_history_request:
        co_return co_await handle_get_data_domain_history_request(payload, remote_address);

    // Subject area messages
    case message_type::get_subject_areas_request:
        co_return co_await handle_get_subject_areas_request(payload, remote_address);
    case message_type::get_subject_areas_by_domain_request:
        co_return co_await handle_get_subject_areas_by_domain_request(payload, remote_address);
    case message_type::save_subject_area_request:
        co_return co_await handle_save_subject_area_request(payload, remote_address);
    case message_type::delete_subject_area_request:
        co_return co_await handle_delete_subject_area_request(payload, remote_address);
    case message_type::get_subject_area_history_request:
        co_return co_await handle_get_subject_area_history_request(payload, remote_address);

    // Dataset messages
    case message_type::get_datasets_request:
        co_return co_await handle_get_datasets_request(payload, remote_address);
    case message_type::save_dataset_request:
        co_return co_await handle_save_dataset_request(payload, remote_address);
    case message_type::delete_dataset_request:
        co_return co_await handle_delete_dataset_request(payload, remote_address);
    case message_type::get_dataset_history_request:
        co_return co_await handle_get_dataset_history_request(payload, remote_address);

    // Methodology messages
    case message_type::get_methodologies_request:
        co_return co_await handle_get_methodologies_request(payload, remote_address);
    case message_type::save_methodology_request:
        co_return co_await handle_save_methodology_request(payload, remote_address);
    case message_type::delete_methodology_request:
        co_return co_await handle_delete_methodology_request(payload, remote_address);
    case message_type::get_methodology_history_request:
        co_return co_await handle_get_methodology_history_request(payload, remote_address);

    // Coding scheme messages
    case message_type::get_coding_schemes_request:
        co_return co_await handle_get_coding_schemes_request(payload, remote_address);
    case message_type::get_coding_schemes_by_authority_type_request:
        co_return co_await handle_get_coding_schemes_by_authority_type_request(payload, remote_address);
    case message_type::save_coding_scheme_request:
        co_return co_await handle_save_coding_scheme_request(payload, remote_address);
    case message_type::delete_coding_scheme_request:
        co_return co_await handle_delete_coding_scheme_request(payload, remote_address);
    case message_type::get_coding_scheme_history_request:
        co_return co_await handle_get_coding_scheme_history_request(payload, remote_address);

    // Coding scheme authority type messages
    case message_type::get_coding_scheme_authority_types_request:
        co_return co_await handle_get_coding_scheme_authority_types_request(payload, remote_address);
    case message_type::save_coding_scheme_authority_type_request:
        co_return co_await handle_save_coding_scheme_authority_type_request(payload, remote_address);
    case message_type::delete_coding_scheme_authority_type_request:
        co_return co_await handle_delete_coding_scheme_authority_type_request(payload, remote_address);
    case message_type::get_coding_scheme_authority_type_history_request:
        co_return co_await handle_get_coding_scheme_authority_type_history_request(payload, remote_address);

    // Nature dimension messages
    case message_type::get_nature_dimensions_request:
        co_return co_await handle_get_nature_dimensions_request(payload, remote_address);
    case message_type::save_nature_dimension_request:
        co_return co_await handle_save_nature_dimension_request(payload, remote_address);
    case message_type::delete_nature_dimension_request:
        co_return co_await handle_delete_nature_dimension_request(payload, remote_address);
    case message_type::get_nature_dimension_history_request:
        co_return co_await handle_get_nature_dimension_history_request(payload, remote_address);

    // Origin dimension messages
    case message_type::get_origin_dimensions_request:
        co_return co_await handle_get_origin_dimensions_request(payload, remote_address);
    case message_type::save_origin_dimension_request:
        co_return co_await handle_save_origin_dimension_request(payload, remote_address);
    case message_type::delete_origin_dimension_request:
        co_return co_await handle_delete_origin_dimension_request(payload, remote_address);
    case message_type::get_origin_dimension_history_request:
        co_return co_await handle_get_origin_dimension_history_request(payload, remote_address);

    // Treatment dimension messages
    case message_type::get_treatment_dimensions_request:
        co_return co_await handle_get_treatment_dimensions_request(payload, remote_address);
    case message_type::save_treatment_dimension_request:
        co_return co_await handle_save_treatment_dimension_request(payload, remote_address);
    case message_type::delete_treatment_dimension_request:
        co_return co_await handle_delete_treatment_dimension_request(payload, remote_address);
    case message_type::get_treatment_dimension_history_request:
        co_return co_await handle_get_treatment_dimension_history_request(payload, remote_address);

    // Publication messages
    case message_type::publish_datasets_request:
        co_return co_await handle_publish_datasets_request(payload, remote_address);

    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown DQ message type " << type;
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

dq_message_handler::auth_check_result
dq_message_handler::get_authenticated_session(
    const std::string& remote_address,
    std::string_view operation_name) {

    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << operation_name
                                  << " denied: no active session for "
                                  << remote_address;
        return std::unexpected(ores::utility::serialization::error_code::authentication_failed);
    }
    return *session;
}

dq_message_handler::auth_check_result
dq_message_handler::check_authorization(
    const std::string& remote_address,
    std::string_view permission,
    std::string_view operation_name) {

    auto session_result = get_authenticated_session(remote_address, operation_name);
    if (!session_result) {
        return session_result;
    }

    const auto& session = *session_result;
    if (!auth_service_->has_permission(session.account_id, permission)) {
        BOOST_LOG_SEV(lg(), warn) << operation_name
                                  << " denied: account "
                                  << session.account_id
                                  << " lacks permission "
                                  << permission;
        return std::unexpected(ores::utility::serialization::error_code::authorization_failed);
    }

    return session;
}

// ============================================================================
// Change Management Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_change_reason_categories_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_change_reason_categories_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List change reason categories");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_change_reason_categories_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_change_reason_categories_request";
        co_return std::unexpected(request_result.error());
    }

    auto categories = change_management_service_.list_categories();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << categories.size()
                              << " change reason categories.";

    get_change_reason_categories_response response{
        .categories = std::move(categories)
    };
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_change_reasons_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_change_reasons_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List change reasons");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_change_reasons_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_change_reasons_request";
        co_return std::unexpected(request_result.error());
    }

    auto reasons = change_management_service_.list_reasons();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << reasons.size()
                              << " change reasons.";

    get_change_reasons_response response{
        .reasons = std::move(reasons)
    };
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_change_reasons_by_category_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_change_reasons_by_category_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List change reasons by category");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_change_reasons_by_category_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_change_reasons_by_category_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Filtering by category: " << request.category_code;

    auto reasons = change_management_service_.list_reasons_by_category(
        request.category_code);
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << reasons.size()
                              << " change reasons for category: "
                              << request.category_code;

    get_change_reasons_by_category_response response{
        .reasons = std::move(reasons)
    };
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_change_reason_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_change_reason_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::change_reasons_write, "Save change reason");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_change_reason_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_change_reason_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    save_change_reason_response response;
    try {
        change_management_service_.save_reason(request.reason);
        response.success = true;
        response.message = "Change reason saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved change reason: " << request.reason.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save change reason: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_change_reason_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_change_reason_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::change_reasons_delete, "Delete change reason");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_change_reason_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_change_reason_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    delete_change_reason_response response;
    for (const auto& code : request.codes) {
        delete_change_reason_result result;
        result.code = code;
        try {
            change_management_service_.remove_reason(code);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted change reason: " << code;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete change reason "
                                       << code << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_change_reason_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_change_reason_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get change reason history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_change_reason_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_change_reason_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Getting history for reason: " << request.code;

    get_change_reason_history_response response;
    try {
        response.versions = change_management_service_.get_reason_history(request.code);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for change reason: " << request.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get change reason history: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_change_reason_category_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_change_reason_category_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::change_reason_categories_write, "Save change reason category");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_change_reason_category_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_change_reason_category_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    save_change_reason_category_response response;
    try {
        change_management_service_.save_category(request.category);
        response.success = true;
        response.message = "Change reason category saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved change reason category: "
                                  << request.category.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save change reason category: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_change_reason_category_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_change_reason_category_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::change_reason_categories_delete, "Delete change reason category");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_change_reason_category_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_change_reason_category_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    delete_change_reason_category_response response;
    for (const auto& code : request.codes) {
        delete_change_reason_category_result result;
        result.code = code;
        try {
            change_management_service_.remove_category(code);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted change reason category: " << code;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete change reason category "
                                       << code << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_change_reason_category_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_change_reason_category_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get change reason category history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_change_reason_category_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_change_reason_category_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Getting history for category: " << request.code;

    get_change_reason_category_history_response response;
    try {
        response.versions = change_management_service_.get_category_history(request.code);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for change reason category: "
                                  << request.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get change reason category history: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Catalog Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_catalogs_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_catalogs_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address, "List catalogs");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_catalogs_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_catalogs_request";
        co_return std::unexpected(request_result.error());
    }

    auto catalogs = data_organization_service_.list_catalogs();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << catalogs.size() << " catalogs.";

    get_catalogs_response response{.catalogs = std::move(catalogs)};
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_catalog_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_catalog_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::catalogs_write, "Save catalog");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_catalog_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_catalog_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_catalog_response response;
    try {
        data_organization_service_.save_catalog(request.catalog);
        response.success = true;
        response.message = "Catalog saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved catalog: " << request.catalog.name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save catalog: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_catalog_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_catalog_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::catalogs_delete, "Delete catalog");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_catalog_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_catalog_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_catalog_response response;
    for (const auto& name : request.names) {
        delete_catalog_result result;
        result.name = name;
        try {
            data_organization_service_.remove_catalog(name);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted catalog: " << name;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete catalog " << name
                                       << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_catalog_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_catalog_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get catalog history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_catalog_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_catalog_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_catalog_history_response response;
    try {
        response.versions = data_organization_service_.get_catalog_history(request.name);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for catalog: " << request.name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get catalog history: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Dataset Dependency Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_dataset_dependencies_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_dataset_dependencies_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List dataset dependencies");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_dataset_dependencies_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_dataset_dependencies_request";
        co_return std::unexpected(request_result.error());
    }

    auto dependencies = data_organization_service_.list_dataset_dependencies();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << dependencies.size()
                              << " dataset dependencies.";

    get_dataset_dependencies_response response{
        .dependencies = std::move(dependencies)
    };
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_dataset_dependencies_by_dataset_request(
    std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_dataset_dependencies_by_dataset_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List dataset dependencies by dataset");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result =
        get_dataset_dependencies_by_dataset_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_dataset_dependencies_by_dataset_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto dependencies = data_organization_service_.list_dataset_dependencies_by_dataset(
        request.dataset_code);
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << dependencies.size()
                              << " dataset dependencies for dataset: "
                              << request.dataset_code;

    get_dataset_dependencies_by_dataset_response response{
        .dependencies = std::move(dependencies)
    };
    co_return response.serialize();
}

// ============================================================================
// Data Domain Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_data_domains_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_data_domains_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List data domains");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_data_domains_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_data_domains_request";
        co_return std::unexpected(request_result.error());
    }

    auto domains = data_organization_service_.list_data_domains();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << domains.size() << " data domains.";

    get_data_domains_response response{
        .domains = std::move(domains)
    };
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_data_domain_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_data_domain_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::data_domains_write, "Save data domain");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_data_domain_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_data_domain_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_data_domain_response response;
    try {
        data_organization_service_.save_data_domain(request.domain);
        response.success = true;
        response.message = "Data domain saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved data domain: " << request.domain.name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save data domain: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_data_domain_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_data_domain_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::data_domains_delete, "Delete data domain");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_data_domain_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_data_domain_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_data_domain_response response;
    for (const auto& name : request.names) {
        delete_data_domain_result result;
        result.name = name;
        try {
            data_organization_service_.remove_data_domain(name);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted data domain: " << name;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete data domain "
                                       << name << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_data_domain_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_data_domain_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get data domain history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_data_domain_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_data_domain_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_data_domain_history_response response;
    try {
        response.versions = data_organization_service_.get_data_domain_history(
            request.name);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for data domain: " << request.name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get data domain history: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Subject Area Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_subject_areas_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_subject_areas_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List subject areas");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_subject_areas_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_subject_areas_request";
        co_return std::unexpected(request_result.error());
    }

    auto subject_areas = data_organization_service_.list_subject_areas();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << subject_areas.size()
                              << " subject areas.";

    get_subject_areas_response response{.subject_areas = std::move(subject_areas)};
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_subject_areas_by_domain_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_subject_areas_by_domain_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List subject areas by domain");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_subject_areas_by_domain_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_subject_areas_by_domain_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto subject_areas = data_organization_service_.list_subject_areas_by_domain(
        request.domain_name);
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << subject_areas.size()
                              << " subject areas for domain: " << request.domain_name;

    get_subject_areas_by_domain_response response{
        .subject_areas = std::move(subject_areas)
    };
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_subject_area_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_subject_area_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::subject_areas_write, "Save subject area");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_subject_area_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_subject_area_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_subject_area_response response;
    try {
        data_organization_service_.save_subject_area(request.subject_area);
        response.success = true;
        response.message = "Subject area saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved subject area: "
                                  << request.subject_area.name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save subject area: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_subject_area_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_subject_area_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::subject_areas_delete, "Delete subject area");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_subject_area_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_subject_area_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_subject_area_response response;
    for (const auto& key : request.keys) {
        delete_subject_area_result result;
        result.key = key;
        try {
            data_organization_service_.remove_subject_area(key.name, key.domain_name);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted subject area: " << key.name;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete subject area "
                                       << key.name << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_subject_area_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_subject_area_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get subject area history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_subject_area_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_subject_area_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_subject_area_history_response response;
    try {
        response.versions = data_organization_service_.get_subject_area_history(
            request.key.name, request.key.domain_name);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for subject area: "
                                  << request.key.name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get subject area history: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Dataset Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_datasets_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_datasets_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address, "List datasets");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_datasets_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_datasets_request";
        co_return std::unexpected(request_result.error());
    }

    auto datasets = dataset_service_.list_datasets();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << datasets.size() << " datasets.";

    get_datasets_response response{.datasets = std::move(datasets)};
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_dataset_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_dataset_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::datasets_write, "Save dataset");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_dataset_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_dataset_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_dataset_response response;
    try {
        dataset_service_.save_dataset(request.dataset);
        response.success = true;
        response.message = "Dataset saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved dataset: " << request.dataset.id;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save dataset: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_dataset_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_dataset_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::datasets_delete, "Delete dataset");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_dataset_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_dataset_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_dataset_response response;
    for (const auto& id : request.ids) {
        delete_dataset_result result;
        result.id = id;
        try {
            dataset_service_.remove_dataset(id);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted dataset: " << id;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete dataset "
                                       << id << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_dataset_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_dataset_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get dataset history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_dataset_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_dataset_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_dataset_history_response response;
    try {
        response.versions = dataset_service_.get_dataset_history(request.id);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for dataset: " << request.id;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get dataset history: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Methodology Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_methodologies_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_methodologies_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List methodologies");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_methodologies_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_methodologies_request";
        co_return std::unexpected(request_result.error());
    }

    auto methodologies = dataset_service_.list_methodologies();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << methodologies.size()
                              << " methodologies.";

    get_methodologies_response response{.methodologies = std::move(methodologies)};
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_methodology_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_methodology_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::methodologies_write, "Save methodology");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_methodology_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_methodology_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_methodology_response response;
    try {
        dataset_service_.save_methodology(request.methodology);
        response.success = true;
        response.message = "Methodology saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved methodology: "
                                  << request.methodology.id;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save methodology: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_methodology_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_methodology_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::methodologies_delete, "Delete methodology");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_methodology_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_methodology_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_methodology_response response;
    for (const auto& id : request.ids) {
        delete_methodology_result result;
        result.id = id;
        try {
            dataset_service_.remove_methodology(id);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted methodology: " << id;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete methodology "
                                       << id << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_methodology_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_methodology_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get methodology history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_methodology_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_methodology_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_methodology_history_response response;
    try {
        response.versions = dataset_service_.get_methodology_history(request.id);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for methodology: " << request.id;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get methodology history: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Coding Scheme Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_coding_schemes_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_coding_schemes_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List coding schemes");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_coding_schemes_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_coding_schemes_request";
        co_return std::unexpected(request_result.error());
    }

    auto schemes = coding_scheme_service_.list_coding_schemes();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << schemes.size()
                              << " coding schemes.";

    get_coding_schemes_response response{.schemes = std::move(schemes)};
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_coding_schemes_by_authority_type_request(
    std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_coding_schemes_by_authority_type_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List coding schemes by authority type");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result =
        get_coding_schemes_by_authority_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_coding_schemes_by_authority_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto schemes = coding_scheme_service_.list_coding_schemes_by_authority_type(
        request.authority_type);
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << schemes.size()
                              << " coding schemes for authority type: "
                              << request.authority_type;

    get_coding_schemes_by_authority_type_response response{
        .schemes = std::move(schemes)
    };
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_coding_scheme_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_coding_scheme_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::coding_schemes_write, "Save coding scheme");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_coding_scheme_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_coding_scheme_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_coding_scheme_response response;
    try {
        coding_scheme_service_.save_coding_scheme(request.scheme);
        response.success = true;
        response.message = "Coding scheme saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved coding scheme: "
                                  << request.scheme.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save coding scheme: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_coding_scheme_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_coding_scheme_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::coding_schemes_delete, "Delete coding scheme");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_coding_scheme_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_coding_scheme_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_coding_scheme_response response;
    for (const auto& code : request.codes) {
        delete_coding_scheme_result result;
        result.code = code;
        try {
            coding_scheme_service_.remove_coding_scheme(code);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted coding scheme: " << code;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete coding scheme "
                                       << code << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_coding_scheme_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_coding_scheme_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get coding scheme history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_coding_scheme_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_coding_scheme_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_coding_scheme_history_response response;
    try {
        response.versions = coding_scheme_service_.get_coding_scheme_history(
            request.code);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for coding scheme: "
                                  << request.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get coding scheme history: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Coding Scheme Authority Type Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_coding_scheme_authority_types_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_coding_scheme_authority_types_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List coding scheme authority types");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_coding_scheme_authority_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_coding_scheme_authority_types_request";
        co_return std::unexpected(request_result.error());
    }

    auto authority_types = coding_scheme_service_.list_authority_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << authority_types.size()
                              << " coding scheme authority types.";

    get_coding_scheme_authority_types_response response{
        .authority_types = std::move(authority_types)
    };
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_coding_scheme_authority_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_coding_scheme_authority_type_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::coding_scheme_authority_types_write, "Save coding scheme authority type");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_coding_scheme_authority_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_coding_scheme_authority_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_coding_scheme_authority_type_response response;
    try {
        coding_scheme_service_.save_authority_type(request.authority_type);
        response.success = true;
        response.message = "Coding scheme authority type saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved coding scheme authority type: "
                                  << request.authority_type.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save coding scheme authority type: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_coding_scheme_authority_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_coding_scheme_authority_type_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::coding_scheme_authority_types_delete,
        "Delete coding scheme authority type");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_coding_scheme_authority_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_coding_scheme_authority_type_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_coding_scheme_authority_type_response response;
    for (const auto& code : request.codes) {
        delete_coding_scheme_authority_type_result result;
        result.code = code;
        try {
            coding_scheme_service_.remove_authority_type(code);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted coding scheme authority type: " << code;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete coding scheme authority type "
                                       << code << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_coding_scheme_authority_type_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_coding_scheme_authority_type_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get coding scheme authority type history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_coding_scheme_authority_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_coding_scheme_authority_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_coding_scheme_authority_type_history_response response;
    try {
        response.versions = coding_scheme_service_.get_authority_type_history(
            request.code);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for coding scheme authority type: "
                                  << request.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get coding scheme authority type history: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Nature Dimension Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_nature_dimensions_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_nature_dimensions_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List nature dimensions");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_nature_dimensions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_nature_dimensions_request";
        co_return std::unexpected(request_result.error());
    }

    auto dimensions = dimension_service_.list_nature_dimensions();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << dimensions.size()
                              << " nature dimensions.";

    get_nature_dimensions_response response{.dimensions = std::move(dimensions)};
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_nature_dimension_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_nature_dimension_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::nature_dimensions_write,
        "Save nature dimension");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_nature_dimension_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_nature_dimension_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_nature_dimension_response response;
    try {
        dimension_service_.save_nature_dimension(request.dimension);
        response.success = true;
        response.message = "Nature dimension saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved nature dimension: "
                                  << request.dimension.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save nature dimension: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_nature_dimension_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_nature_dimension_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::nature_dimensions_delete,
        "Delete nature dimension");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_nature_dimension_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_nature_dimension_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_nature_dimension_response response;
    for (const auto& code : request.codes) {
        delete_nature_dimension_result result;
        result.code = code;
        try {
            dimension_service_.remove_nature_dimension(code);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted nature dimension: " << code;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete nature dimension "
                                       << code << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_nature_dimension_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_nature_dimension_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get nature dimension history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_nature_dimension_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_nature_dimension_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_nature_dimension_history_response response;
    try {
        response.versions = dimension_service_.get_nature_dimension_history(
            request.code);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for nature dimension: "
                                  << request.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get nature dimension history: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Origin Dimension Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_origin_dimensions_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_origin_dimensions_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List origin dimensions");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_origin_dimensions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_origin_dimensions_request";
        co_return std::unexpected(request_result.error());
    }

    auto dimensions = dimension_service_.list_origin_dimensions();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << dimensions.size()
                              << " origin dimensions.";

    get_origin_dimensions_response response{.dimensions = std::move(dimensions)};
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_origin_dimension_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_origin_dimension_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::origin_dimensions_write,
        "Save origin dimension");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_origin_dimension_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_origin_dimension_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_origin_dimension_response response;
    try {
        dimension_service_.save_origin_dimension(request.dimension);
        response.success = true;
        response.message = "Origin dimension saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved origin dimension: "
                                  << request.dimension.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save origin dimension: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_origin_dimension_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_origin_dimension_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::origin_dimensions_delete,
        "Delete origin dimension");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_origin_dimension_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_origin_dimension_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_origin_dimension_response response;
    for (const auto& code : request.codes) {
        delete_origin_dimension_result result;
        result.code = code;
        try {
            dimension_service_.remove_origin_dimension(code);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted origin dimension: " << code;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete origin dimension "
                                       << code << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_origin_dimension_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_origin_dimension_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get origin dimension history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_origin_dimension_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_origin_dimension_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_origin_dimension_history_response response;
    try {
        response.versions = dimension_service_.get_origin_dimension_history(
            request.code);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for origin dimension: "
                                  << request.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get origin dimension history: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// Treatment Dimension Handlers
// ============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_get_treatment_dimensions_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_treatment_dimensions_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "List treatment dimensions");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_treatment_dimensions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_treatment_dimensions_request";
        co_return std::unexpected(request_result.error());
    }

    auto dimensions = dimension_service_.list_treatment_dimensions();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << dimensions.size()
                              << " treatment dimensions.";

    get_treatment_dimensions_response response{.dimensions = std::move(dimensions)};
    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_save_treatment_dimension_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_treatment_dimension_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::treatment_dimensions_write,
        "Save treatment dimension");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = save_treatment_dimension_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_treatment_dimension_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    save_treatment_dimension_response response;
    try {
        dimension_service_.save_treatment_dimension(request.dimension);
        response.success = true;
        response.message = "Treatment dimension saved successfully.";
        BOOST_LOG_SEV(lg(), info) << "Saved treatment dimension: "
                                  << request.dimension.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save treatment dimension: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_delete_treatment_dimension_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_treatment_dimension_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::treatment_dimensions_delete,
        "Delete treatment dimension");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_treatment_dimension_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_treatment_dimension_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    delete_treatment_dimension_response response;
    for (const auto& code : request.codes) {
        delete_treatment_dimension_result result;
        result.code = code;
        try {
            dimension_service_.remove_treatment_dimension(code);
            result.success = true;
            result.message = "Deleted successfully.";
            BOOST_LOG_SEV(lg(), info) << "Deleted treatment dimension: " << code;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete treatment dimension "
                                       << code << ": " << e.what();
            result.success = false;
            result.message = e.what();
        }
        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

dq_message_handler::handler_result dq_message_handler::
handle_get_treatment_dimension_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_treatment_dimension_history_request from "
                               << remote_address;

    auto auth_result = get_authenticated_session(remote_address,
        "Get treatment dimension history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_treatment_dimension_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_treatment_dimension_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    get_treatment_dimension_history_response response;
    try {
        response.versions = dimension_service_.get_treatment_dimension_history(
            request.code);
        response.success = true;
        response.message = "";
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.versions.size()
                                  << " versions for treatment dimension: "
                                  << request.code;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get treatment dimension history: "
                                   << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// =============================================================================
// Publication Handlers
// =============================================================================

dq_message_handler::handler_result dq_message_handler::
handle_publish_datasets_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing publish_datasets_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        "datasets:publish", "Publish datasets");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = publish_datasets_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize publish_datasets_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Publishing " << request.dataset_ids.size()
                              << " datasets, mode: " << request.mode
                              << ", resolve_dependencies: "
                              << request.resolve_dependencies;

    publish_datasets_response response;
    try {
        response.results = publication_service_.publish(
            request.dataset_ids,
            request.mode,
            request.published_by,
            request.resolve_dependencies);

        std::uint64_t total_inserted = 0;
        std::uint64_t total_failed = 0;
        for (const auto& result : response.results) {
            if (result.success) {
                total_inserted += result.records_inserted;
            } else {
                ++total_failed;
            }
        }

        BOOST_LOG_SEV(lg(), info) << "Publication complete: "
                                  << response.results.size() << " datasets, "
                                  << total_inserted << " total records inserted, "
                                  << total_failed << " failures";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Publication failed: " << e.what();
        // Return a single error result
        domain::publication_result error_result;
        error_result.success = false;
        error_result.error_message = e.what();
        response.results.push_back(error_result);
    }

    co_return response.serialize();
}

}
