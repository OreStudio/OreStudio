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

#include "ores.dq/messaging/change_management_protocol.hpp"

namespace ores::dq::messaging {

using namespace ores::logging;
using comms::messaging::message_type;

dq_message_handler::dq_message_handler(database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : ctx_(ctx), sessions_(std::move(sessions)),
      change_management_service_(ctx) {}

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

    auto auth_result = get_authenticated_session(remote_address,
        "Save change reason");
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
        // Check if reason exists (update) or not (create)
        auto existing = change_management_service_.find_reason(request.reason.code);
        if (existing) {
            change_management_service_.update_reason(request.reason);
        } else {
            change_management_service_.create_reason(request.reason);
        }
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

    auto auth_result = get_authenticated_session(remote_address,
        "Delete change reason");
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

    auto auth_result = get_authenticated_session(remote_address,
        "Save change reason category");
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
        // Check if category exists (update) or not (create)
        auto existing = change_management_service_.find_category(request.category.code);
        if (existing) {
            change_management_service_.update_category(request.category);
        } else {
            change_management_service_.create_category(request.category);
        }
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

    auto auth_result = get_authenticated_session(remote_address,
        "Delete change reason category");
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

}
