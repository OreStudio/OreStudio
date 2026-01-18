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
#ifndef ORES_DQ_MESSAGING_DQ_MESSAGE_HANDLER_HPP
#define ORES_DQ_MESSAGING_DQ_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.dq/service/change_management_service.hpp"
#include "ores.dq/service/data_organization_service.hpp"
#include "ores.dq/service/dataset_service.hpp"
#include "ores.dq/service/coding_scheme_service.hpp"
#include "ores.dq/service/dimension_service.hpp"

namespace ores::dq::messaging {

/**
 * @brief Message handler for Data Quality (DQ) subsystem messages.
 *
 * Processes messages in the DQ subsystem range (0x6000-0x6FFF).
 * Currently handles change management messages:
 * - Change management: categories and reasons CRUD
 * - Data organization: catalogs and subject areas CRUD
 * - Datasets and methodologies CRUD
 * - Coding schemes CRUD
 */
class dq_message_handler final : public comms::messaging::message_handler {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.dq.messaging.dq_message_handler");
        return instance;
    }

public:
    /**
     * @brief Construct a DQ message handler.
     *
     * @param ctx Database context for repository access
     * @param sessions Shared auth session service for authentication
     * @param auth_service Shared authorization service for RBAC permission checks
     */
    dq_message_handler(database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions,
        std::shared_ptr<iam::service::authorization_service> auth_service);

    using handler_result = boost::asio::awaitable<
        std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>
    >;

    /**
     * @brief Handle a DQ subsystem message.
     *
     * @param type The message type (must be in range 0x6000-0x6FFF)
     * @param payload The message payload
     * @param remote_address The remote endpoint address of the client connection
     * @return Expected containing response payload, or error code
     */
    handler_result
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    // =========================================================================
    // Change Management Handlers
    // =========================================================================

    /**
     * @brief Handle get_change_reason_categories_request message.
     *
     * Requires authentication. Returns all change reason categories.
     */
    handler_result
    handle_get_change_reason_categories_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_change_reasons_request message.
     *
     * Requires authentication. Returns all change reasons.
     */
    handler_result
    handle_get_change_reasons_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_change_reasons_by_category_request message.
     *
     * Requires authentication. Returns change reasons for a category.
     */
    handler_result
    handle_get_change_reasons_by_category_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle save_change_reason_request message.
     *
     * Requires authentication and change_reasons:write permission.
     */
    handler_result
    handle_save_change_reason_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle delete_change_reason_request message.
     *
     * Requires authentication and change_reasons:delete permission.
     */
    handler_result
    handle_delete_change_reason_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_change_reason_history_request message.
     *
     * Requires authentication. Returns all versions of a change reason.
     */
    handler_result
    handle_get_change_reason_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle save_change_reason_category_request message.
     *
     * Requires authentication and change_reason_categories:write permission.
     */
    handler_result
    handle_save_change_reason_category_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle delete_change_reason_category_request message.
     *
     * Requires authentication and change_reason_categories:delete permission.
     */
    handler_result
    handle_delete_change_reason_category_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_change_reason_category_history_request message.
     *
     * Requires authentication. Returns all versions of a category.
     */
    handler_result
    handle_get_change_reason_category_history_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Catalog Handlers
    // =========================================================================

    handler_result
    handle_get_catalogs_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_catalog_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_catalog_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_catalog_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Data Domain Handlers
    // =========================================================================

    handler_result
    handle_get_data_domains_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_data_domain_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_data_domain_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_data_domain_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Subject Area Handlers
    // =========================================================================

    handler_result
    handle_get_subject_areas_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_subject_areas_by_domain_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_subject_area_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_subject_area_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_subject_area_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Dataset Handlers
    // =========================================================================

    handler_result
    handle_get_datasets_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_dataset_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_dataset_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_dataset_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Methodology Handlers
    // =========================================================================

    handler_result
    handle_get_methodologies_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_methodology_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_methodology_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_methodology_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Coding Scheme Handlers
    // =========================================================================

    handler_result
    handle_get_coding_schemes_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_coding_schemes_by_authority_type_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_coding_scheme_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_coding_scheme_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_coding_scheme_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Coding Scheme Authority Type Handlers
    // =========================================================================

    handler_result
    handle_get_coding_scheme_authority_types_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_coding_scheme_authority_type_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_coding_scheme_authority_type_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_coding_scheme_authority_type_history_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Nature Dimension Handlers
    // =========================================================================

    handler_result
    handle_get_nature_dimensions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_nature_dimension_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_nature_dimension_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_nature_dimension_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Origin Dimension Handlers
    // =========================================================================

    handler_result
    handle_get_origin_dimensions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_origin_dimension_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_origin_dimension_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_origin_dimension_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // =========================================================================
    // Treatment Dimension Handlers
    // =========================================================================

    handler_result
    handle_get_treatment_dimensions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_save_treatment_dimension_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_delete_treatment_dimension_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    handler_result
    handle_get_treatment_dimension_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Result type for authentication checks.
     *
     * Contains the session info if authenticated, or an error code if not.
     */
    using auth_check_result = std::expected<
        comms::service::session_info,
        ores::utility::serialization::error_code
    >;

    /**
     * @brief Get session for a request without permission check.
     *
     * Used for operations that require authentication but no specific permission.
     *
     * @param remote_address The remote endpoint address
     * @param operation_name Human-readable name for logging
     * @return The session if found, or error code if not
     */
    auth_check_result get_authenticated_session(
        const std::string& remote_address,
        std::string_view operation_name);

    database::context ctx_;
    std::shared_ptr<comms::service::auth_session_service> sessions_;
    std::shared_ptr<iam::service::authorization_service> auth_service_;
    service::change_management_service change_management_service_;
    service::data_organization_service data_organization_service_;
    service::dataset_service dataset_service_;
    service::coding_scheme_service coding_scheme_service_;
    service::dimension_service dimension_service_;
};

}

#endif
