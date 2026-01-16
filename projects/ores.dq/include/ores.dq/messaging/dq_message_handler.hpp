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
#include "ores.dq/service/change_management_service.hpp"

namespace ores::dq::messaging {

/**
 * @brief Message handler for Data Quality (DQ) subsystem messages.
 *
 * Processes messages in the DQ subsystem range (0x6000-0x6FFF).
 * Currently handles change management messages:
 * - get_change_reason_categories_request: Retrieves all change reason categories
 * - get_change_reasons_request: Retrieves all change reasons
 * - get_change_reasons_by_category_request: Retrieves reasons for a category
 * - save_change_reason_request: Creates or updates a change reason
 * - delete_change_reason_request: Deletes change reason(s)
 * - get_change_reason_history_request: Retrieves version history for a reason
 * - save_change_reason_category_request: Creates or updates a category
 * - delete_change_reason_category_request: Deletes category(ies)
 * - get_change_reason_category_history_request: Retrieves version history
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
     */
    dq_message_handler(database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions);

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
    service::change_management_service change_management_service_;
};

}

#endif
