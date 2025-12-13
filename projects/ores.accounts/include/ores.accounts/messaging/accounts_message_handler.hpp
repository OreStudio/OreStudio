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
#ifndef ORES_ACCOUNTS_MESSAGING_ACCOUNTS_MESSAGE_HANDLER_HPP
#define ORES_ACCOUNTS_MESSAGING_ACCOUNTS_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.database/domain/context.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.accounts/service/account_service.hpp"
#include "ores.variability/service/system_flags_service.hpp"

namespace ores::accounts::messaging {

/**
 * @brief Message handler for accounts subsystem messages.
 *
 * Processes messages in the accounts subsystem range (0x2000-0x2FFF).
 * Currently handles:
 * - create_account_request: Creates a new account
 * - list_accounts_request: Retrieves all accounts from the repository
 * - list_login_info_request: Retrieves all login info records
 * - login_request: Authenticates a user and updates login tracking
 * - logout_request: Logs out a user and marks them as offline
 * - unlock_account_request: Unlocks a locked account
 * - delete_account_request: Deletes an account (bitemporal soft delete)
 * - create_initial_admin_request: Creates initial admin (bootstrap mode only, localhost only)
 * - bootstrap_status_request: Checks if system is in bootstrap mode
 */
class accounts_message_handler final : public comms::messaging::message_handler {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.accounts.messaging.accounts_message_handler");
        return instance;
    }

public:
    /**
     * @brief Construct an accounts message handler.
     *
     * @param ctx Database context for repository access
     * @param system_flags Shared system flags service for flag access
     * @param sessions Shared auth session service for authentication
     */
    accounts_message_handler(database::context ctx,
        std::shared_ptr<variability::service::system_flags_service> system_flags,
        std::shared_ptr<comms::service::auth_session_service> sessions);

    using handler_result = boost::asio::awaitable<
        std::expected<std::vector<std::byte>, comms::messaging::error_code>
    >;

    /**
     * @brief Handle an accounts subsystem message.
     *
     * @param type The message type (must be in range 0x2000-0x2FFF)
     * @param payload The message payload
     * @param remote_address The remote endpoint address of the client connection
     * @return Expected containing response payload, or error code
     */
    handler_result
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    /**
     * @brief Handle create_account_request message.
     *
     * Requires authentication. Only admin users can create accounts.
     */
    handler_result
    handle_create_account_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle list_accounts_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_list_accounts_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle list_login_info_request message.
     *
     * Requires authentication.
     */
    handler_result
    handle_list_login_info_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle login_request message.
     */
    handler_result
    handle_login_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle unlock_account_request message.
     *
     * Requires authentication. Only admin users can unlock accounts.
     */
    handler_result
    handle_unlock_account_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle delete_account_request message.
     *
     * Requires authentication. Only admin users can delete accounts.
     */
    handler_result
    handle_delete_account_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle create_initial_admin_request message.
     *
     * Only available in bootstrap mode and from localhost.
     */
    handler_result
    handle_create_initial_admin_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle bootstrap_status_request message.
     *
     * Always available - returns current bootstrap mode status.
     */
    handler_result
    handle_bootstrap_status_request(std::span<const std::byte> payload);

    /**
     * @brief Handle logout_request message.
     *
     * Sets the user's online status to false and removes the session.
     */
    handler_result
    handle_logout_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Check if a remote address is localhost.
     *
     * @param remote_address The remote endpoint address
     * @return true if the address is localhost (127.0.0.1 or ::1), false otherwise
     */
    static bool is_localhost(const std::string& remote_address);

    service::account_service service_;
    database::context ctx_;
    std::shared_ptr<variability::service::system_flags_service> system_flags_;
    std::shared_ptr<comms::service::auth_session_service> sessions_;
};

}

#endif
