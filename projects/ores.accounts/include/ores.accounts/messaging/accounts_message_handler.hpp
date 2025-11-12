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

#include "ores.utility/repository/context.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/protocol/message_handler.hpp"
#include "ores.accounts/repository/login_info_repository.hpp"
#include "ores.accounts/repository/account_repository.hpp"

namespace ores::accounts::messaging {

/**
 * @brief Message handler for accounts subsystem messages.
 *
 * Processes messages in the accounts subsystem range (0x2000-0x2FFF).
 * Currently handles:
 * - create_account_request: Creates a new account
 * - list_accounts_request: Retrieves all accounts from the repository
 * - login_request: Authenticates a user and updates login tracking
 * - unlock_account_request: Unlocks a locked account
 */
class accounts_message_handler final : public comms::protocol::message_handler {
private:
    static auto& lg() {
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
     */
    explicit accounts_message_handler(utility::repository::context ctx);

    /**
     * @brief Handle an accounts subsystem message.
     *
     * @param type The message type (must be in range 0x2000-0x2FFF)
     * @param payload The message payload
     * @param remote_address The remote endpoint address of the client connection
     * @return Expected containing response payload, or error code
     */
    boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                         comms::protocol::error_code>>
    handle_message(comms::protocol::message_type type,
        std::span<const std::uint8_t> payload,
        const std::string& remote_address) override;

private:
    /**
     * @brief Handle create_account_request message.
     */
  boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                       comms::protocol::error_code>>
    handle_create_account_request(std::span<const std::uint8_t> payload);

    /**
     * @brief Handle list_accounts_request message.
     */
  boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                       comms::protocol::error_code>>
    handle_list_accounts_request(std::span<const std::uint8_t> payload);

    /**
     * @brief Handle login_request message.
     */
  boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                       comms::protocol::error_code>>
    handle_login_request(std::span<const std::uint8_t> payload,
        const std::string& remote_address);

    /**
     * @brief Handle unlock_account_request message.
     */
  boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                       comms::protocol::error_code>>
    handle_unlock_account_request(std::span<const std::uint8_t> payload);

    utility::repository::context ctx_;
    repository::account_repository account_repo_;
    repository::login_info_repository login_info_repo_;
};

}

#endif
