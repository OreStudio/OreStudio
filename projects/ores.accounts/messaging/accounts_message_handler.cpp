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
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.accounts/messaging/accounts_message_handler.hpp"
#include "ores.accounts/messaging/protocol.hpp"
#include "ores.utility/log/logger.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.accounts.messaging.accounts_message_handler"));

}

namespace ores::accounts::messaging {

accounts_message_handler::accounts_message_handler(utility::repository::context ctx)
    : ctx_(std::move(ctx)) {}

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>, comms::protocol::error_code>>
accounts_message_handler::handle_message(comms::protocol::message_type type,
    std::span<const std::uint8_t> payload) {

    BOOST_LOG_SEV(lg, debug) << "Handling accounts message type "
                              << std::hex << static_cast<std::uint16_t>(type);

    switch (type) {
    case comms::protocol::message_type::create_account_request:
        co_return co_await handle_create_account_request(payload);
    case comms::protocol::message_type::list_accounts_request:
        co_return co_await handle_list_accounts_request(payload);
    default:
        BOOST_LOG_SEV(lg, error) << "Unknown accounts message type "
                                  << std::hex << static_cast<std::uint16_t>(type);
        co_return std::unexpected(comms::protocol::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>, comms::protocol::error_code>>
accounts_message_handler::handle_create_account_request(std::span<const std::uint8_t> payload) {
    BOOST_LOG_SEV(lg, debug) << "Processing create_account_request";

    // Deserialize request
    auto request_result = create_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg, error) << "Failed to deserialize create_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    try {
        // Create new account
        domain::account account;
        account.version = 1;
        account.modified_by = "system";
        account.id = boost::uuids::random_generator()();
        account.username = request.username;
        account.password_hash = request.password_hash;
        account.password_salt = request.password_salt;
        account.totp_secret = request.totp_secret;
        account.email = request.email;
        account.is_admin = request.is_admin;

        // Write account to repository
        std::vector<domain::account> accounts{account};
        account_repo_.write(ctx_, accounts);

        BOOST_LOG_SEV(lg, info) << "Created account with ID: " << account.id
                                 << " for username: " << account.username;

        // Create and serialize response
        create_account_response response{account.id};
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Exception while creating account: " << e.what();
        co_return std::unexpected(comms::protocol::error_code::network_error);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>, comms::protocol::error_code>>
accounts_message_handler::handle_list_accounts_request(std::span<const std::uint8_t> payload) {
    BOOST_LOG_SEV(lg, debug) << "Processing list_accounts_request";

    // Deserialize request
    auto request_result = list_accounts_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg, error) << "Failed to deserialize list_accounts_request";
        co_return std::unexpected(request_result.error());
    }

    try {
        // Retrieve accounts from repository
        auto accounts = account_repo_.read_latest(ctx_);
        BOOST_LOG_SEV(lg, info) << "Retrieved " << accounts.size() << " accounts";

        // Create and serialize response
        list_accounts_response response{std::move(accounts)};
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Exception while retrieving accounts: " << e.what();
        co_return std::unexpected(comms::protocol::error_code::network_error);
    }
}

}
