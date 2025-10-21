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
#include "ores.accounts/service/account_service.hpp"
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

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                     comms::protocol::error_code>>
accounts_message_handler::handle_message(comms::protocol::message_type type,
    std::span<const std::uint8_t> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg, debug) << "Handling accounts message type "
                              << std::hex << static_cast<std::uint16_t>(type);

    switch (type) {
    case comms::protocol::message_type::create_account_request:
        co_return co_await handle_create_account_request(payload);
    case comms::protocol::message_type::list_accounts_request:
        co_return co_await handle_list_accounts_request(payload);
    case comms::protocol::message_type::login_request:
        co_return co_await handle_login_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg, error) << "Unknown accounts message type "
                                  << std::hex << static_cast<std::uint16_t>(type);
        co_return std::unexpected(comms::protocol::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>,
                                     comms::protocol::error_code>>
accounts_message_handler::
handle_create_account_request(std::span<const std::uint8_t> payload) {
    BOOST_LOG_SEV(lg, debug) << "Processing create_account_request";

    auto request_result = create_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg, error) << "Failed to deserialize create_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg, debug) << "Request: " << request;

    try {
        service::account_service s(account_repo_, logins_repo_);
        domain::account account = s.create_account(ctx_, request.username, request.email,
            request.password, request.modified_by, request.is_admin);

        BOOST_LOG_SEV(lg, info) << "Created account with ID: " << account.id
                                 << " for username: " << account.username;

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

boost::asio::awaitable<std::expected<std::vector<std::uint8_t>, comms::protocol::error_code>>
accounts_message_handler::handle_login_request(std::span<const std::uint8_t> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg, debug) << "Processing login_request from " << remote_address;

    // Deserialize request
    auto request_result = login_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg, error) << "Failed to deserialize login_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg, debug) << "Request: " << request;

    try {
        // Extract IP address from remote_address (format is "IP:port")
        auto colon_pos = remote_address.find(':');
        std::string ip_string = (colon_pos != std::string::npos) ?
            remote_address.substr(0, colon_pos) : remote_address;

        // Parse IP address from string
        boost::asio::ip::address ip_address = boost::asio::ip::make_address(ip_string);

        // Attempt login
        service::account_service s(account_repo_, logins_repo_);
        domain::account account = s.login(ctx_, request.username, request.password, ip_address);

        BOOST_LOG_SEV(lg, info) << "Successful login for username: " << account.username
                                 << " from IP: " << ip_address;

        // Create successful response
        login_response response{
            .success = true,
            .error_message = "",
            .account_id = account.id,
            .username = account.username,
            .is_admin = account.is_admin
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, warn) << "Login failed: " << e.what();

        // Create failed response
        login_response response{
            .success = false,
            .error_message = e.what(),
            .account_id = boost::uuids::nil_uuid(),
            .username = "",
            .is_admin = false
        };
        co_return response.serialize();
    }
}

}
