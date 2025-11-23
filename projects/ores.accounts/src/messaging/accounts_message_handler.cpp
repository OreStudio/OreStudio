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
#include "ores.accounts/messaging/accounts_message_handler.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.accounts/messaging/protocol.hpp"

namespace ores::accounts::messaging {

using namespace ores::utility::log;
using comms::protocol::message_type;

accounts_message_handler::accounts_message_handler(utility::repository::context ctx)
    : service_(ctx) {}

accounts_message_handler::handler_result
accounts_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling accounts message type: "
                               << std::hex << static_cast<std::uint16_t>(type);

    switch (type) {
    case message_type::create_account_request:
        co_return co_await handle_create_account_request(payload);
    case message_type::list_accounts_request:
        co_return co_await handle_list_accounts_request(payload);
    case message_type::list_login_info_request:
        co_return co_await handle_list_login_info_request(payload);
    case message_type::list_feature_flags_request:
        co_return co_await handle_list_feature_flags_request(payload);
    case message_type::login_request:
        co_return co_await handle_login_request(payload, remote_address);
    case message_type::unlock_account_request:
        co_return co_await handle_unlock_account_request(payload);
    case message_type::delete_account_request:
        co_return co_await handle_delete_account_request(payload);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown accounts message type: "
                                   << std::hex << static_cast<std::uint16_t>(type);
        co_return std::unexpected(comms::protocol::error_code::invalid_message_type);
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_create_account_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing create_account_request.";

    auto request_result = create_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize create_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    domain::account account =
        service_.create_account( request.username, request.email,
        request.password,request.modified_by, request.is_admin);

    BOOST_LOG_SEV(lg(), info) << "Created account with ID: " << account.id
                              << " for username: " << account.username;

    create_account_response response{account.id};
    co_return response.serialize();
}

accounts_message_handler::handler_result
accounts_message_handler::accounts_message_handler::
handle_list_accounts_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_accounts_request.";

    auto request_result = list_accounts_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_accounts_request.";
        co_return std::unexpected(request_result.error());
    }

    auto accounts = service_.list_accounts();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << accounts.size()
                              << " accounts.";

    list_accounts_response response{std::move(accounts)};
    co_return response.serialize();
}

accounts_message_handler::handler_result
accounts_message_handler::
handle_list_login_info_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_login_info_request.";

    auto request_result = list_login_info_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_login_info_request.";
        co_return std::unexpected(request_result.error());
    }

    auto login_infos = service_.list_login_info();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << login_infos.size()
                              << " login info records.";

    list_login_info_response response{std::move(login_infos)};
    co_return response.serialize();
}

accounts_message_handler::handler_result
accounts_message_handler::
handle_list_feature_flags_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_feature_flags_request.";

    auto request_result = list_feature_flags_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_feature_flags_request.";
        co_return std::unexpected(request_result.error());
    }

    auto feature_flags = service_.list_feature_flags();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << feature_flags.size()
                              << " feature flags.";

    list_feature_flags_response response{std::move(feature_flags)};
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_login_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing login_request from "
                               << remote_address;

    auto request_result = login_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize login_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    try {
        // Extract the IP address.
        std::string_view ip_view(remote_address);
        auto colon_pos = ip_view.find(':');
        if (colon_pos != ip_view.npos)
            ip_view = ip_view.substr(0, colon_pos);

        using namespace boost::asio::ip;
        const address ip_address = make_address(std::string(ip_view));

        domain::account account = service_.login(request.username,
            request.password, ip_address);

        BOOST_LOG_SEV(lg(), info) << "Successful login for username: "
                                  << account.username
                                  << " from IP: " << ip_address;

        login_response response{
            .success = true,
            .error_message = "",
            .account_id = account.id,
            .username = account.username,
            .is_admin = account.is_admin
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: " << e.what();

        login_response response{
            .success = false,
            .error_message = e.what(),
            .account_id = boost::uuids::nil_uuid(),
            .username = request.username,
            .is_admin = false
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_unlock_account_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing unlock_account_request";

    auto request_result = unlock_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize unlock_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    try {
        service_.unlock_account(request.account_id);

        BOOST_LOG_SEV(lg(), info) << "Successfully unlocked account: "
                                  << boost::uuids::to_string(request.account_id);

        unlock_account_response response{ .success = true };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to unlock account: " << e.what();

        unlock_account_response response{
            .success = false,
            .error_message = e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_delete_account_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_account_request";

    auto request_result = delete_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    try {
        service_.delete_account(request.account_id);

        BOOST_LOG_SEV(lg(), info) << "Successfully deleted account: "
                                  << boost::uuids::to_string(request.account_id);

        delete_account_response response{
            .success = true,
            .message = "Account deleted successfully"
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete account: " << e.what();

        delete_account_response response{
            .success = false,
            .message = std::string("Failed to delete account: ") + e.what()
        };
        co_return response.serialize();
    }
}

}
