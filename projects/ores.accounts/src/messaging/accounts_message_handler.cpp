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
    : service_(ctx), ctx_(ctx) {}

accounts_message_handler::handler_result
accounts_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling accounts message type " << type;

    // Check bootstrap mode - only allow bootstrap endpoints
    const bool in_bootstrap = ctx_.is_in_bootstrap_mode();
    const bool is_bootstrap_endpoint =
        type == message_type::create_initial_admin_request ||
        type == message_type::bootstrap_status_request;

    if (in_bootstrap && !is_bootstrap_endpoint) {
        BOOST_LOG_SEV(lg(), warn)
            << "Blocked operation " << type << " - system in bootstrap mode";
        co_return std::unexpected(comms::protocol::error_code::bootstrap_mode_only);
    }

    if (!in_bootstrap && type == message_type::create_initial_admin_request) {
        BOOST_LOG_SEV(lg(), warn)
            << "Blocked create_initial_admin_request - system not in bootstrap mode";
        co_return std::unexpected(comms::protocol::error_code::bootstrap_mode_forbidden);
    }

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
    case message_type::create_initial_admin_request:
        co_return co_await handle_create_initial_admin_request(payload, remote_address);
    case message_type::bootstrap_status_request:
        co_return co_await handle_bootstrap_status_request(payload);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown accounts message type " << type;
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

    const auto& request = *request_result;

    // Validate pagination parameters
    constexpr std::uint32_t max_limit = 1000;
    if (request.limit == 0 || request.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(comms::protocol::error_code::invalid_request);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching accounts with offset: "
                               << request.offset << ", limit: " << request.limit;

    // Get paginated accounts and total count
    auto accounts = service_.list_accounts(request.offset, request.limit);
    auto total_count = service_.get_total_account_count();

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << accounts.size()
                              << " accounts (total available: " << total_count << ").";

    list_accounts_response response{
        .accounts = std::move(accounts),
        .total_available_count = total_count
    };
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

bool accounts_message_handler::is_localhost(const std::string& remote_address) {
    return remote_address == "127.0.0.1" || remote_address == "::1" ||
        remote_address.starts_with("127.0.0.1:") ||
        remote_address.starts_with("::1") ||
        remote_address.starts_with("[::1]");
}

accounts_message_handler::handler_result accounts_message_handler::
handle_create_initial_admin_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing create_initial_admin_request from "
                               << remote_address;

    if (!is_localhost(remote_address)) {
        BOOST_LOG_SEV(lg(), warn)
            << "Rejected create_initial_admin_request from non-localhost: "
            << remote_address;
        create_initial_admin_response response{
            .success = false,
            .error_message = "Bootstrap endpoint only accessible from localhost",
            .account_id = boost::uuids::nil_uuid()
        };
        co_return response.serialize();
    }

    if (!ctx_.is_in_bootstrap_mode()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Rejected create_initial_admin_request: system not in bootstrap mode";
        create_initial_admin_response response{
            .success = false,
            .error_message = "System not in bootstrap mode - admin account already exists",
            .account_id = boost::uuids::nil_uuid()
        };
        co_return response.serialize();
    }

    auto request_result = create_initial_admin_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to deserialize create_initial_admin_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    try {
        domain::account account = service_.create_account(
            request.username,
            request.email,
            request.password,
            "bootstrap",
            true
        );

        // Exit bootstrap mode by updating feature flag
        service::bootstrap_mode_service bootstrap_svc(ctx_);
        bootstrap_svc.exit_bootstrap_mode();

        // Update context flag for current session
        ctx_.set_bootstrap_mode(false);

        BOOST_LOG_SEV(lg(), info)
            << "Created initial admin account with ID: " << account.id
            << " for username: " << account.username;
        BOOST_LOG_SEV(lg(), info) << "System transitioned to SECURE MODE";

        create_initial_admin_response response{
            .success = true,
            .error_message = "",
            .account_id = account.id
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to create initial admin account: " << e.what();

        create_initial_admin_response response{
            .success = false,
            .error_message = std::string("Failed to create admin account: ") + e.what(),
            .account_id = boost::uuids::nil_uuid()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_bootstrap_status_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing bootstrap_status_request";

    auto request_result = bootstrap_status_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to deserialize bootstrap_status_request";
        co_return std::unexpected(request_result.error());
    }

    const bool in_bootstrap = ctx_.is_in_bootstrap_mode();

    BOOST_LOG_SEV(lg(), debug) << "Bootstrap mode status: "
                               << (in_bootstrap ? "ACTIVE" : "INACTIVE");

    bootstrap_status_response response{
        .is_in_bootstrap_mode = in_bootstrap,
        .message = in_bootstrap
            ? "System in BOOTSTRAP MODE - awaiting initial admin account creation"
            : "System in SECURE MODE - admin account exists"
    };

    co_return response.serialize();
}

}
