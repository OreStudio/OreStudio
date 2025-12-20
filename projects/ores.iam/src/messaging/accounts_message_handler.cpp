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
#include "ores.iam/messaging/accounts_message_handler.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.iam/messaging/protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.iam/service/signup_service.hpp"

namespace ores::iam::messaging {

using namespace ores::utility::log;
using comms::messaging::message_type;

accounts_message_handler::accounts_message_handler(database::context ctx,
    std::shared_ptr<variability::service::system_flags_service> system_flags,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : service_(ctx), ctx_(ctx), system_flags_(std::move(system_flags)),
      sessions_(std::move(sessions)) {}

accounts_message_handler::handler_result
accounts_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling accounts message type " << type;

    // Check bootstrap mode - only allow bootstrap endpoints
    const bool in_bootstrap = system_flags_->is_bootstrap_mode_enabled();
    const bool is_bootstrap_endpoint =
        type == message_type::create_initial_admin_request ||
        type == message_type::bootstrap_status_request;

    if (in_bootstrap && !is_bootstrap_endpoint) {
        BOOST_LOG_SEV(lg(), warn)
            << "Blocked operation " << type << " - system in bootstrap mode";
        co_return std::unexpected(comms::messaging::error_code::bootstrap_mode_only);
    }

    if (!in_bootstrap && type == message_type::create_initial_admin_request) {
        BOOST_LOG_SEV(lg(), warn)
            << "Blocked create_initial_admin_request - system not in bootstrap mode";
        co_return std::unexpected(comms::messaging::error_code::bootstrap_mode_forbidden);
    }

    switch (type) {
    case message_type::create_account_request:
        co_return co_await handle_create_account_request(payload, remote_address);
    case message_type::list_accounts_request:
        co_return co_await handle_list_accounts_request(payload, remote_address);
    case message_type::list_login_info_request:
        co_return co_await handle_list_login_info_request(payload, remote_address);
    case message_type::login_request:
        co_return co_await handle_login_request(payload, remote_address);
    case message_type::logout_request:
        co_return co_await handle_logout_request(payload, remote_address);
    case message_type::lock_account_request:
        co_return co_await handle_lock_account_request(payload, remote_address);
    case message_type::unlock_account_request:
        co_return co_await handle_unlock_account_request(payload, remote_address);
    case message_type::delete_account_request:
        co_return co_await handle_delete_account_request(payload);
    case message_type::create_initial_admin_request:
        co_return co_await handle_create_initial_admin_request(payload, remote_address);
    case message_type::bootstrap_status_request:
        co_return co_await handle_bootstrap_status_request(payload);
    case message_type::update_account_request:
        co_return co_await handle_update_account_request(payload, remote_address);
    case message_type::get_account_history_request:
        co_return co_await handle_get_account_history_request(payload, remote_address);
    case message_type::reset_password_request:
        co_return co_await handle_reset_password_request(payload, remote_address);
    case message_type::change_password_request:
        co_return co_await handle_change_password_request(payload, remote_address);
    case message_type::update_my_email_request:
        co_return co_await handle_update_my_email_request(payload, remote_address);
    case message_type::signup_request:
        co_return co_await handle_signup_request(payload);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown accounts message type " << type;
        co_return std::unexpected(comms::messaging::error_code::invalid_message_type);
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_create_account_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing create_account_request from "
                               << remote_address;

    // Authorization is checked by dispatcher before reaching this handler

    auto request_result = create_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize create_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    domain::account account =
        service_.create_account( request.username, request.email,
        request.password, request.recorded_by, request.is_admin);

    BOOST_LOG_SEV(lg(), info) << "Created account with ID: " << account.id
                              << " for username: " << account.username;

    create_account_response response{account.id};
    co_return response.serialize();
}

accounts_message_handler::handler_result
accounts_message_handler::
handle_list_accounts_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_accounts_request from "
                               << remote_address;

    // Authorization is checked by dispatcher before reaching this handler

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
        co_return std::unexpected(comms::messaging::error_code::invalid_request);
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
handle_list_login_info_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_login_info_request from "
                               << remote_address;

    // Authorization is checked by dispatcher before reaching this handler

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

accounts_message_handler::handler_result accounts_message_handler::
handle_login_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing login_request from "
                               << remote_address;

    auto request_result = login_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Failed to deserialize login_request"
                                   << " from " << remote_address
                                   << ", payload_size: " << payload.size()
                                   << ", error: " << request_result.error();
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

        // Get login_info to check password_reset_required flag
        const auto login_info = service_.get_login_info(account.id);
        const bool password_reset_required = login_info.password_reset_required;

        BOOST_LOG_SEV(lg(), info) << "LOGIN SUCCESS: User '" << account.username
                                  << "' authenticated from IP: " << ip_address
                                  << ", account_id: " << account.id
                                  << ", is_admin: " << account.is_admin
                                  << ", password_reset_required: "
                                  << password_reset_required;

        // Store session for this client in the shared session service
        sessions_->store_session(remote_address, comms::service::session_info{
            .account_id = account.id,
            .is_admin = account.is_admin
        });

        login_response response{
            .success = true,
            .error_message = "",
            .account_id = account.id,
            .username = account.username,
            .email = account.email,
            .is_admin = account.is_admin,
            .password_reset_required = password_reset_required
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "LOGIN FAILURE: Authentication failed for username '"
                                  << request.username << "' from " << remote_address
                                  << ", reason: " << e.what();

        login_response response{
            .success = false,
            .error_message = e.what(),
            .account_id = boost::uuids::nil_uuid(),
            .username = request.username,
            .email = "",
            .is_admin = false,
            .password_reset_required = false
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_lock_account_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing lock_account_request from "
                               << remote_address;

    auto request_result = lock_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize lock_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the requester's session from shared session service
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Lock account denied: no active session for "
                                  << remote_address;
        // Return error for all requested accounts
        lock_account_response response;
        for (const auto& id : request.account_ids) {
            response.results.push_back({
                .account_id = id,
                .success = false,
                .message = "Authentication required to lock accounts"
            });
        }
        co_return response.serialize();
    }

    // Check if requester has admin privileges
    if (!session->is_admin) {
        BOOST_LOG_SEV(lg(), warn) << "Lock account denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " is not an admin";
        // Return error for all requested accounts
        lock_account_response response;
        for (const auto& id : request.account_ids) {
            response.results.push_back({
                .account_id = id,
                .success = false,
                .message = "Admin privileges required to lock accounts"
            });
        }
        co_return response.serialize();
    }

    // Process each account
    lock_account_response response;
    for (const auto& account_id : request.account_ids) {
        bool success = service_.lock_account(account_id);

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "Successfully locked account: "
                                      << boost::uuids::to_string(account_id);
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Failed to lock account: "
                                      << boost::uuids::to_string(account_id);
        }

        response.results.push_back({
            .account_id = account_id,
            .success = success,
            .message = success ? "" : "Account does not exist"
        });
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_unlock_account_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing unlock_account_request from "
                               << remote_address;

    auto request_result = unlock_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize unlock_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the requester's session from shared session service
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Unlock account denied: no active session for "
                                  << remote_address;
        // Return error for all requested accounts
        unlock_account_response response;
        for (const auto& id : request.account_ids) {
            response.results.push_back({
                .account_id = id,
                .success = false,
                .message = "Authentication required to unlock accounts"
            });
        }
        co_return response.serialize();
    }

    // Check if requester has admin privileges
    if (!session->is_admin) {
        BOOST_LOG_SEV(lg(), warn) << "Unlock account denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " is not an admin";
        // Return error for all requested accounts
        unlock_account_response response;
        for (const auto& id : request.account_ids) {
            response.results.push_back({
                .account_id = id,
                .success = false,
                .message = "Admin privileges required to unlock accounts"
            });
        }
        co_return response.serialize();
    }

    // Process each account
    unlock_account_response response;
    for (const auto& account_id : request.account_ids) {
        bool success = service_.unlock_account(account_id);

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "Successfully unlocked account: "
                                      << boost::uuids::to_string(account_id);
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Failed to unlock account: "
                                      << boost::uuids::to_string(account_id);
        }

        response.results.push_back({
            .account_id = account_id,
            .success = success,
            .message = success ? "" : "Account does not exist"
        });
    }

    co_return response.serialize();
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

    if (!system_flags_->is_bootstrap_mode_enabled()) {
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

        // Exit bootstrap mode - updates database and shared cache
        system_flags_->set_bootstrap_mode(false, "system");

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

    const bool in_bootstrap = system_flags_->is_bootstrap_mode_enabled();

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

accounts_message_handler::handler_result accounts_message_handler::
handle_logout_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing logout_request from "
                               << remote_address;

    // Deserialize empty request (for protocol compliance)
    auto request_result = logout_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize logout_request";
        co_return std::unexpected(request_result.error());
    }

    // Get account_id from session context (not from request payload)
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "No session found for: " << remote_address;
        logout_response response{
            .success = false,
            .message = "Not logged in"
        };
        co_return response.serialize();
    }

    const auto& account_id = session->account_id;
    BOOST_LOG_SEV(lg(), debug) << "Logging out account: "
                               << boost::uuids::to_string(account_id);

    try {
        service_.logout(account_id);

        // Remove session for this client from the shared session service
        sessions_->remove_session(remote_address);

        BOOST_LOG_SEV(lg(), info) << "Successfully logged out account: "
                                  << boost::uuids::to_string(account_id);

        logout_response response{
            .success = true,
            .message = "Logged out successfully"
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to logout account: " << e.what();

        logout_response response{
            .success = false,
            .message = e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_update_account_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing update_account_request from "
                               << remote_address;

    auto request_result = update_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize update_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the requester's session from shared session service
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Update account denied: no active session for "
                                  << remote_address;
        update_account_response response{
            .success = false,
            .error_message = "Authentication required to update accounts"
        };
        co_return response.serialize();
    }

    // Check if requester has admin privileges
    if (!session->is_admin) {
        BOOST_LOG_SEV(lg(), warn) << "Update account denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " is not an admin";
        update_account_response response{
            .success = false,
            .error_message = "Admin privileges required to update accounts"
        };
        co_return response.serialize();
    }

    try {
        bool success = service_.update_account(request.account_id,
            request.email, request.recorded_by, request.is_admin);

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "Successfully updated account: "
                                      << boost::uuids::to_string(request.account_id);
        }

        update_account_response response{
            .success = success,
            .error_message = success ? "" : "Failed to update account"
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to update account: " << e.what();

        update_account_response response{
            .success = false,
            .error_message = std::string("Failed to update account: ") + e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_account_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_account_history_request from "
                               << remote_address;

    auto request_result = get_account_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_account_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for account: " << request.username;

    get_account_history_response response;
    try {
        // Get all versions of the account from service
        auto accounts = service_.get_account_history(request.username);

        if (accounts.empty()) {
            response.success = false;
            response.message = "Account not found: " + request.username;
            BOOST_LOG_SEV(lg(), warn) << "No history found for account: "
                                      << request.username;
            co_return response.serialize();
        }

        // Convert accounts to account_version objects
        domain::account_version_history history;
        history.username = request.username;

        // Sort by version descending (newest first) - use database version field
        std::sort(accounts.begin(), accounts.end(),
            [](const auto& a, const auto& b) {
                return a.version > b.version;
            });

        for (const auto& account : accounts) {
            domain::account_version version;
            version.data = account;
            version.version_number = account.version;  // Use database version field
            version.recorded_by = account.recorded_by;
            version.recorded_at = account.recorded_at;
            version.change_summary = "Version " + std::to_string(version.version_number);

            history.versions.push_back(std::move(version));
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.history = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                  << response.history.versions.size()
                                  << " versions for account: " << request.username;

    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve account history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for account "
                                   << request.username << ": " << e.what();
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_reset_password_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing reset_password_request from "
                               << remote_address;

    auto request_result = reset_password_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize reset_password_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the requester's session from shared session service
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Reset password denied: no active session for "
                                  << remote_address;
        // Return error for all requested accounts
        reset_password_response response;
        for (const auto& id : request.account_ids) {
            response.results.push_back({
                .account_id = id,
                .success = false,
                .message = "Authentication required to reset passwords"
            });
        }
        co_return response.serialize();
    }

    // Check if requester has admin privileges
    if (!session->is_admin) {
        BOOST_LOG_SEV(lg(), warn) << "Reset password denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " is not an admin";
        // Return error for all requested accounts
        reset_password_response response;
        for (const auto& id : request.account_ids) {
            response.results.push_back({
                .account_id = id,
                .success = false,
                .message = "Admin privileges required to reset passwords"
            });
        }
        co_return response.serialize();
    }

    // Process each account
    reset_password_response response;
    for (const auto& account_id : request.account_ids) {
        // Look up username for better logging
        std::string username = "<unknown>";
        if (auto account = service_.get_account(account_id)) {
            username = account->username;
        }

        bool success = service_.set_password_reset_required(account_id);

        if (success) {
            BOOST_LOG_SEV(lg(), info)
                << "Password reset: admin forced password reset for user '"
                << username << "' (account_id: "
                << boost::uuids::to_string(account_id) << ")";
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "Password reset failed: could not set reset flag for user '"
                << username << "' (account_id: "
                << boost::uuids::to_string(account_id) << ")";
        }

        response.results.push_back({
            .account_id = account_id,
            .success = success,
            .message = success ? "" : "Account does not exist"
        });
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_change_password_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing change_password_request from "
                               << remote_address;

    auto request_result = change_password_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize change_password_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    // Don't log password in request

    // Get the user's session from shared session service
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Change password denied: no active session for "
                                  << remote_address;
        change_password_response response{
            .success = false,
            .message = "Authentication required to change password"
        };
        co_return response.serialize();
    }

    const auto& account_id = session->account_id;

    // Look up username for better logging
    std::string username = "<unknown>";
    if (auto account = service_.get_account(account_id)) {
        username = account->username;
    }

    BOOST_LOG_SEV(lg(), debug) << "Change password: processing request for user '"
                               << username << "' (account_id: "
                               << boost::uuids::to_string(account_id) << ")";

    try {
        // Call service to change password (validates strength, hashes, clears flag)
        auto error = service_.change_password(account_id, request.new_password);

        if (!error.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "Change password failed: user '" << username
                << "' (account_id: " << boost::uuids::to_string(account_id)
                << ") - reason: " << error;
            change_password_response response{
                .success = false,
                .message = error
            };
            co_return response.serialize();
        }

        BOOST_LOG_SEV(lg(), info)
            << "Change password: user '" << username
            << "' successfully changed their password (account_id: "
            << boost::uuids::to_string(account_id) << ")";

        change_password_response response{
            .success = true,
            .message = "Password changed successfully"
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Change password error: exception for user '" << username
            << "' (account_id: " << boost::uuids::to_string(account_id)
            << "): " << e.what();
        change_password_response response{
            .success = false,
            .message = std::string("Failed to change password: ") + e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_update_my_email_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing update_my_email_request from "
                               << remote_address;

    auto request_result = update_my_email_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize update_my_email_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the user's session from shared session service
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Update email denied: no active session for "
                                  << remote_address;
        update_my_email_response response{
            .success = false,
            .message = "Authentication required to update email"
        };
        co_return response.serialize();
    }

    const auto& account_id = session->account_id;

    // Look up username for better logging
    std::string username = "<unknown>";
    if (auto account = service_.get_account(account_id)) {
        username = account->username;
    }

    BOOST_LOG_SEV(lg(), debug) << "Update email: processing request for user '"
                               << username << "' (account_id: "
                               << boost::uuids::to_string(account_id) << ")";

    try {
        auto error = service_.update_my_email(account_id, request.new_email);

        if (!error.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "Update email failed: user '" << username
                << "' (account_id: " << boost::uuids::to_string(account_id)
                << ") - reason: " << error;
            update_my_email_response response{
                .success = false,
                .message = error
            };
            co_return response.serialize();
        }

        BOOST_LOG_SEV(lg(), info)
            << "Update email: user '" << username
            << "' successfully updated their email to '" << request.new_email
            << "' (account_id: " << boost::uuids::to_string(account_id) << ")";

        update_my_email_response response{
            .success = true,
            .message = "Email updated successfully"
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Update email error: exception for user '" << username
            << "' (account_id: " << boost::uuids::to_string(account_id)
            << "): " << e.what();
        update_my_email_response response{
            .success = false,
            .message = std::string("Failed to update email: ") + e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_signup_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing signup_request";

    auto request_result = signup_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize signup_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Use signup_service which handles all validation and feature flag checks
    service::signup_service signup_svc(ctx_, system_flags_);
    auto result = signup_svc.register_user(request.username, request.email,
        request.password);

    if (result.success) {
        BOOST_LOG_SEV(lg(), info) << "Signup successful for username: "
                                  << result.username
                                  << ", account_id: " << result.account_id;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Signup failed for username: "
                                  << request.username
                                  << ", reason: " << result.error_message;
    }

    signup_response response{
        .success = result.success,
        .error_message = result.error_message,
        .account_id = result.account_id,
        .username = result.username
    };
    co_return response.serialize();
}

}
