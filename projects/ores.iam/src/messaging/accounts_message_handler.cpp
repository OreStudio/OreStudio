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

#include <format>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam/domain/permission.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.iam/messaging/protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/messaging/session_samples_protocol.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.iam/messaging/tenant_type_protocol.hpp"
#include "ores.iam/messaging/tenant_status_protocol.hpp"
#include "ores.iam/repository/account_party_repository.hpp"
#include "ores.iam/service/signup_service.hpp"
#include "ores.iam/service/session_converter.hpp"
#include "ores.iam/service/tenant_type_service.hpp"
#include "ores.iam/service/tenant_status_service.hpp"

namespace ores::iam::messaging {

using namespace ores::logging;
using comms::messaging::message_type;
namespace reason = database::domain::change_reason_constants;

namespace {

/**
 * @brief Parsed components of a user principal.
 */
struct parsed_principal {
    std::string username;
    std::string hostname;  ///< Empty if no hostname specified (system tenant)
};

/**
 * @brief Parse a principal string into username and hostname components.
 *
 * The principal format is `username@hostname` or just `username`.
 * If the principal contains `@`, the last `@` is used as the delimiter
 * (allowing usernames that contain `@` if needed in the future).
 *
 * @param principal The principal string to parse.
 * @return Parsed username and hostname. Hostname is empty if not specified.
 */
parsed_principal parse_principal(const std::string& principal) {
    const auto at_pos = principal.rfind('@');
    if (at_pos == std::string::npos) {
        // No @ found - entire string is username, use system tenant
        return {.username = principal, .hostname = ""};
    }
    // Split at last @
    return {
        .username = principal.substr(0, at_pos),
        .hostname = principal.substr(at_pos + 1)
    };
}

}  // anonymous namespace

accounts_message_handler::accounts_message_handler(database::context ctx,
    std::shared_ptr<variability::service::system_flags_service> system_flags,
    std::shared_ptr<comms::service::auth_session_service> sessions,
    std::shared_ptr<service::authorization_service> auth_service,
    std::shared_ptr<geo::service::geolocation_service> geo_service,
    bundle_provider_fn bundle_provider)
    : service_(ctx), ctx_(ctx), system_flags_(std::move(system_flags)),
      sessions_(std::move(sessions)), auth_service_(auth_service),
      setup_service_(service_, auth_service_),
      session_repo_(ctx), tenant_repo_(ctx), geo_service_(std::move(geo_service)),
      bundle_provider_(std::move(bundle_provider)) {}

database::context accounts_message_handler::make_request_context(
    const comms::service::session_info& session) const {
    return ctx_.with_tenant(session.tenant_id);
}

accounts_message_handler::handler_result
accounts_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling accounts message type " << type;

    // Drain any sample batches queued by record_sample() since the last request
    if (auto pending = sessions_->take_pending_samples(remote_address)) {
        auto& [session_id, samples] = *pending;
        try {
            session_repo_.insert_samples(session_id, samples);
            BOOST_LOG_SEV(lg(), debug) << "Periodic flush: inserted " << samples.size()
                                       << " samples for session "
                                       << boost::uuids::to_string(session_id);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to flush pending samples: " << e.what();
        }
    }

    // Check bootstrap mode - only allow bootstrap endpoints
    const bool in_bootstrap = system_flags_->is_bootstrap_mode_enabled();
    const bool is_bootstrap_endpoint =
        type == message_type::create_initial_admin_request ||
        type == message_type::bootstrap_status_request;

    if (in_bootstrap && !is_bootstrap_endpoint) {
        BOOST_LOG_SEV(lg(), warn)
            << "Blocked operation " << type << " - system in bootstrap mode";
        co_return std::unexpected(ores::utility::serialization::error_code::bootstrap_mode_only);
    }

    if (!in_bootstrap && type == message_type::create_initial_admin_request) {
        BOOST_LOG_SEV(lg(), warn)
            << "Blocked create_initial_admin_request - system not in bootstrap mode";
        co_return std::unexpected(ores::utility::serialization::error_code::bootstrap_mode_forbidden);
    }

    switch (type) {
    case message_type::save_account_request:
        co_return co_await handle_save_account_request(payload, remote_address);
    case message_type::get_accounts_request:
        co_return co_await handle_get_accounts_request(payload, remote_address);
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
        co_return co_await handle_delete_account_request(payload, remote_address);
    case message_type::create_initial_admin_request:
        co_return co_await handle_create_initial_admin_request(payload, remote_address);
    case message_type::bootstrap_status_request:
        co_return co_await handle_bootstrap_status_request(payload);
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
    // Session tracking messages
    case message_type::list_sessions_request:
        co_return co_await handle_list_sessions_request(payload, remote_address);
    case message_type::get_session_statistics_request:
        co_return co_await handle_get_session_statistics_request(payload, remote_address);
    case message_type::get_active_sessions_request:
        co_return co_await handle_get_active_sessions_request(payload, remote_address);
    case message_type::get_session_samples_request:
        co_return co_await handle_get_session_samples_request(payload, remote_address);
    // RBAC messages
    case message_type::list_roles_request:
        co_return co_await handle_list_roles_request(payload, remote_address);
    case message_type::list_permissions_request:
        co_return co_await handle_list_permissions_request(payload, remote_address);
    case message_type::get_role_request:
        co_return co_await handle_get_role_request(payload, remote_address);
    case message_type::assign_role_request:
        co_return co_await handle_assign_role_request(payload, remote_address);
    case message_type::revoke_role_request:
        co_return co_await handle_revoke_role_request(payload, remote_address);
    case message_type::get_account_roles_request:
        co_return co_await handle_get_account_roles_request(payload, remote_address);
    case message_type::get_account_permissions_request:
        co_return co_await handle_get_account_permissions_request(payload, remote_address);
    case message_type::suggest_role_commands_request:
        co_return co_await handle_suggest_role_commands_request(payload, remote_address);
    case message_type::assign_role_by_name_request:
        co_return co_await handle_assign_role_by_name_request(payload, remote_address);
    case message_type::revoke_role_by_name_request:
        co_return co_await handle_revoke_role_by_name_request(payload, remote_address);
    // Tenant management messages
    case message_type::get_tenants_request:
        co_return co_await handle_get_tenants_request(payload, remote_address);
    case message_type::save_tenant_request:
        co_return co_await handle_save_tenant_request(payload, remote_address);
    case message_type::delete_tenant_request:
        co_return co_await handle_delete_tenant_request(payload, remote_address);
    case message_type::get_tenant_history_request:
        co_return co_await handle_get_tenant_history_request(payload, remote_address);
    case message_type::provision_tenant_request:
        co_return co_await handle_provision_tenant_request(payload, remote_address);
    case message_type::get_tenant_types_request:
        co_return co_await handle_get_tenant_types_request(payload, remote_address);
    case message_type::save_tenant_type_request:
        co_return co_await handle_save_tenant_type_request(payload, remote_address);
    case message_type::delete_tenant_type_request:
        co_return co_await handle_delete_tenant_type_request(payload, remote_address);
    case message_type::get_tenant_type_history_request:
        co_return co_await handle_get_tenant_type_history_request(payload, remote_address);
    case message_type::get_tenant_statuses_request:
        co_return co_await handle_get_tenant_statuses_request(payload, remote_address);
    case message_type::save_tenant_status_request:
        co_return co_await handle_save_tenant_status_request(payload, remote_address);
    case message_type::delete_tenant_status_request:
        co_return co_await handle_delete_tenant_status_request(payload, remote_address);
    case message_type::get_tenant_status_history_request:
        co_return co_await handle_get_tenant_status_history_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown accounts message type " << type;
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_save_account_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_account_request from "
                               << remote_address;

    auto request_result = save_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Parse principal into username and hostname
    const auto [username, hostname] = parse_principal(request.principal);
    BOOST_LOG_SEV(lg(), debug) << "Parsed principal - username: " << username
                               << ", hostname: " << (hostname.empty() ? "(system)" : hostname);

    // Determine if this is a create or update based on account_id
    const bool is_create = request.account_id.is_nil();

    if (is_create) {
        // Create new account - requires accounts:create permission
        auto auth_result = check_authorization(remote_address,
            domain::permissions::accounts_create, "Create account");
        if (!auth_result) {
            co_return std::unexpected(auth_result.error());
        }

        try {
            // Create a fresh context for this request to avoid race conditions
            // with concurrent requests from different tenants.
            // Note: uses hostname if provided, otherwise uses handler's configured tenant.
            database::context operation_ctx = [&]() {
                if (!hostname.empty()) {
                    BOOST_LOG_SEV(lg(), debug) << "Looking up tenant by hostname: " << hostname;
                    const auto tenant_id =
                        database::service::tenant_context::lookup_by_hostname(ctx_, hostname);
                    return database::service::tenant_context::with_tenant(ctx_, tenant_id.to_string());
                } else {
                    BOOST_LOG_SEV(lg(), debug) << "No hostname in principal, using handler tenant: "
                                               << ctx_.tenant_id().to_string();
                    return ctx_.with_tenant(ctx_.tenant_id());
                }
            }();

            // Create temporary services with the correctly-scoped tenant context.
            // This ensures each concurrent request is isolated.
            service::account_service temp_account_svc(operation_ctx);
            service::account_setup_service temp_setup_svc(temp_account_svc, auth_service_);

            domain::account account =
                temp_setup_svc.create_account(username, request.email,
                request.password, auth_result->username, request.change_commentary);

            BOOST_LOG_SEV(lg(), info) << "Created account with ID: " << account.id
                                      << " for username: " << account.username
                                      << " with Viewer role assigned";

            save_account_response response{
                .success = true,
                .message = "",
                .account_id = account.id
            };
            co_return response.serialize();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to create account: " << e.what();
            save_account_response response{
                .success = false,
                .message = std::string("Failed to create account: ") + e.what(),
                .account_id = boost::uuids::nil_uuid()
            };
            co_return response.serialize();
        }
    } else {
        // Update existing account - requires accounts:update permission
        auto auth_result = check_authorization(remote_address,
            domain::permissions::accounts_update, "Update account");
        if (!auth_result) {
            co_return std::unexpected(auth_result.error());
        }

        try {
            // Create per-request context with session's tenant
            auto ctx = make_request_context(*auth_result);
            service::account_service svc(ctx);

            bool success = svc.update_account(request.account_id,
                request.email, auth_result->username,
                request.change_reason_code, request.change_commentary);

            if (success) {
                BOOST_LOG_SEV(lg(), info) << "Successfully updated account: "
                                          << boost::uuids::to_string(request.account_id);
            }

            save_account_response response{
                .success = success,
                .message = success ? "" : "Failed to update account",
                .account_id = request.account_id
            };
            co_return response.serialize();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to update account: " << e.what();
            save_account_response response{
                .success = false,
                .message = std::string("Failed to update account: ") + e.what(),
                .account_id = request.account_id
            };
            co_return response.serialize();
        }
    }
}

accounts_message_handler::handler_result
accounts_message_handler::
handle_get_accounts_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_accounts_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        domain::permissions::accounts_read, "List accounts");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_accounts_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_accounts_request.";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    // Validate pagination parameters
    constexpr std::uint32_t max_limit = 1000;
    if (request.limit == 0 || request.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_request);
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching accounts with offset: "
                               << request.offset << ", limit: " << request.limit;

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth_result);
    service::account_service svc(ctx);

    // Get paginated accounts and total count
    auto accounts = svc.list_accounts(request.offset, request.limit);
    auto total_count = svc.get_total_account_count();

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << accounts.size()
                              << " accounts (total available: " << total_count << ").";

    get_accounts_response response{
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

    auto auth_result = check_authorization(remote_address,
        domain::permissions::login_info_read, "List login info");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = list_login_info_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_login_info_request.";
        co_return std::unexpected(request_result.error());
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth_result);
    service::account_service svc(ctx);

    auto login_infos = svc.list_login_info();
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

    // Parse principal into username and hostname
    const auto [username, hostname] = parse_principal(request.principal);
    BOOST_LOG_SEV(lg(), debug) << "Parsed principal - username: " << username
                               << ", hostname: " << (hostname.empty() ? "(system)" : hostname);

    try {
        // Resolve tenant context based on hostname if provided.
        // Create a per-request context to avoid race conditions.
        utility::uuid::tenant_id tenant_id = ctx_.tenant_id();
        database::context login_ctx = [&]() {
            if (!hostname.empty()) {
                BOOST_LOG_SEV(lg(), debug) << "Looking up tenant by hostname: " << hostname;
                tenant_id = database::service::tenant_context::lookup_by_hostname(ctx_, hostname);
                return database::service::tenant_context::with_tenant(ctx_, tenant_id.to_string());
            } else {
                BOOST_LOG_SEV(lg(), debug) << "No hostname in principal, using handler tenant: "
                                           << ctx_.tenant_id().to_string();
                return ctx_.with_tenant(ctx_.tenant_id());
            }
        }();

        // Create temporary service for this login request
        service::account_service login_svc(login_ctx);

        // Extract the IP address.
        std::string_view ip_view(remote_address);
        auto colon_pos = ip_view.find(':');
        if (colon_pos != ip_view.npos)
            ip_view = ip_view.substr(0, colon_pos);

        using namespace boost::asio::ip;
        const address ip_address = make_address(std::string(ip_view));

        domain::account account = login_svc.login(username,
            request.password, ip_address);

        // Get login_info to check password_reset_required flag
        const auto login_info = login_svc.get_login_info(account.id);
        const bool password_reset_required = login_info.password_reset_required;

        BOOST_LOG_SEV(lg(), info) << "LOGIN SUCCESS: User '" << account.username
                                  << "' authenticated from IP: " << ip_address
                                  << ", account_id: " << account.id
                                  << ", password_reset_required: "
                                  << password_reset_required;

        // Create full session object for tracking
        auto sess = std::make_shared<domain::session>();
        sess->id = boost::uuids::random_generator()();
        sess->tenant_id = tenant_id;
        sess->account_id = account.id;
        sess->username = account.username;
        sess->start_time = std::chrono::system_clock::now();
        sess->client_ip = ip_address;

        // Resolve party context from account_parties
        try {
            repository::account_party_repository ap_repo(login_ctx);
            auto parties = ap_repo.read_latest_by_account(account.id);
            if (!parties.empty()) {
                sess->party_id = parties.front().party_id;

                // Compute visible party set via SQL function
                auto vis = database::repository::execute_parameterized_string_query(
                    login_ctx,
                    "SELECT unnest(ores_refdata_visible_party_ids_fn("
                    "$1::uuid, $2::uuid))::text",
                    {tenant_id.to_string(),
                     boost::uuids::to_string(sess->party_id)},
                    lg(), "Computing visible party set");
                for (const auto& s : vis) {
                    sess->visible_party_ids.push_back(
                        boost::lexical_cast<boost::uuids::uuid>(s));
                }

                BOOST_LOG_SEV(lg(), info) << "Party context resolved: party_id="
                                          << sess->party_id
                                          << ", visible_parties="
                                          << sess->visible_party_ids.size();
            } else {
                BOOST_LOG_SEV(lg(), debug) << "No party assignment for account: "
                                           << account.id;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to resolve party context: "
                                      << e.what();
            // Continue without party context - tenant-only isolation
        }

        // Retrieve client info stored during handshake
        auto client_info = sessions_->get_client_info(remote_address);
        if (client_info) {
            sess->client_identifier = client_info->client_identifier;
            sess->client_version_major = client_info->client_version_major;
            sess->client_version_minor = client_info->client_version_minor;
            BOOST_LOG_SEV(lg(), debug) << "Client info from handshake: "
                                       << sess->client_identifier << " v"
                                       << sess->client_version_major << "."
                                       << sess->client_version_minor;
        }

        // Perform geolocation lookup if service is available
        if (geo_service_) {
            auto geo_result = geo_service_->lookup(ip_address);
            if (geo_result) {
                sess->country_code = geo_result->country_code;
                BOOST_LOG_SEV(lg(), debug) << "Geolocation for " << ip_address
                                           << ": " << sess->country_code;
            } else {
                BOOST_LOG_SEV(lg(), debug) << "Geolocation lookup failed for "
                                           << ip_address << ": "
                                           << static_cast<int>(geo_result.error());
            }
        }

        // Persist session to database using per-request context
        try {
            repository::session_repository login_sess_repo(login_ctx);
            login_sess_repo.create(*sess);
            BOOST_LOG_SEV(lg(), debug) << "Session persisted to database: "
                                       << boost::uuids::to_string(sess->id);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to persist session to database: "
                                      << e.what();
            // Continue anyway - in-memory session tracking still works
        }

        // Store session in shared session service for authorization
        // Convert to comms::service::session_data for storage
        sessions_->store_session_data(remote_address,
            service::session_converter::to_session_data(*sess));

        // Look up tenant name
        const auto tenant_name =
            database::service::tenant_context::lookup_name(login_ctx, tenant_id);

        // Check if tenant is in bootstrap mode
        bool tenant_bootstrap = false;
        try {
            variability::service::system_flags_service tenant_flags(
                login_ctx, tenant_id.to_string());
            tenant_flags.refresh();
            tenant_bootstrap = tenant_flags.is_bootstrap_mode_enabled();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to check tenant bootstrap: " << e.what();
        }

        login_response response{
            .success = true,
            .error_message = "",
            .account_id = account.id,
            .tenant_id = tenant_id,
            .tenant_name = tenant_name,
            .username = account.username,
            .email = account.email,
            .password_reset_required = password_reset_required,
            .tenant_bootstrap_mode = tenant_bootstrap
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "LOGIN FAILURE: Authentication failed for principal '"
                                  << request.principal << "' from " << remote_address
                                  << ", reason: " << e.what();

        login_response response{
            .success = false,
            .error_message = e.what(),
            .account_id = boost::uuids::nil_uuid(),
            .tenant_id = utility::uuid::tenant_id::system(),
            .tenant_name = "",
            .username = username,
            .email = "",
            .password_reset_required = false,
            .tenant_bootstrap_mode = false
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

    // Check if requester has permission to lock accounts
    if (!auth_service_->has_permission(session->account_id,
            domain::permissions::accounts_lock)) {
        BOOST_LOG_SEV(lg(), warn) << "Lock account denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " lacks accounts:lock permission";
        // Return error for all requested accounts
        lock_account_response response;
        for (const auto& id : request.account_ids) {
            response.results.push_back({
                .account_id = id,
                .success = false,
                .message = "Permission denied: accounts:lock required"
            });
        }
        co_return response.serialize();
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    service::account_service svc(ctx);

    // Process each account
    lock_account_response response;
    for (const auto& account_id : request.account_ids) {
        bool success = svc.lock_account(account_id);

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

    // Check if requester has permission to unlock accounts
    if (!auth_service_->has_permission(session->account_id,
            domain::permissions::accounts_unlock)) {
        BOOST_LOG_SEV(lg(), warn) << "Unlock account denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " lacks accounts:unlock permission";
        // Return error for all requested accounts
        unlock_account_response response;
        for (const auto& id : request.account_ids) {
            response.results.push_back({
                .account_id = id,
                .success = false,
                .message = "Permission denied: accounts:unlock required"
            });
        }
        co_return response.serialize();
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    service::account_service svc(ctx);

    // Process each account
    unlock_account_response response;
    for (const auto& account_id : request.account_ids) {
        bool success = svc.unlock_account(account_id);

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
handle_delete_account_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_account_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        domain::permissions::accounts_delete, "Delete account");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = delete_account_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_account_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth_result);
    service::account_service svc(ctx);

    try {
        svc.delete_account(request.account_id);

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

accounts_message_handler::auth_check_result
accounts_message_handler::get_authenticated_session(
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

accounts_message_handler::auth_check_result
accounts_message_handler::check_authorization(
    const std::string& remote_address,
    std::string_view permission,
    std::string_view operation_name) {

    auto session_result = get_authenticated_session(remote_address, operation_name);
    if (!session_result) {
        return session_result;
    }

    const auto& session = *session_result;
    if (!auth_service_->has_permission(session.account_id, std::string(permission))) {
        BOOST_LOG_SEV(lg(), warn) << operation_name << " denied: requester "
                                  << boost::uuids::to_string(session.account_id)
                                  << " lacks " << permission << " permission";
        return std::unexpected(ores::utility::serialization::error_code::authorization_failed);
    }

    return session;
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
            .account_id = boost::uuids::nil_uuid(),
            .tenant_id = utility::uuid::tenant_id::system(),
            .tenant_name = ""
        };
        co_return response.serialize();
    }

    if (!system_flags_->is_bootstrap_mode_enabled()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Rejected create_initial_admin_request: system not in bootstrap mode";
        create_initial_admin_response response{
            .success = false,
            .error_message = "System not in bootstrap mode - admin account already exists",
            .account_id = boost::uuids::nil_uuid(),
            .tenant_id = utility::uuid::tenant_id::system(),
            .tenant_name = ""
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

    // Parse principal into username and hostname
    const auto [username, hostname] = parse_principal(request.principal);
    BOOST_LOG_SEV(lg(), debug) << "Parsed principal - username: " << username
                               << ", hostname: " << (hostname.empty() ? "(system)" : hostname);

    try {
        // Resolve tenant context based on hostname.
        // If hostname is empty, use handler's configured tenant.
        // Create a per-request context to avoid race conditions.
        utility::uuid::tenant_id tenant_id = ctx_.tenant_id();
        database::context signup_ctx = [&]() {
            if (hostname.empty()) {
                BOOST_LOG_SEV(lg(), debug) << "No hostname in principal, using handler tenant: "
                                           << ctx_.tenant_id().to_string();
                return ctx_.with_tenant(ctx_.tenant_id());
            } else {
                BOOST_LOG_SEV(lg(), debug) << "Looking up tenant by hostname: " << hostname;
                tenant_id = database::service::tenant_context::lookup_by_hostname(ctx_, hostname);
                return database::service::tenant_context::with_tenant(ctx_, tenant_id.to_string());
            }
        }();

        // Create temporary services for this signup request
        service::account_service signup_account_svc(signup_ctx);
        service::account_setup_service signup_setup_svc(signup_account_svc, auth_service_);

        // Create the initial admin account with SuperAdmin role
        // This checks role exists BEFORE creating account to avoid orphaned accounts
        domain::account account = signup_setup_svc.create_account_with_role(
            username,
            request.email,
            request.password,
            ctx_.credentials().user,
            domain::roles::super_admin,
            "Initial SuperAdmin account created during system bootstrap"
        );

        // Exit bootstrap mode - updates database and shared cache
        system_flags_->set_bootstrap_mode(false, ctx_.credentials().user,
            std::string{reason::codes::new_record}, "Bootstrap mode disabled after initial admin account created");

        // Look up tenant name
        const auto tenant_name =
            database::service::tenant_context::lookup_name(signup_ctx, tenant_id);

        BOOST_LOG_SEV(lg(), info)
            << "Created initial admin account with ID: " << account.id
            << " for username: " << account.username
            << " in tenant: " << tenant_name << " (" << tenant_id << ")";
        BOOST_LOG_SEV(lg(), info) << "System transitioned to SECURE MODE";

        create_initial_admin_response response{
            .success = true,
            .error_message = "",
            .account_id = account.id,
            .tenant_id = tenant_id,
            .tenant_name = tenant_name
        };
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to create initial admin account: " << e.what();

        create_initial_admin_response response{
            .success = false,
            .error_message = std::string("Failed to create admin account: ") + e.what(),
            .account_id = boost::uuids::nil_uuid(),
            .tenant_id = utility::uuid::tenant_id::system(),
            .tenant_name = ""
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
            : "System in SECURE MODE - admin account exists",
        .available_bundles = {}
    };

    if (in_bootstrap && bundle_provider_) {
        response.available_bundles = bundle_provider_();
        BOOST_LOG_SEV(lg(), debug) << "Included " << response.available_bundles.size()
                                   << " available bundles in bootstrap response";
    }

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

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    service::account_service svc(ctx);
    repository::session_repository sess_repo(ctx);

    try {
        svc.logout(account_id);

        // Remove session and persist end state to database
        // Also clean up client info stored during handshake
        sessions_->remove_client_info(remote_address);
        auto sess = sessions_->remove_session(remote_address);
        if (sess) {
            sess->end_time = std::chrono::system_clock::now();
            try {
                sess_repo.end_session(sess->id, sess->start_time,
                    *sess->end_time, sess->bytes_sent, sess->bytes_received);
                BOOST_LOG_SEV(lg(), debug) << "Session end persisted to database: "
                                           << boost::uuids::to_string(sess->id)
                                           << ", bytes_sent: " << sess->bytes_sent
                                           << ", bytes_received: " << sess->bytes_received;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "Failed to persist session end to database: "
                                          << e.what();
            }
            // Merge any pending flush batch with remaining accumulator samples
            auto all_samples = std::move(sess->flush_pending);
            all_samples.insert(all_samples.end(),
                std::make_move_iterator(sess->samples.begin()),
                std::make_move_iterator(sess->samples.end()));
            if (!all_samples.empty()) {
                try {
                    sess_repo.insert_samples(sess->id, all_samples);
                    BOOST_LOG_SEV(lg(), debug) << "Inserted " << all_samples.size()
                                               << " samples at logout for session: "
                                               << boost::uuids::to_string(sess->id);
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn) << "Failed to persist session samples: "
                                              << e.what();
                }
            }
        }

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
handle_get_account_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_account_history_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        domain::permissions::accounts_read, "Get account history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = get_account_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_account_history_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for account: " << request.username;

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth_result);
    service::account_service svc(ctx);

    get_account_history_response response;
    try {
        // Get all versions of the account from service
        auto accounts = svc.get_account_history(request.username);

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
            version.modified_by = account.modified_by;
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

    // Check if requester has permission to reset passwords
    if (!auth_service_->has_permission(session->account_id,
            domain::permissions::accounts_reset_password)) {
        BOOST_LOG_SEV(lg(), warn) << "Reset password denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " lacks accounts:reset_password permission";
        // Return error for all requested accounts
        reset_password_response response;
        for (const auto& id : request.account_ids) {
            response.results.push_back({
                .account_id = id,
                .success = false,
                .message = "Permission denied: accounts:reset_password required"
            });
        }
        co_return response.serialize();
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    service::account_service svc(ctx);

    // Process each account
    reset_password_response response;
    for (const auto& account_id : request.account_ids) {
        // Look up username for better logging
        std::string username = "<unknown>";
        if (auto account = svc.get_account(account_id)) {
            username = account->username;
        }

        bool success = svc.set_password_reset_required(account_id);

        if (success) {
            BOOST_LOG_SEV(lg(), info)
                << "Password reset: forced password reset for user '"
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

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    service::account_service svc(ctx);

    // Look up username for better logging
    std::string username = "<unknown>";
    if (auto account = svc.get_account(account_id)) {
        username = account->username;
    }

    BOOST_LOG_SEV(lg(), debug) << "Change password: processing request for user '"
                               << username << "' (account_id: "
                               << boost::uuids::to_string(account_id) << ")";

    try {
        // Call service to change password (validates strength, hashes, clears flag)
        auto error = svc.change_password(account_id, request.new_password);

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

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    service::account_service svc(ctx);

    // Look up username for better logging
    std::string username = "<unknown>";
    if (auto account = svc.get_account(account_id)) {
        username = account->username;
    }

    BOOST_LOG_SEV(lg(), debug) << "Update email: processing request for user '"
                               << username << "' (account_id: "
                               << boost::uuids::to_string(account_id) << ")";

    try {
        auto error = svc.update_my_email(account_id, request.new_email);

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
    service::signup_service signup_svc(ctx_, system_flags_, auth_service_);
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

// ============================================================================
// RBAC Handlers
// ============================================================================

accounts_message_handler::handler_result accounts_message_handler::
handle_list_roles_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_roles_request from "
                               << remote_address;

    auto request_result = list_roles_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_roles_request";
        co_return std::unexpected(request_result.error());
    }

    auto roles = auth_service_->list_roles();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << roles.size() << " roles.";

    list_roles_response response{.roles = std::move(roles)};
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_list_permissions_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_permissions_request from "
                               << remote_address;

    auto request_result = list_permissions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_permissions_request";
        co_return std::unexpected(request_result.error());
    }

    auto permissions = auth_service_->list_permissions();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << permissions.size() << " permissions.";

    list_permissions_response response{.permissions = std::move(permissions)};
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_role_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_role_request from "
                               << remote_address;

    auto request_result = get_role_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_role_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Looking up role: " << request.identifier;

    // Try to parse as UUID first
    std::optional<domain::role> found_role;
    try {
        auto role_id = boost::lexical_cast<boost::uuids::uuid>(request.identifier);
        found_role = auth_service_->find_role(role_id);
    } catch (const boost::bad_lexical_cast&) {
        // Not a UUID, try by name
        found_role = auth_service_->find_role_by_name(request.identifier);
    }

    get_role_response response;
    if (found_role) {
        BOOST_LOG_SEV(lg(), info) << "Found role: " << found_role->name;
        response.found = true;
        response.role = std::move(*found_role);
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Role not found: " << request.identifier;
        response.found = false;
        response.error_message = "Role not found: " + request.identifier;
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_assign_role_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing assign_role_request from "
                               << remote_address;

    auto request_result = assign_role_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize assign_role_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the requester's session from shared session service
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Assign role denied: no active session for "
                                  << remote_address;
        assign_role_response response{
            .success = false,
            .error_message = "Authentication required to assign roles"
        };
        co_return response.serialize();
    }

    // Check if requester has permission to assign roles
    if (!auth_service_->has_permission(session->account_id,
            domain::permissions::roles_assign)) {
        BOOST_LOG_SEV(lg(), warn) << "Assign role denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " lacks roles:assign permission";
        assign_role_response response{
            .success = false,
            .error_message = "Permission denied: roles:assign required"
        };
        co_return response.serialize();
    }

    try {
        auth_service_->assign_role(request.account_id, request.role_id,
            boost::uuids::to_string(session->account_id));

        BOOST_LOG_SEV(lg(), info) << "Assigned role "
                                  << boost::uuids::to_string(request.role_id)
                                  << " to account "
                                  << boost::uuids::to_string(request.account_id);

        assign_role_response response{.success = true, .error_message = ""};
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to assign role: " << e.what();
        assign_role_response response{
            .success = false,
            .error_message = std::string("Failed to assign role: ") + e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_revoke_role_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing revoke_role_request from "
                               << remote_address;

    auto request_result = revoke_role_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize revoke_role_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the requester's session from shared session service
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Revoke role denied: no active session for "
                                  << remote_address;
        revoke_role_response response{
            .success = false,
            .error_message = "Authentication required to revoke roles"
        };
        co_return response.serialize();
    }

    // Check if requester has permission to revoke roles
    if (!auth_service_->has_permission(session->account_id,
            domain::permissions::roles_revoke)) {
        BOOST_LOG_SEV(lg(), warn) << "Revoke role denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " lacks roles:revoke permission";
        revoke_role_response response{
            .success = false,
            .error_message = "Permission denied: roles:revoke required"
        };
        co_return response.serialize();
    }

    try {
        auth_service_->revoke_role(request.account_id, request.role_id);

        BOOST_LOG_SEV(lg(), info) << "Revoked role "
                                  << boost::uuids::to_string(request.role_id)
                                  << " from account "
                                  << boost::uuids::to_string(request.account_id);

        revoke_role_response response{.success = true, .error_message = ""};
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to revoke role: " << e.what();
        revoke_role_response response{
            .success = false,
            .error_message = std::string("Failed to revoke role: ") + e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_account_roles_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_account_roles_request from "
                               << remote_address;

    auto request_result = get_account_roles_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_account_roles_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Getting roles for account: "
                               << boost::uuids::to_string(request.account_id);

    auto roles = auth_service_->get_account_roles(request.account_id);
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << roles.size() << " roles for account "
                              << boost::uuids::to_string(request.account_id);

    get_account_roles_response response{.roles = std::move(roles)};
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_account_permissions_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_account_permissions_request from "
                               << remote_address;

    auto request_result = get_account_permissions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_account_permissions_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Getting permissions for account: "
                               << boost::uuids::to_string(request.account_id);

    auto permissions = auth_service_->get_effective_permissions(request.account_id);
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << permissions.size()
                              << " permissions for account "
                              << boost::uuids::to_string(request.account_id);

    get_account_permissions_response response{.permission_codes = std::move(permissions)};
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_suggest_role_commands_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing suggest_role_commands_request from "
                               << remote_address;

    auto request_result = suggest_role_commands_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize suggest_role_commands_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Suggesting role commands for user: "
                               << request.username
                               << ", hostname: " << request.hostname
                               << ", tenant_id: " << request.tenant_id;

    // Build the parameterized SQL query to prevent SQL injection
    std::string sql;
    std::vector<std::string> params;

    if (!request.tenant_id.empty()) {
        sql = "SELECT command FROM ores_iam_generate_role_commands_fn($1, NULL, $2::uuid)";
        params = {request.username, request.tenant_id};
    } else if (!request.hostname.empty()) {
        sql = "SELECT command FROM ores_iam_generate_role_commands_fn($1, $2)";
        params = {request.username, request.hostname};
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Neither hostname nor tenant_id provided";
        suggest_role_commands_response response{
            .commands = {"# ERROR: Must provide either hostname or tenant_id"}
        };
        co_return response.serialize();
    }

    try {
        auto rows = database::repository::execute_parameterized_string_query(
            ctx_, sql, params, lg(), "Generating role commands");

        suggest_role_commands_response response{.commands = std::move(rows)};
        BOOST_LOG_SEV(lg(), info) << "Generated " << response.commands.size()
                                  << " role commands for " << request.username;
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error generating role commands: " << e.what();
        suggest_role_commands_response response{
            .commands = {std::string("# ERROR: ") + e.what()}
        };
        co_return response.serialize();
    }
}

accounts_message_handler::resolve_result
accounts_message_handler::resolve_role_target(
    const std::string& principal, const std::string& role_name) {

    // Parse principal into username and hostname
    const auto [username, hostname] = parse_principal(principal);
    BOOST_LOG_SEV(lg(), debug) << "Parsed principal - username: " << username
                               << ", hostname: " << (hostname.empty() ? "(system)" : hostname);

    // Resolve tenant context based on hostname
    database::context resolve_ctx = [&]() {
        if (!hostname.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Looking up tenant by hostname: " << hostname;
            const auto tenant_id =
                database::service::tenant_context::lookup_by_hostname(ctx_, hostname);
            return database::service::tenant_context::with_tenant(ctx_, tenant_id.to_string());
        } else {
            return ctx_.with_tenant(ctx_.tenant_id());
        }
    }();

    // Resolve username to account
    service::account_service acct_svc(resolve_ctx);
    auto account = acct_svc.find_account_by_username(username);
    if (!account) {
        assign_role_response err{
            .success = false,
            .error_message = "Account not found: " + username
        };
        return std::unexpected(err.serialize());
    }

    // Resolve role name to role
    auto role = auth_service_->find_role_by_name(role_name);
    if (!role) {
        assign_role_response err{
            .success = false,
            .error_message = "Role not found: " + role_name
        };
        return std::unexpected(err.serialize());
    }

    return resolved_role_target{.account_id = account->id, .role_id = role->id};
}

accounts_message_handler::handler_result accounts_message_handler::
handle_assign_role_by_name_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing assign_role_by_name_request from "
                               << remote_address;

    auto request_result = assign_role_by_name_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize assign_role_by_name_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Assign role by name denied: no active session for "
                                  << remote_address;
        assign_role_response response{
            .success = false,
            .error_message = "Authentication required to assign roles"
        };
        co_return response.serialize();
    }

    if (!auth_service_->has_permission(session->account_id,
            domain::permissions::roles_assign)) {
        BOOST_LOG_SEV(lg(), warn) << "Assign role by name denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " lacks roles:assign permission";
        assign_role_response response{
            .success = false,
            .error_message = "Permission denied: roles:assign required"
        };
        co_return response.serialize();
    }

    try {
        auto target = resolve_role_target(request.principal, request.role_name);
        if (!target) {
            co_return std::move(target.error());
        }

        auth_service_->assign_role(target->account_id, target->role_id,
            boost::uuids::to_string(session->account_id));

        BOOST_LOG_SEV(lg(), info) << "Assigned role " << request.role_name
                                  << " to account " << request.principal;

        assign_role_response response{.success = true, .error_message = ""};
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to assign role by name: " << e.what();
        assign_role_response response{
            .success = false,
            .error_message = std::string("Failed to assign role: ") + e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_revoke_role_by_name_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing revoke_role_by_name_request from "
                               << remote_address;

    auto request_result = revoke_role_by_name_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize revoke_role_by_name_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Revoke role by name denied: no active session for "
                                  << remote_address;
        revoke_role_response response{
            .success = false,
            .error_message = "Authentication required to revoke roles"
        };
        co_return response.serialize();
    }

    if (!auth_service_->has_permission(session->account_id,
            domain::permissions::roles_revoke)) {
        BOOST_LOG_SEV(lg(), warn) << "Revoke role by name denied: requester "
                                  << boost::uuids::to_string(session->account_id)
                                  << " lacks roles:revoke permission";
        revoke_role_response response{
            .success = false,
            .error_message = "Permission denied: roles:revoke required"
        };
        co_return response.serialize();
    }

    try {
        auto target = resolve_role_target(request.principal, request.role_name);
        if (!target) {
            co_return std::move(target.error());
        }

        auth_service_->revoke_role(target->account_id, target->role_id);

        BOOST_LOG_SEV(lg(), info) << "Revoked role " << request.role_name
                                  << " from account " << request.principal;

        revoke_role_response response{.success = true, .error_message = ""};
        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to revoke role by name: " << e.what();
        revoke_role_response response{
            .success = false,
            .error_message = std::string("Failed to revoke role: ") + e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_list_sessions_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_sessions_request from "
                               << remote_address;

    auto request_result = list_sessions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_sessions_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the requester's session
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "List sessions denied: no active session for "
                                  << remote_address;
        co_return std::unexpected(ores::utility::serialization::error_code::authentication_failed);
    }

    // Determine which account's sessions to return
    boost::uuids::uuid target_account_id = request.account_id;
    const bool is_admin = auth_service_->has_permission(session->account_id,
        domain::permissions::accounts_read);
    if (target_account_id.is_nil()) {
        // Nil UUID means requesting own sessions
        target_account_id = session->account_id;
    } else if (!is_admin && target_account_id != session->account_id) {
        // Non-admin trying to view someone else's sessions
        BOOST_LOG_SEV(lg(), warn) << "List sessions denied: non-admin trying to view "
                                  << "sessions for account "
                                  << boost::uuids::to_string(target_account_id);
        co_return std::unexpected(ores::utility::serialization::error_code::authorization_failed);
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    repository::session_repository sess_repo(ctx);

    // Query sessions from database
    auto sessions_list = sess_repo.read_by_account(target_account_id,
        request.limit, request.offset);
    auto total_count = sess_repo.count_by_account(target_account_id);

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << sessions_list.size()
                              << " sessions for account "
                              << boost::uuids::to_string(target_account_id);

    list_sessions_response response{
        .sessions = std::move(sessions_list),
        .total_count = static_cast<std::uint32_t>(total_count)
    };
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_session_statistics_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_session_statistics_request from "
                               << remote_address;

    auto request_result = get_session_statistics_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_session_statistics_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the requester's session
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Get session statistics denied: no active session for "
                                  << remote_address;
        co_return std::unexpected(ores::utility::serialization::error_code::authentication_failed);
    }

    // Determine target account
    boost::uuids::uuid target_account_id = request.account_id;
    bool aggregate_mode = target_account_id.is_nil();
    const bool is_admin = auth_service_->has_permission(session->account_id,
        domain::permissions::accounts_read);

    // Non-admin can only view their own stats, not aggregate
    if (!is_admin) {
        if (aggregate_mode) {
            // Non-admin requesting aggregate - limit to their own account
            target_account_id = session->account_id;
            aggregate_mode = false;
        } else if (target_account_id != session->account_id) {
            BOOST_LOG_SEV(lg(), warn) << "Get session statistics denied: non-admin trying to view "
                                      << "statistics for account "
                                      << boost::uuids::to_string(target_account_id);
            co_return std::unexpected(ores::utility::serialization::error_code::authorization_failed);
        }
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    repository::session_repository sess_repo(ctx);

    // Query statistics from database
    std::vector<domain::session_statistics> stats;
    if (aggregate_mode) {
        // Admin requesting aggregate stats
        stats = sess_repo.read_aggregate_daily_statistics(
            request.start_time, request.end_time);
    } else {
        stats = sess_repo.read_daily_statistics(target_account_id,
            request.start_time, request.end_time);
    }

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << stats.size()
                              << " statistics records";

    get_session_statistics_response response{
        .statistics = std::move(stats)
    };
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_active_sessions_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_active_sessions_request from "
                               << remote_address;

    auto request_result = get_active_sessions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_active_sessions_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Request: " << request;

    // Get the requester's session
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Get active sessions denied: no active session for "
                                  << remote_address;
        co_return std::unexpected(ores::utility::serialization::error_code::authentication_failed);
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    repository::session_repository sess_repo(ctx);

    std::vector<domain::session> active_sessions;
    const bool is_admin = auth_service_->has_permission(session->account_id,
        domain::permissions::accounts_read);

    if (is_admin) {
        // Admin gets all active sessions
        active_sessions = sess_repo.read_all_active();
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << active_sessions.size()
                                  << " active sessions (admin view)";
    } else {
        // Non-admin gets only their own active sessions
        active_sessions = sess_repo.read_active_by_account(session->account_id);
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << active_sessions.size()
                                  << " active sessions for account "
                                  << boost::uuids::to_string(session->account_id);
    }

    get_active_sessions_response response{
        .sessions = std::move(active_sessions)
    };
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_session_samples_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_session_samples_request from "
                               << remote_address;

    auto request_result = get_session_samples_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_session_samples_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    const auto session_id_str = boost::uuids::to_string(request.session_id);

    // Get the requester's session
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Get session samples denied: no active session for "
                                  << remote_address;
        get_session_samples_response response{
            .success = false,
            .message = "Not authenticated"
        };
        co_return response.serialize();
    }

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);
    repository::session_repository sess_repo(ctx);

    // Verify the requester owns the session or is an admin
    const bool is_admin = auth_service_->has_permission(session->account_id,
        domain::permissions::accounts_read);

    if (!is_admin) {
        auto db_session = sess_repo.read(request.session_id);
        if (!db_session) {
            BOOST_LOG_SEV(lg(), warn) << "Get session samples: session not found: "
                                      << session_id_str;
            get_session_samples_response response{
                .success = false,
                .message = "Session not found"
            };
            co_return response.serialize();
        }

        if (db_session->account_id != session->account_id) {
            BOOST_LOG_SEV(lg(), warn) << "Get session samples denied: non-admin trying to "
                                      << "access samples for session " << session_id_str;
            get_session_samples_response response{
                .success = false,
                .message = "Not authorised to view samples for this session"
            };
            co_return response.serialize();
        }
    }

    try {
        auto samples = sess_repo.read_samples(request.session_id);

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << samples.size()
                                  << " samples for session " << session_id_str;

        get_session_samples_response response;
        response.success = true;
        response.samples.reserve(samples.size());

        for (const auto& s : samples) {
            const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
                s.timestamp.time_since_epoch()).count();
            session_sample_dto dto{
                .sample_time_ms = static_cast<std::uint64_t>(ms),
                .bytes_sent     = s.bytes_sent,
                .bytes_received = s.bytes_received,
                .latency_ms     = s.latency_ms
            };
            response.samples.push_back(std::move(dto));
        }

        co_return response.serialize();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to read session samples: " << e.what();
        get_session_samples_response response{
            .success = false,
            .message = e.what()
        };
        co_return response.serialize();
    }
}

// =============================================================================
// Tenant Management Handlers
// =============================================================================

accounts_message_handler::handler_result accounts_message_handler::
handle_get_tenants_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_tenants_request from "
                               << remote_address;

    auto request_result = get_tenants_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_tenants_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication and iam::tenants:read permission
    auto auth_result = check_authorization(remote_address, domain::permissions::tenants_read, "Get tenants");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    const auto& request = *request_result;

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth_result);
    repository::tenant_repository tenant_repo(ctx);

    try {
        std::vector<domain::tenant> tenants;
        if (request.include_deleted) {
            tenants = tenant_repo.read_all_latest();
            BOOST_LOG_SEV(lg(), info) << "Retrieved " << tenants.size()
                                      << " tenants (including deleted)";
        } else {
            tenants = tenant_repo.read_latest();
            BOOST_LOG_SEV(lg(), info) << "Retrieved " << tenants.size() << " tenants";
        }

        get_tenants_response response{
            .tenants = std::move(tenants)
        };
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get tenants: " << e.what();
        get_tenants_response response{
            .tenants = {}
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_save_tenant_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_tenant_request from "
                               << remote_address;

    auto request_result = save_tenant_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_tenant_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication and iam::tenants:update permission
    auto auth_result = check_authorization(remote_address, domain::permissions::tenants_update, "Save tenant");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    const auto& request = *request_result;
    auto tenant = request.tenant;
    // modified_by comes from the request; if empty, the database trigger sets it

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth_result);
    repository::tenant_repository tenant_repo(ctx);

    try {
        tenant_repo.write(tenant);
        BOOST_LOG_SEV(lg(), info) << "Saved tenant: " << tenant.name
                                  << " (code: " << tenant.code << ")";

        save_tenant_response response{
            .success = true,
            .message = "Tenant saved successfully"
        };
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save tenant: " << e.what();
        save_tenant_response response{
            .success = false,
            .message = std::string("Failed to save tenant: ") + e.what()
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_delete_tenant_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_tenant_request from "
                               << remote_address;

    auto request_result = delete_tenant_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_tenant_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication and iam::tenants:delete permission
    auto auth_result = check_authorization(remote_address, domain::permissions::tenants_delete, "Delete tenant");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    const auto& request = *request_result;

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth_result);
    repository::tenant_repository tenant_repo(ctx);

    std::vector<delete_tenant_result> results;
    results.reserve(request.ids.size());

    for (const auto& id : request.ids) {
        try {
            // Check if tenant exists
            auto existing = tenant_repo.read_latest(id);
            if (existing.empty()) {
                results.push_back({
                    .id = id,
                    .success = false,
                    .message = "Tenant not found"
                });
                continue;
            }

            // Prevent deletion of system tenant
            static const auto system_tenant_id = boost::uuids::uuid{};
            if (id == system_tenant_id) {
                results.push_back({
                    .id = id,
                    .success = false,
                    .message = "Cannot delete the system tenant"
                });
                continue;
            }

            tenant_repo.remove(id);
            BOOST_LOG_SEV(lg(), info) << "Deleted tenant: " << id;

            results.push_back({
                .id = id,
                .success = true,
                .message = "Tenant deleted successfully"
            });
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to delete tenant " << id
                                       << ": " << e.what();
            results.push_back({
                .id = id,
                .success = false,
                .message = std::string("Failed to delete tenant: ") + e.what()
            });
        }
    }

    delete_tenant_response response{
        .results = std::move(results)
    };
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_tenant_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_tenant_history_request from "
                               << remote_address;

    auto request_result = get_tenant_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_tenant_history_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication and iam::tenants:read permission
    auto auth_result = check_authorization(remote_address, domain::permissions::tenants_read, "Get tenant history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    const auto& request = *request_result;

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth_result);
    repository::tenant_repository tenant_repo(ctx);

    try {
        auto versions = tenant_repo.read_history(request.id);
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << versions.size()
                                  << " versions for tenant " << request.id;

        get_tenant_history_response response{
            .success = true,
            .message = "",
            .versions = std::move(versions)
        };
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get tenant history: " << e.what();
        get_tenant_history_response response{
            .success = false,
            .message = std::string("Failed to get tenant history: ") + e.what(),
            .versions = {}
        };
        co_return response.serialize();
    }
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_tenant_types_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_tenant_types_request from "
                               << remote_address;

    auto request_result = get_tenant_types_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_tenant_types_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication
    auto auth_result = get_authenticated_session(remote_address, "Get tenant types");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto ctx = make_request_context(*auth_result);
    service::tenant_type_service svc(ctx);

    auto types = svc.list_types();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << types.size() << " tenant types";

    get_tenant_types_response response{
        .types = std::move(types)
    };
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_save_tenant_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_tenant_type_request from "
                               << remote_address;

    auto request_result = save_tenant_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_tenant_type_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication and system admin permission
    auto auth_result = get_authenticated_session(remote_address, "Save tenant type");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto ctx = make_request_context(*auth_result);
    service::tenant_type_service svc(ctx);

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving tenant type: " << request.type.type;

    request.type.modified_by = auth_result->username;
    request.type.performed_by.clear();

    save_tenant_type_response response;
    try {
        svc.save_type(request.type);
        response.success = true;
        response.message = "Tenant type saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved tenant type: "
                                  << request.type.type << " by " << auth_result->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save tenant type: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving tenant type "
                                   << request.type.type << ": " << e.what();
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_delete_tenant_type_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_tenant_type_request from "
                               << remote_address;

    auto request_result = delete_tenant_type_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_tenant_type_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication and system admin permission
    auto auth_result = get_authenticated_session(remote_address, "Delete tenant type");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto ctx = make_request_context(*auth_result);
    service::tenant_type_service svc(ctx);

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.types.size()
                              << " tenant type(s)";

    delete_tenant_type_response response;
    for (const auto& type : request.types) {
        delete_tenant_type_result result;
        result.type = type;

        try {
            svc.remove_type(type);
            result.success = true;
            result.message = "Tenant type deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted tenant type: "
                                      << type;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete tenant type: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting tenant type "
                                       << type << ": " << e.what();
        }

        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_tenant_type_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_tenant_type_history_request from "
                               << remote_address;

    auto request_result = get_tenant_type_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_tenant_type_history_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication
    auto auth_result = get_authenticated_session(remote_address, "Get tenant type history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto ctx = make_request_context(*auth_result);
    service::tenant_type_service svc(ctx);

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for tenant type: "
                              << request.type;

    get_tenant_type_history_response response;
    try {
        auto history = svc.get_type_history(request.type);

        if (history.empty()) {
            response.success = false;
            response.message = "Tenant type not found: " + request.type;
            BOOST_LOG_SEV(lg(), warn) << "No history found for tenant type: "
                                      << request.type;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                  << response.versions.size()
                                  << " versions for tenant type: "
                                  << request.type;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for tenant type "
                                   << request.type << ": " << e.what();
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_tenant_statuses_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_tenant_statuses_request from "
                               << remote_address;

    auto request_result = get_tenant_statuses_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_tenant_statuses_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication
    auto auth_result = get_authenticated_session(remote_address, "Get tenant statuses");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto ctx = make_request_context(*auth_result);
    service::tenant_status_service svc(ctx);

    auto statuses = svc.list_statuses();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << statuses.size()
                              << " tenant statuses";

    get_tenant_statuses_response response{
        .statuses = std::move(statuses)
    };
    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_save_tenant_status_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing save_tenant_status_request from "
                               << remote_address;

    auto request_result = save_tenant_status_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize save_tenant_status_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication and system admin permission
    auto auth_result = get_authenticated_session(remote_address, "Save tenant status");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto ctx = make_request_context(*auth_result);
    service::tenant_status_service svc(ctx);

    auto request = std::move(*request_result);
    BOOST_LOG_SEV(lg(), info) << "Saving tenant status: "
                              << request.status.status;

    request.status.modified_by = auth_result->username;
    request.status.performed_by.clear();

    save_tenant_status_response response;
    try {
        svc.save_status(request.status);
        response.success = true;
        response.message = "Tenant status saved successfully";
        BOOST_LOG_SEV(lg(), info) << "Successfully saved tenant status: "
                                  << request.status.status << " by "
                                  << auth_result->username;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to save tenant status: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error saving tenant status "
                                   << request.status.status << ": " << e.what();
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_delete_tenant_status_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing delete_tenant_status_request from "
                               << remote_address;

    auto request_result = delete_tenant_status_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_tenant_status_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication and system admin permission
    auto auth_result = get_authenticated_session(remote_address, "Delete tenant status");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto ctx = make_request_context(*auth_result);
    service::tenant_status_service svc(ctx);

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Deleting " << request.statuses.size()
                              << " tenant status(es)";

    delete_tenant_status_response response;
    for (const auto& status : request.statuses) {
        delete_tenant_status_result result;
        result.status = status;

        try {
            svc.remove_status(status);
            result.success = true;
            result.message = "Tenant status deleted successfully";
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted tenant status: "
                                      << status;
        } catch (const std::exception& e) {
            result.success = false;
            result.message = std::string("Failed to delete tenant status: ") + e.what();
            BOOST_LOG_SEV(lg(), error) << "Error deleting tenant status "
                                       << status << ": " << e.what();
        }

        response.results.push_back(std::move(result));
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_get_tenant_status_history_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_tenant_status_history_request from "
                               << remote_address;

    auto request_result = get_tenant_status_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_tenant_status_history_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication
    auto auth_result = get_authenticated_session(remote_address, "Get tenant status history");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto ctx = make_request_context(*auth_result);
    service::tenant_status_service svc(ctx);

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Retrieving history for tenant status: "
                              << request.status;

    get_tenant_status_history_response response;
    try {
        auto history = svc.get_status_history(request.status);

        if (history.empty()) {
            response.success = false;
            response.message = "Tenant status not found: " + request.status;
            BOOST_LOG_SEV(lg(), warn) << "No history found for tenant status: "
                                      << request.status;
            co_return response.serialize();
        }

        response.success = true;
        response.message = "History retrieved successfully";
        response.versions = std::move(history);

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                  << response.versions.size()
                                  << " versions for tenant status: "
                                  << request.status;
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to retrieve history: ") + e.what();
        BOOST_LOG_SEV(lg(), error) << "Error retrieving history for tenant status "
                                   << request.status << ": " << e.what();
    }

    co_return response.serialize();
}

accounts_message_handler::handler_result accounts_message_handler::
handle_provision_tenant_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing provision_tenant_request from "
                               << remote_address;

    auto request_result = provision_tenant_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize provision_tenant_request";
        co_return std::unexpected(request_result.error());
    }

    // Requires authentication and iam::tenants:update permission
    auto auth_result = check_authorization(remote_address, domain::permissions::tenants_update, "Provision tenant");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    const auto& request = *request_result;

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*auth_result);

    // Escape single quotes by doubling them
    auto escape = [](const std::string& s) {
        std::string escaped;
        for (char c : s) {
            if (c == '\'') escaped += "''";
            else escaped += c;
        }
        return escaped;
    };

    const auto sql = std::format(
        "SELECT ores_iam_provision_tenant_fn('{}', '{}', '{}', '{}', '{}')",
        escape(request.type), escape(request.code), escape(request.name),
        escape(request.hostname), escape(request.description));

    try {
        auto rows = database::repository::execute_raw_multi_column_query(
            ctx, sql, lg(), "Provisioning tenant");

        if (rows.empty() || rows[0].empty() || !rows[0][0].has_value()) {
            BOOST_LOG_SEV(lg(), error) << "Provision tenant function returned no result";
            provision_tenant_response response{
                .success = false,
                .error_message = "Provision tenant function returned no result",
                .tenant_id = ""
            };
            co_return response.serialize();
        }

        const auto& tenant_id = *rows[0][0];
        BOOST_LOG_SEV(lg(), info) << "Provisioned tenant: " << request.name
                                  << " (code: " << request.code
                                  << ", id: " << tenant_id << ")";

        // If admin credentials are provided, create a TenantAdmin account
        if (!request.admin_username.empty()
            && !request.admin_password.empty()) {
            BOOST_LOG_SEV(lg(), info)
                << "Creating admin account '"
                << request.admin_username
                << "' in new tenant " << tenant_id;

            auto tenant_ctx =
                database::service::tenant_context::with_tenant(
                    ctx, tenant_id);
            service::account_service tenant_account_svc(tenant_ctx);
            service::account_setup_service tenant_setup_svc(
                tenant_account_svc, auth_service_);

            tenant_setup_svc.create_account_with_role(
                request.admin_username,
                request.admin_email,
                request.admin_password,
                auth_result->username,
                domain::roles::tenant_admin,
                "TenantAdmin account created during tenant onboarding");

            BOOST_LOG_SEV(lg(), info)
                << "Admin account created for tenant " << tenant_id;
        }

        provision_tenant_response response{
            .success = true,
            .error_message = "",
            .tenant_id = tenant_id
        };
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to provision tenant: " << e.what();
        provision_tenant_response response{
            .success = false,
            .error_message = std::string("Failed to provision tenant: ") + e.what(),
            .tenant_id = ""
        };
        co_return response.serialize();
    }
}

}

