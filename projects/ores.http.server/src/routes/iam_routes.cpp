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
#include "ores.http.server/routes/iam_routes.hpp"

#include <algorithm>
#include <sstream>
#include <rfl/json.hpp>
#include "ores.http/domain/jwt_claims.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/account_json.hpp"
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.iam/domain/role_json.hpp"
#include "ores.iam/domain/permission_json.hpp"
#include "ores.iam/domain/permission.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.iam/domain/session.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/messaging/account_history_protocol.hpp"
#include "ores.iam/domain/account_version.hpp"
#include "ores.iam/service/signup_service.hpp"
#include "ores.iam/service/account_setup_service.hpp"
#include "ores.database/service/tenant_context.hpp"

namespace ores::http_server::routes {

using namespace ores::logging;
using namespace ores::http::domain;
namespace asio = boost::asio;

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
 * If the principal contains `@`, the last `@` is used as the delimiter.
 *
 * @param principal The principal string to parse.
 * @return Parsed username and hostname. Hostname is empty if not specified.
 */
parsed_principal parse_principal(const std::string& principal) {
    const auto at_pos = principal.rfind('@');
    if (at_pos == std::string::npos) {
        return {.username = principal, .hostname = ""};
    }
    return {
        .username = principal.substr(0, at_pos),
        .hostname = principal.substr(at_pos + 1)
    };
}

}  // anonymous namespace
namespace reason = database::domain::change_reason_constants;

iam_routes::iam_routes(database::context ctx,
    std::shared_ptr<variability::service::system_flags_service> system_flags,
    std::shared_ptr<comms::service::auth_session_service> sessions,
    std::shared_ptr<iam::service::authorization_service> auth_service,
    std::shared_ptr<http::middleware::jwt_authenticator> authenticator,
    std::shared_ptr<geo::service::geolocation_service> geo_service)
    : ctx_(std::move(ctx))
    , account_service_(ctx_)
    , session_repo_(ctx_)
    , system_flags_(std::move(system_flags))
    , sessions_(std::move(sessions))
    , auth_service_(std::move(auth_service))
    , authenticator_(std::move(authenticator))
    , geo_service_(std::move(geo_service)) {
    BOOST_LOG_SEV(lg(), debug) << "IAM routes initialized";
}

void iam_routes::register_routes(std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> registry) {

    BOOST_LOG_SEV(lg(), info) << "Registering IAM routes";

    // Authentication routes (no auth required for login/signup/bootstrap)
    auto login = router->post("/api/v1/auth/login")
        .summary("User login")
        .description("Authenticate user with username and password")
        .tags({"auth"})
        .body<iam::messaging::login_request>()
        .response<iam::messaging::login_response>()
        .handler([this](const http_request& req) { return handle_login(req); });
    router->add_route(login.build());
    registry->register_route(login.build());

    auto logout = router->post("/api/v1/auth/logout")
        .summary("User logout")
        .description("Logout current session")
        .tags({"auth"})
        .auth_required()
        .response<iam::messaging::logout_response>()
        .handler([this](const http_request& req) { return handle_logout(req); });
    router->add_route(logout.build());
    registry->register_route(logout.build());

    auto signup = router->post("/api/v1/auth/signup")
        .summary("User signup")
        .description("Create a new account (when self-registration is enabled)")
        .tags({"auth"})
        .body<iam::messaging::signup_request>()
        .response<iam::messaging::signup_response>()
        .handler([this](const http_request& req) { return handle_signup(req); });
    router->add_route(signup.build());
    registry->register_route(signup.build());

    auto bootstrap_status = router->get("/api/v1/auth/bootstrap-status")
        .summary("Get bootstrap status")
        .description("Check if system is in bootstrap mode awaiting initial admin")
        .tags({"auth"})
        .response<iam::messaging::bootstrap_status_response>()
        .handler([this](const http_request& req) { return handle_bootstrap_status(req); });
    router->add_route(bootstrap_status.build());
    registry->register_route(bootstrap_status.build());

    auto bootstrap = router->post("/api/v1/auth/bootstrap")
        .summary("Create initial admin")
        .description("Create initial admin account (bootstrap mode only, localhost only)")
        .tags({"auth"})
        .body<iam::messaging::create_initial_admin_request>()
        .response<iam::messaging::create_initial_admin_response>()
        .handler([this](const http_request& req) { return handle_create_initial_admin(req); });
    router->add_route(bootstrap.build());
    registry->register_route(bootstrap.build());

    // Account management routes
    auto list_accounts = router->get("/api/v1/accounts")
        .summary("List accounts")
        .description("Retrieve accounts with pagination")
        .tags({"accounts"})
        .auth_required()
        .query_param("offset", "integer", "", false, "Pagination offset", "0")
        .query_param("limit", "integer", "", false, "Maximum number of results", "100")
        .response<iam::messaging::get_accounts_response>()
        .handler([this](const http_request& req) { return handle_list_accounts(req); });
    router->add_route(list_accounts.build());
    registry->register_route(list_accounts.build());

    auto create_account = router->post("/api/v1/accounts")
        .summary("Create account")
        .description("Create a new user account")
        .tags({"accounts"})
        .auth_required()
        .roles({"Admin"})
        .body<iam::messaging::save_account_request>()
        .response<iam::messaging::save_account_response>()
        .handler([this](const http_request& req) { return handle_create_account(req); });
    router->add_route(create_account.build());
    registry->register_route(create_account.build());

    auto delete_account = router->delete_("/api/v1/accounts/{id}")
        .summary("Delete account")
        .description("Delete an account by ID")
        .tags({"accounts"})
        .auth_required()
        .roles({"Admin"})
        .response<iam::messaging::delete_account_response>()
        .handler([this](const http_request& req) { return handle_delete_account(req); });
    router->add_route(delete_account.build());
    registry->register_route(delete_account.build());

    auto update_account = router->put("/api/v1/accounts/{id}")
        .summary("Update account")
        .description("Update account details")
        .tags({"accounts"})
        .auth_required()
        .roles({"Admin"})
        .body<iam::messaging::save_account_request>()
        .response<iam::messaging::save_account_response>()
        .handler([this](const http_request& req) { return handle_update_account(req); });
    router->add_route(update_account.build());
    registry->register_route(update_account.build());

    auto account_history = router->get("/api/v1/accounts/{username}/history")
        .summary("Get account history")
        .description("Retrieve version history for an account")
        .tags({"accounts"})
        .auth_required()
        .response<iam::messaging::get_account_history_response>()
        .handler([this](const http_request& req) { return handle_get_account_history(req); });
    router->add_route(account_history.build());
    registry->register_route(account_history.build());

    auto lock_accounts = router->post("/api/v1/accounts/lock")
        .summary("Lock accounts")
        .description("Lock one or more accounts")
        .tags({"accounts"})
        .auth_required()
        .roles({"Admin"})
        .body<iam::messaging::lock_account_request>()
        .response<iam::messaging::lock_account_response>()
        .handler([this](const http_request& req) { return handle_lock_accounts(req); });
    router->add_route(lock_accounts.build());
    registry->register_route(lock_accounts.build());

    auto unlock_accounts = router->post("/api/v1/accounts/unlock")
        .summary("Unlock accounts")
        .description("Unlock one or more accounts")
        .tags({"accounts"})
        .auth_required()
        .roles({"Admin"})
        .body<iam::messaging::unlock_account_request>()
        .response<iam::messaging::unlock_account_response>()
        .handler([this](const http_request& req) { return handle_unlock_accounts(req); });
    router->add_route(unlock_accounts.build());
    registry->register_route(unlock_accounts.build());

    auto login_info = router->get("/api/v1/accounts/login-info")
        .summary("List login info")
        .description("Retrieve login info for all accounts")
        .tags({"accounts"})
        .auth_required()
        .response<iam::messaging::list_login_info_response>()
        .handler([this](const http_request& req) { return handle_list_login_info(req); });
    router->add_route(login_info.build());
    registry->register_route(login_info.build());

    auto reset_password = router->post("/api/v1/accounts/reset-password")
        .summary("Reset password")
        .description("Admin-initiated password reset for accounts")
        .tags({"accounts"})
        .auth_required()
        .roles({"Admin"})
        .body<iam::messaging::reset_password_request>()
        .response<iam::messaging::reset_password_response>()
        .handler([this](const http_request& req) { return handle_reset_password(req); });
    router->add_route(reset_password.build());
    registry->register_route(reset_password.build());

    // Current user routes
    auto change_password = router->post("/api/v1/me/change-password")
        .summary("Change password")
        .description("Change own password")
        .tags({"me"})
        .auth_required()
        .body<iam::messaging::change_password_request>()
        .response<iam::messaging::change_password_response>()
        .handler([this](const http_request& req) { return handle_change_password(req); });
    router->add_route(change_password.build());
    registry->register_route(change_password.build());

    auto update_email = router->put("/api/v1/me/email")
        .summary("Update email")
        .description("Update own email address")
        .tags({"me"})
        .auth_required()
        .body<iam::messaging::update_my_email_request>()
        .response<iam::messaging::update_my_email_response>()
        .handler([this](const http_request& req) { return handle_update_my_email(req); });
    router->add_route(update_email.build());
    registry->register_route(update_email.build());

    // RBAC routes
    auto list_roles = router->get("/api/v1/roles")
        .summary("List roles")
        .description("List all roles in the system")
        .tags({"rbac"})
        .auth_required()
        .response<iam::messaging::list_roles_response>()
        .handler([this](const http_request& req) { return handle_list_roles(req); });
    router->add_route(list_roles.build());
    registry->register_route(list_roles.build());

    auto get_role = router->get("/api/v1/roles/{id}")
        .summary("Get role")
        .description("Get a specific role by ID")
        .tags({"rbac"})
        .auth_required()
        .response<iam::messaging::get_role_response>()
        .handler([this](const http_request& req) { return handle_get_role(req); });
    router->add_route(get_role.build());
    registry->register_route(get_role.build());

    auto list_permissions = router->get("/api/v1/permissions")
        .summary("List permissions")
        .description("List all permissions in the system")
        .tags({"rbac"})
        .auth_required()
        .response<iam::messaging::list_permissions_response>()
        .handler([this](const http_request& req) { return handle_list_permissions(req); });
    router->add_route(list_permissions.build());
    registry->register_route(list_permissions.build());

    auto assign_role = router->post("/api/v1/accounts/{id}/roles")
        .summary("Assign role")
        .description("Assign a role to an account")
        .tags({"rbac"})
        .auth_required()
        .roles({"Admin"})
        .body<iam::messaging::assign_role_request>()
        .response<iam::messaging::assign_role_response>()
        .handler([this](const http_request& req) { return handle_assign_role(req); });
    router->add_route(assign_role.build());
    registry->register_route(assign_role.build());

    auto revoke_role = router->delete_("/api/v1/accounts/{accountId}/roles/{roleId}")
        .summary("Revoke role")
        .description("Revoke a role from an account")
        .tags({"rbac"})
        .auth_required()
        .roles({"Admin"})
        .response<iam::messaging::revoke_role_response>()
        .handler([this](const http_request& req) { return handle_revoke_role(req); });
    router->add_route(revoke_role.build());
    registry->register_route(revoke_role.build());

    auto account_roles = router->get("/api/v1/accounts/{id}/roles")
        .summary("Get account roles")
        .description("Get all roles assigned to an account")
        .tags({"rbac"})
        .auth_required()
        .response<iam::messaging::get_account_roles_response>()
        .handler([this](const http_request& req) { return handle_get_account_roles(req); });
    router->add_route(account_roles.build());
    registry->register_route(account_roles.build());

    auto account_perms = router->get("/api/v1/accounts/{id}/permissions")
        .summary("Get account permissions")
        .description("Get effective permissions for an account")
        .tags({"rbac"})
        .auth_required()
        .response<iam::messaging::get_account_permissions_response>()
        .handler([this](const http_request& req) { return handle_get_account_permissions(req); });
    router->add_route(account_perms.build());
    registry->register_route(account_perms.build());

    // Session routes
    auto list_sessions = router->get("/api/v1/sessions")
        .summary("List sessions")
        .description("List session history")
        .tags({"sessions"})
        .auth_required()
        .query_param("account_id", "string", "uuid", false, "Filter by account UUID")
        .query_param("offset", "integer", "", false, "Pagination offset", "0")
        .query_param("limit", "integer", "", false, "Maximum number of results", "100")
        .response<iam::messaging::list_sessions_response>()
        .handler([this](const http_request& req) { return handle_list_sessions(req); });
    router->add_route(list_sessions.build());
    registry->register_route(list_sessions.build());

    auto session_stats = router->get("/api/v1/sessions/statistics")
        .summary("Get session statistics")
        .description("Get aggregated session statistics")
        .tags({"sessions"})
        .auth_required()
        .response<iam::messaging::get_session_statistics_response>()
        .handler([this](const http_request& req) { return handle_get_session_statistics(req); });
    router->add_route(session_stats.build());
    registry->register_route(session_stats.build());

    auto active_sessions = router->get("/api/v1/sessions/active")
        .summary("Get active sessions")
        .description("Get currently active sessions")
        .tags({"sessions"})
        .auth_required()
        .response<iam::messaging::get_active_sessions_response>()
        .handler([this](const http_request& req) { return handle_get_active_sessions(req); });
    router->add_route(active_sessions.build());
    registry->register_route(active_sessions.build());

    BOOST_LOG_SEV(lg(), info) << "IAM routes registered: 26 endpoints";
}

// Authorization helper

std::expected<auth_result, http_response> iam_routes::check_auth(
    const http_request& req,
    std::string_view required_permission,
    std::string_view operation_name) {

    // Step 1: Check authentication
    if (!req.authenticated_user) {
        BOOST_LOG_SEV(lg(), warn) << operation_name << " denied: not authenticated";
        return std::unexpected(http_response::unauthorized("Not authenticated"));
    }

    // Step 2: Extract account ID from JWT
    boost::uuids::uuid account_id;
    try {
        account_id = boost::uuids::string_generator()(req.authenticated_user->subject);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << operation_name
                                   << " failed: invalid account ID in JWT: " << e.what();
        return std::unexpected(http_response::unauthorized("Invalid token"));
    }

    // Step 3: Check if user has admin permissions (for convenience)
    bool is_admin = auth_service_->has_permission(account_id,
        iam::domain::permissions::accounts_read);

    // Step 4: Check required permission if specified
    if (!required_permission.empty()) {
        if (!auth_service_->has_permission(account_id, std::string(required_permission))) {
            BOOST_LOG_SEV(lg(), warn) << operation_name << " denied: account "
                                      << boost::uuids::to_string(account_id)
                                      << " lacks " << required_permission << " permission";
            return std::unexpected(http_response::forbidden("Insufficient permissions"));
        }
    }

    return auth_result{account_id, is_admin};
}

// Handler implementations

asio::awaitable<http_response> iam_routes::handle_login(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling login request";

    auto login_req = parse_body<iam::messaging::login_request>(req, "login");
    if (!login_req) {
        co_return login_req.error();
    }

    try {
        // Parse principal into username and hostname
        const auto [username, hostname] = parse_principal(login_req->principal);
        BOOST_LOG_SEV(lg(), debug) << "Parsed principal - username: " << username
                                   << ", hostname: " << (hostname.empty() ? "(system)" : hostname);

        // Set tenant context based on hostname and track tenant info
        std::string tenant_id_str;
        std::string tenant_name;
        if (hostname.empty()) {
            database::service::tenant_context::set_system_tenant(ctx_);
            tenant_id_str = database::service::tenant_context::system_tenant_id;
            tenant_name = "System";
        } else {
            tenant_id_str =
                database::service::tenant_context::lookup_by_hostname(ctx_, hostname);
            database::service::tenant_context::set(ctx_, tenant_id_str);
            tenant_name =
                database::service::tenant_context::lookup_name(ctx_, tenant_id_str);
        }

        auto ip_address = boost::asio::ip::make_address(req.remote_address);
        auto account = account_service_.login(
            username, login_req->password, ip_address);

        auto login_info = account_service_.get_login_info(account.id);

        // Create session record for tracking
        iam::domain::session sess;
        sess.id = boost::uuids::random_generator()();
        sess.account_id = account.id;
        sess.start_time = std::chrono::system_clock::now();
        sess.client_ip = ip_address;
        sess.protocol = iam::domain::session_protocol::http;

        // Extract User-Agent and HTTP version from request
        auto user_agent = req.get_header("User-Agent");
        sess.client_identifier = user_agent.empty() ? "HTTP Client" : user_agent;
        sess.client_version_major = req.http_version_major;
        sess.client_version_minor = req.http_version_minor;

        // Track request body size as bytes_received
        sess.bytes_received = req.body.size();

        // Perform geolocation lookup
        auto geo_result = geo_service_->lookup(ip_address);
        if (geo_result) {
            sess.country_code = geo_result->country_code;
            BOOST_LOG_SEV(lg(), debug) << "Geolocation for " << ip_address
                                       << ": " << sess.country_code;
        } else {
            BOOST_LOG_SEV(lg(), debug) << "Geolocation lookup failed for "
                                       << ip_address;
        }

        // Persist session to database
        try {
            session_repo_.create(sess);
            BOOST_LOG_SEV(lg(), debug) << "HTTP session persisted: "
                                       << boost::uuids::to_string(sess.id);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to persist HTTP session: "
                                      << e.what();
            // Continue anyway - session tracking is not critical
        }

        // Generate JWT token if authenticator is configured
        std::string token;
        if (authenticator_ && authenticator_->is_configured()) {
            // Get user's roles for the token
            auto roles = auth_service_->get_account_roles(account.id);
            std::vector<std::string> role_names;
            for (const auto& role : roles) {
                role_names.push_back(role.name);
            }

            // Create JWT claims with session ID and start time
            http::domain::jwt_claims claims;
            claims.subject = boost::uuids::to_string(account.id);
            claims.username = account.username;
            claims.email = account.email;
            claims.roles = role_names;
            claims.session_id = boost::uuids::to_string(sess.id);
            claims.session_start_time = sess.start_time;
            claims.issued_at = std::chrono::system_clock::now();
            claims.expires_at = claims.issued_at + std::chrono::hours(24);

            auto token_opt = authenticator_->create_token(claims);
            if (token_opt) {
                token = *token_opt;
                BOOST_LOG_SEV(lg(), debug) << "Generated JWT token for user: "
                    << account.username;
            } else {
                BOOST_LOG_SEV(lg(), warn) << "Failed to generate JWT token for user: "
                    << account.username;
            }
        }

        // Build response with token
        std::ostringstream oss;
        oss << R"({"success":true,"account_id":")"
            << boost::uuids::to_string(account.id) << R"(","tenant_id":")"
            << tenant_id_str << R"(","tenant_name":")"
            << tenant_name << R"(","username":")"
            << account.username << R"(","email":")"
            << account.email << R"(","password_reset_required":)"
            << (login_info.password_reset_required ? "true" : "false");

        if (!token.empty()) {
            oss << R"(,"token":")" << token << R"(")";
        }
        oss << "}";

        // Note: bytes_sent for the login response will be tracked by the
        // centralized session_bytes_callback for subsequent authenticated requests.
        // For login, bytes_received was already set when creating the session.
        co_return http_response::json(oss.str());
    } catch (const std::runtime_error&) {
        BOOST_LOG_SEV(lg(), warn) << "Login failed for principal: " << login_req->principal;
        co_return http_response::unauthorized("Invalid credentials");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Login error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_logout(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling logout request";

    auto auth = check_auth(req, "", "logout");
    if (!auth) {
        co_return auth.error();
    }

    try {
        // Record the logout event (updates login_info last_logout_time)
        account_service_.logout(auth->account_id);

        // End the session record if session_id is in the JWT
        if (req.authenticated_user && req.authenticated_user->session_id) {
            try {
                auto session_id = boost::uuids::string_generator()(
                    *req.authenticated_user->session_id);
                auto now = std::chrono::system_clock::now();

                // We need the start_time to update the session. Read it first.
                auto session = session_repo_.read(session_id);
                if (session) {
                    session_repo_.end_session(session_id, session->start_time,
                        now, 0, 0);  // No byte tracking for HTTP
                    BOOST_LOG_SEV(lg(), debug) << "HTTP session ended: "
                                               << *req.authenticated_user->session_id;
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "Failed to end HTTP session: "
                                          << e.what();
                // Continue anyway - session tracking is not critical
            }
        }

        BOOST_LOG_SEV(lg(), info) << "Successfully logged out account: "
                                  << boost::uuids::to_string(auth->account_id);

        iam::messaging::logout_response resp;
        resp.success = true;
        resp.message = "Logged out successfully";

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Logout error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_signup(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling signup request";

    // Check if signups are enabled
    if (!system_flags_->is_user_signups_enabled()) {
        co_return http_response::forbidden("User signups are disabled");
    }

    auto signup_req = parse_body<iam::messaging::signup_request>(req, "signup");
    if (!signup_req) {
        co_return signup_req.error();
    }

    try {
        iam::service::signup_service signup_svc(ctx_, system_flags_, auth_service_);
        auto result = signup_svc.register_user(
            signup_req->username, signup_req->email, signup_req->password);

        if (!result.success) {
            co_return http_response::bad_request(result.error_message);
        }

        iam::messaging::signup_response resp;
        resp.success = true;
        resp.account_id = result.account_id;
        resp.username = result.username;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Signup error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_bootstrap_status(const http_request&) {
    BOOST_LOG_SEV(lg(), debug) << "Handling bootstrap status request";

    try {
        bool in_bootstrap = system_flags_->is_bootstrap_mode_enabled();

        iam::messaging::bootstrap_status_response resp;
        resp.is_in_bootstrap_mode = in_bootstrap;
        resp.message = in_bootstrap ?
            "System in BOOTSTRAP MODE - awaiting initial admin account creation" :
            "System in SECURE MODE - admin account exists";

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Bootstrap status error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_create_initial_admin(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling create initial admin request";

    // Check if request is from localhost
    bool is_localhost = req.remote_address.starts_with("127.0.0.1") ||
                       req.remote_address.starts_with("::1");
    if (!is_localhost) {
        BOOST_LOG_SEV(lg(), warn)
            << "Rejected create_initial_admin from non-localhost: "
            << req.remote_address;
        co_return http_response::forbidden("Bootstrap endpoint only accessible from localhost");
    }

    if (!system_flags_->is_bootstrap_mode_enabled()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Rejected create_initial_admin: system not in bootstrap mode";
        co_return http_response::forbidden("System not in bootstrap mode - admin account already exists");
    }

    auto admin_req = parse_body<iam::messaging::create_initial_admin_request>(
        req, "create_initial_admin");
    if (!admin_req) {
        co_return admin_req.error();
    }

    try {
        // Parse principal into username and hostname
        const auto [username, hostname] = parse_principal(admin_req->principal);
        BOOST_LOG_SEV(lg(), debug) << "Parsed principal - username: " << username
                                   << ", hostname: " << (hostname.empty() ? "(system)" : hostname);

        // Set tenant context based on hostname
        if (hostname.empty()) {
            database::service::tenant_context::set_system_tenant(ctx_);
        } else {
            const auto tenant_id =
                database::service::tenant_context::lookup_by_hostname(ctx_, hostname);
            database::service::tenant_context::set(ctx_, tenant_id);
        }

        // Create the initial admin account with Admin role
        // This checks role exists BEFORE creating account to avoid orphaned accounts
        iam::service::account_setup_service setup_service(account_service_, auth_service_);
        auto account = setup_service.create_account_with_role(
            username, admin_req->email, admin_req->password,
            "bootstrap", iam::domain::roles::admin,
            "Initial admin account created during system bootstrap");

        // Exit bootstrap mode - updates database and shared cache
        system_flags_->set_bootstrap_mode(false, "system",
            std::string{reason::codes::new_record}, "Bootstrap mode disabled after initial admin account created");

        BOOST_LOG_SEV(lg(), info)
            << "Created initial admin account with ID: " << account.id
            << " for username: " << account.username;
        BOOST_LOG_SEV(lg(), info) << "System transitioned to SECURE MODE";

        iam::messaging::create_initial_admin_response resp;
        resp.success = true;
        resp.account_id = account.id;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Create initial admin error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_list_accounts(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling list accounts request";

    auto auth = check_auth(req, iam::domain::permissions::accounts_read, "list_accounts");
    if (!auth) {
        co_return auth.error();
    }

    try {
        std::uint32_t offset = 0;
        std::uint32_t limit = 100;

        auto offset_str = req.get_query_param("offset");
        auto limit_str = req.get_query_param("limit");

        if (!offset_str.empty()) offset = std::stoul(offset_str);
        if (!limit_str.empty()) limit = std::stoul(limit_str);

        auto accounts = account_service_.list_accounts(offset, limit);

        iam::messaging::get_accounts_response resp;
        resp.accounts = accounts;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List accounts error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_create_account(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling create account request";

    auto auth = check_auth(req, iam::domain::permissions::accounts_create, "create_account");
    if (!auth) {
        co_return auth.error();
    }

    auto create_req = parse_body<iam::messaging::save_account_request>(req, "create_account");
    if (!create_req) {
        co_return create_req.error();
    }

    try {
        // Use setup_service to create account with default Viewer role
        iam::service::account_setup_service setup_service(account_service_, auth_service_);
        auto account = setup_service.create_account(
            create_req->principal, create_req->email,
            create_req->password, req.authenticated_user->username.value_or("system"));

        iam::messaging::save_account_response resp;
        resp.account_id = account.id;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Create account error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_delete_account(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling delete account request";

    auto auth = check_auth(req, iam::domain::permissions::accounts_delete, "delete_account");
    if (!auth) {
        co_return auth.error();
    }

    try {
        auto account_id = req.get_path_param("id");
        if (account_id.empty()) {
            co_return http_response::bad_request("Account ID required");
        }

        auto uuid = boost::uuids::string_generator()(account_id);
        account_service_.delete_account(uuid);

        iam::messaging::delete_account_response resp;
        resp.success = true;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Delete account error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_update_account(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling update account request";

    auto auth = check_auth(req, iam::domain::permissions::accounts_update, "update_account");
    if (!auth) {
        co_return auth.error();
    }

    auto account_id = req.get_path_param("id");
    if (account_id.empty()) {
        co_return http_response::bad_request("Account ID required");
    }

    auto update_req = parse_body<iam::messaging::save_account_request>(req, "update_account");
    if (!update_req) {
        co_return update_req.error();
    }

    try {
        auto uuid = boost::uuids::string_generator()(account_id);
        bool success = account_service_.update_account(uuid, update_req->email,
            req.authenticated_user->username.value_or("system"),
            update_req->change_reason_code,
            update_req->change_commentary);

        iam::messaging::save_account_response resp;
        resp.success = success;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Update account error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_get_account_history(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get account history request";

    try {
        auto username = req.get_path_param("username");
        if (username.empty()) {
            co_return http_response::bad_request("Username required");
        }

        auto accounts = account_service_.get_account_history(username);

        iam::messaging::get_account_history_response resp;

        if (accounts.empty()) {
            resp.success = false;
            resp.message = "Account not found: " + username;
            co_return http_response::json(rfl::json::write(resp));
        }

        // Sort by version descending (newest first) - use database version field
        std::sort(accounts.begin(), accounts.end(),
            [](const auto& a, const auto& b) {
                return a.version > b.version;
            });

        resp.success = true;
        resp.message = "History retrieved successfully";
        resp.history.username = username;

        for (const auto& account : accounts) {
            iam::domain::account_version ver;
            ver.data = account;
            ver.version_number = account.version;  // Use database version field
            ver.recorded_by = account.recorded_by;
            ver.recorded_at = account.recorded_at;
            ver.change_summary = "Version " + std::to_string(ver.version_number);
            resp.history.versions.push_back(std::move(ver));
        }

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << resp.history.versions.size()
                                  << " versions for account: " << username;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get account history error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_lock_accounts(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling lock accounts request";

    auto auth = check_auth(req, iam::domain::permissions::accounts_lock, "lock_accounts");
    if (!auth) {
        co_return auth.error();
    }

    auto lock_req = parse_body<iam::messaging::lock_account_request>(req, "lock_accounts");
    if (!lock_req) {
        co_return lock_req.error();
    }

    try {
        std::vector<iam::messaging::lock_account_result> results;
        for (const auto& id : lock_req->account_ids) {
            bool success = account_service_.lock_account(id);
            iam::messaging::lock_account_result result;
            result.account_id = id;
            result.success = success;
            result.message = success ? "" : "Failed to lock";
            results.push_back(result);
        }

        iam::messaging::lock_account_response resp;
        resp.results = results;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Lock accounts error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_unlock_accounts(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling unlock accounts request";

    auto auth = check_auth(req, iam::domain::permissions::accounts_unlock, "unlock_accounts");
    if (!auth) {
        co_return auth.error();
    }

    auto unlock_req = parse_body<iam::messaging::unlock_account_request>(req, "unlock_accounts");
    if (!unlock_req) {
        co_return unlock_req.error();
    }

    try {
        std::vector<iam::messaging::unlock_account_result> results;
        for (const auto& id : unlock_req->account_ids) {
            bool success = account_service_.unlock_account(id);
            iam::messaging::unlock_account_result result;
            result.account_id = id;
            result.success = success;
            result.message = success ? "" : "Failed to unlock";
            results.push_back(result);
        }

        iam::messaging::unlock_account_response resp;
        resp.results = results;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Unlock accounts error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_list_login_info(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling list login info request";

    auto auth = check_auth(req, iam::domain::permissions::login_info_read, "list_login_info");
    if (!auth) {
        co_return auth.error();
    }

    try {
        auto login_infos = account_service_.list_login_info();

        iam::messaging::list_login_info_response resp;
        resp.login_infos = login_infos;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List login info error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_reset_password(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling reset password request";

    auto auth = check_auth(req, iam::domain::permissions::accounts_reset_password,
        "reset_password");
    if (!auth) {
        co_return auth.error();
    }

    auto reset_req = parse_body<iam::messaging::reset_password_request>(req, "reset_password");
    if (!reset_req) {
        co_return reset_req.error();
    }

    try {
        std::vector<iam::messaging::reset_password_result> results;
        for (const auto& id : reset_req->account_ids) {
            bool success = account_service_.set_password_reset_required(id);
            iam::messaging::reset_password_result result;
            result.account_id = id;
            result.success = success;
            result.message = success ? "" : "Failed to reset password";
            results.push_back(result);
        }

        iam::messaging::reset_password_response resp;
        resp.results = results;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Reset password error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_change_password(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling change password request";

    auto auth = check_auth(req, "", "change_password");
    if (!auth) {
        co_return auth.error();
    }

    auto change_req = parse_body<iam::messaging::change_password_request>(req, "change_password");
    if (!change_req) {
        co_return change_req.error();
    }

    try {
        auto error = account_service_.change_password(auth->account_id, change_req->new_password);

        iam::messaging::change_password_response resp;
        resp.success = error.empty();
        resp.message = error.empty() ? "Password changed" : error;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Change password error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_update_my_email(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling update my email request";

    auto auth = check_auth(req, "", "update_my_email");
    if (!auth) {
        co_return auth.error();
    }

    auto update_req = parse_body<iam::messaging::update_my_email_request>(req, "update_my_email");
    if (!update_req) {
        co_return update_req.error();
    }

    try {
        auto error = account_service_.update_my_email(auth->account_id, update_req->new_email);

        iam::messaging::update_my_email_response resp;
        resp.success = error.empty();
        resp.message = error.empty() ? "Email updated" : error;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Update my email error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_list_roles(const http_request&) {
    BOOST_LOG_SEV(lg(), debug) << "Handling list roles request";

    try {
        auto roles = auth_service_->list_roles();

        iam::messaging::list_roles_response resp;
        resp.roles = roles;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List roles error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_get_role(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get role request";

    try {
        auto role_id = req.get_path_param("id");
        if (role_id.empty()) {
            co_return http_response::bad_request("Role ID required");
        }

        auto role_uuid = boost::uuids::string_generator()(role_id);
        auto role = auth_service_->find_role(role_uuid);
        if (!role) {
            co_return http_response::not_found("Role not found");
        }

        iam::messaging::get_role_response resp;
        resp.role = *role;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get role error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_list_permissions(const http_request&) {
    BOOST_LOG_SEV(lg(), debug) << "Handling list permissions request";

    try {
        auto permissions = auth_service_->list_permissions();

        iam::messaging::list_permissions_response resp;
        resp.permissions = permissions;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List permissions error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_assign_role(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling assign role request";

    auto auth = check_auth(req, iam::domain::permissions::roles_assign, "assign_role");
    if (!auth) {
        co_return auth.error();
    }

    auto account_id = req.get_path_param("id");
    if (account_id.empty()) {
        co_return http_response::bad_request("Account ID required");
    }

    auto assign_req = parse_body<iam::messaging::assign_role_request>(req, "assign_role");
    if (!assign_req) {
        co_return assign_req.error();
    }

    try {
        auto uuid = boost::uuids::string_generator()(account_id);
        auth_service_->assign_role(uuid, assign_req->role_id,
            req.authenticated_user->username.value_or("system"));

        iam::messaging::assign_role_response resp;
        resp.success = true;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Assign role error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_revoke_role(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling revoke role request";

    auto auth = check_auth(req, iam::domain::permissions::roles_revoke, "revoke_role");
    if (!auth) {
        co_return auth.error();
    }

    try {
        auto account_id = req.get_path_param("accountId");
        auto role_id = req.get_path_param("roleId");

        if (account_id.empty() || role_id.empty()) {
            co_return http_response::bad_request("Account ID and Role ID required");
        }

        auto uuid = boost::uuids::string_generator()(account_id);
        auto role_uuid = boost::uuids::string_generator()(role_id);
        auth_service_->revoke_role(uuid, role_uuid);

        iam::messaging::revoke_role_response resp;
        resp.success = true;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Revoke role error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_get_account_roles(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get account roles request";

    try {
        auto account_id = req.get_path_param("id");
        if (account_id.empty()) {
            co_return http_response::bad_request("Account ID required");
        }

        auto uuid = boost::uuids::string_generator()(account_id);
        auto roles = auth_service_->get_account_roles(uuid);

        iam::messaging::get_account_roles_response resp;
        resp.roles = roles;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get account roles error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_get_account_permissions(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get account permissions request";

    try {
        auto account_id = req.get_path_param("id");
        if (account_id.empty()) {
            co_return http_response::bad_request("Account ID required");
        }

        auto uuid = boost::uuids::string_generator()(account_id);
        auto permission_codes = auth_service_->get_effective_permissions(uuid);

        iam::messaging::get_account_permissions_response resp;
        resp.permission_codes = permission_codes;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get account permissions error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_list_sessions(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling list sessions request";

    auto auth = check_auth(req, "", "list_sessions");
    if (!auth) {
        co_return auth.error();
    }

    try {
        auto account_id_str = req.get_query_param("account_id");
        std::uint32_t offset = 0;
        std::uint32_t limit = 100;

        auto offset_str = req.get_query_param("offset");
        auto limit_str = req.get_query_param("limit");

        if (!offset_str.empty()) offset = std::stoul(offset_str);
        if (!limit_str.empty()) limit = std::stoul(limit_str);

        // Determine target account
        boost::uuids::uuid target_account_id = auth->account_id;

        if (!account_id_str.empty()) {
            auto requested_id = boost::uuids::string_generator()(account_id_str);
            if (!auth->is_admin && requested_id != auth->account_id) {
                // Non-admin trying to view someone else's sessions
                co_return http_response::forbidden("Cannot view other users' sessions");
            }
            target_account_id = requested_id;
        }

        // Query sessions from database
        auto sessions_list = session_repo_.read_by_account(target_account_id, limit, offset);
        auto total_count = session_repo_.count_by_account(target_account_id);

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << sessions_list.size()
                                  << " sessions for account "
                                  << boost::uuids::to_string(target_account_id);

        iam::messaging::list_sessions_response resp;
        resp.sessions = std::move(sessions_list);
        resp.total_count = total_count;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List sessions error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_get_session_statistics(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get session statistics request";

    auto auth = check_auth(req, "", "get_session_statistics");
    if (!auth) {
        co_return auth.error();
    }

    try {
        auto account_id_str = req.get_query_param("account_id");
        auto start_str = req.get_query_param("start");
        auto end_str = req.get_query_param("end");

        boost::uuids::uuid target_account_id = auth->account_id;
        bool aggregate_mode = false;

        if (account_id_str.empty()) {
            // No account specified - aggregate mode for admin, own account for others
            if (auth->is_admin) {
                aggregate_mode = true;
            }
        } else {
            auto requested_id = boost::uuids::string_generator()(account_id_str);
            if (!auth->is_admin && requested_id != auth->account_id) {
                co_return http_response::forbidden("Cannot view other users' statistics");
            }
            target_account_id = requested_id;
        }

        // Parse time range (default to last 30 days)
        auto now = std::chrono::system_clock::now();
        auto start_time = now - std::chrono::hours(24 * 30);
        auto end_time = now;

        // TODO: Parse start/end from query params if provided
        (void)start_str;
        (void)end_str;

        // Query statistics from database
        std::vector<iam::domain::session_statistics> stats;
        if (aggregate_mode) {
            stats = session_repo_.read_aggregate_daily_statistics(start_time, end_time);
            BOOST_LOG_SEV(lg(), info) << "Retrieved " << stats.size()
                                      << " aggregate statistics entries";
        } else {
            stats = session_repo_.read_daily_statistics(target_account_id, start_time, end_time);
            BOOST_LOG_SEV(lg(), info) << "Retrieved " << stats.size()
                                      << " statistics entries for account "
                                      << boost::uuids::to_string(target_account_id);
        }

        iam::messaging::get_session_statistics_response resp;
        resp.statistics = std::move(stats);

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get session statistics error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_get_active_sessions(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get active sessions request";

    auto auth = check_auth(req, "", "get_active_sessions");
    if (!auth) {
        co_return auth.error();
    }

    try {
        std::vector<iam::domain::session> active_sessions;

        if (auth->is_admin) {
            // Admin gets all active sessions
            active_sessions = session_repo_.read_all_active();
            BOOST_LOG_SEV(lg(), info) << "Retrieved " << active_sessions.size()
                                      << " active sessions (admin view)";
        } else {
            // Non-admin gets only their own active sessions
            active_sessions = session_repo_.read_active_by_account(auth->account_id);
            BOOST_LOG_SEV(lg(), info) << "Retrieved " << active_sessions.size()
                                      << " active sessions for account "
                                      << boost::uuids::to_string(auth->account_id);
        }

        iam::messaging::get_active_sessions_response resp;
        resp.sessions = std::move(active_sessions);

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get active sessions error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

}
