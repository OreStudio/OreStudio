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

#include <rfl/json.hpp>
#include "ores.iam/domain/account_json.hpp"
#include "ores.iam/domain/role_json.hpp"
#include "ores.iam/domain/permission_json.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/service/signup_service.hpp"
#include "ores.iam/service/account_setup_service.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"

namespace ores::http_server::routes {

using namespace ores::telemetry::log;
using namespace ores::http::domain;
namespace asio = boost::asio;

iam_routes::iam_routes(database::context ctx,
    std::shared_ptr<variability::service::system_flags_service> system_flags,
    std::shared_ptr<comms::service::auth_session_service> sessions,
    std::shared_ptr<iam::service::authorization_service> auth_service)
    : ctx_(std::move(ctx))
    , account_service_(ctx_)
    , session_repo_(ctx_)
    , system_flags_(std::move(system_flags))
    , sessions_(std::move(sessions))
    , auth_service_(std::move(auth_service)) {
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
        .handler([this](const http_request& req) { return handle_login(req); });
    router->add_route(login.build());
    registry->register_route(login.build());

    auto logout = router->post("/api/v1/auth/logout")
        .summary("User logout")
        .description("Logout current session")
        .tags({"auth"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_logout(req); });
    router->add_route(logout.build());
    registry->register_route(logout.build());

    auto signup = router->post("/api/v1/auth/signup")
        .summary("User signup")
        .description("Create a new account (when self-registration is enabled)")
        .tags({"auth"})
        .handler([this](const http_request& req) { return handle_signup(req); });
    router->add_route(signup.build());
    registry->register_route(signup.build());

    auto bootstrap_status = router->get("/api/v1/auth/bootstrap-status")
        .summary("Get bootstrap status")
        .description("Check if system is in bootstrap mode awaiting initial admin")
        .tags({"auth"})
        .handler([this](const http_request& req) { return handle_bootstrap_status(req); });
    router->add_route(bootstrap_status.build());
    registry->register_route(bootstrap_status.build());

    auto bootstrap = router->post("/api/v1/auth/bootstrap")
        .summary("Create initial admin")
        .description("Create initial admin account (bootstrap mode only, localhost only)")
        .tags({"auth"})
        .handler([this](const http_request& req) { return handle_create_initial_admin(req); });
    router->add_route(bootstrap.build());
    registry->register_route(bootstrap.build());

    // Account management routes
    auto list_accounts = router->get("/api/v1/accounts")
        .summary("List accounts")
        .description("Retrieve accounts with pagination")
        .tags({"accounts"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_list_accounts(req); });
    router->add_route(list_accounts.build());
    registry->register_route(list_accounts.build());

    auto create_account = router->post("/api/v1/accounts")
        .summary("Create account")
        .description("Create a new user account")
        .tags({"accounts"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_create_account(req); });
    router->add_route(create_account.build());
    registry->register_route(create_account.build());

    auto delete_account = router->delete_("/api/v1/accounts/{id}")
        .summary("Delete account")
        .description("Delete an account by ID")
        .tags({"accounts"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_delete_account(req); });
    router->add_route(delete_account.build());
    registry->register_route(delete_account.build());

    auto update_account = router->put("/api/v1/accounts/{id}")
        .summary("Update account")
        .description("Update account details")
        .tags({"accounts"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_update_account(req); });
    router->add_route(update_account.build());
    registry->register_route(update_account.build());

    auto account_history = router->get("/api/v1/accounts/{username}/history")
        .summary("Get account history")
        .description("Retrieve version history for an account")
        .tags({"accounts"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_get_account_history(req); });
    router->add_route(account_history.build());
    registry->register_route(account_history.build());

    auto lock_accounts = router->post("/api/v1/accounts/lock")
        .summary("Lock accounts")
        .description("Lock one or more accounts")
        .tags({"accounts"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_lock_accounts(req); });
    router->add_route(lock_accounts.build());
    registry->register_route(lock_accounts.build());

    auto unlock_accounts = router->post("/api/v1/accounts/unlock")
        .summary("Unlock accounts")
        .description("Unlock one or more accounts")
        .tags({"accounts"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_unlock_accounts(req); });
    router->add_route(unlock_accounts.build());
    registry->register_route(unlock_accounts.build());

    auto login_info = router->get("/api/v1/accounts/login-info")
        .summary("List login info")
        .description("Retrieve login info for all accounts")
        .tags({"accounts"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_list_login_info(req); });
    router->add_route(login_info.build());
    registry->register_route(login_info.build());

    auto reset_password = router->post("/api/v1/accounts/reset-password")
        .summary("Reset password")
        .description("Admin-initiated password reset for accounts")
        .tags({"accounts"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_reset_password(req); });
    router->add_route(reset_password.build());
    registry->register_route(reset_password.build());

    // Current user routes
    auto change_password = router->post("/api/v1/me/change-password")
        .summary("Change password")
        .description("Change own password")
        .tags({"me"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_change_password(req); });
    router->add_route(change_password.build());
    registry->register_route(change_password.build());

    auto update_email = router->put("/api/v1/me/email")
        .summary("Update email")
        .description("Update own email address")
        .tags({"me"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_update_my_email(req); });
    router->add_route(update_email.build());
    registry->register_route(update_email.build());

    // RBAC routes
    auto list_roles = router->get("/api/v1/roles")
        .summary("List roles")
        .description("List all roles in the system")
        .tags({"rbac"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_list_roles(req); });
    router->add_route(list_roles.build());
    registry->register_route(list_roles.build());

    auto get_role = router->get("/api/v1/roles/{id}")
        .summary("Get role")
        .description("Get a specific role by ID")
        .tags({"rbac"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_get_role(req); });
    router->add_route(get_role.build());
    registry->register_route(get_role.build());

    auto list_permissions = router->get("/api/v1/permissions")
        .summary("List permissions")
        .description("List all permissions in the system")
        .tags({"rbac"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_list_permissions(req); });
    router->add_route(list_permissions.build());
    registry->register_route(list_permissions.build());

    auto assign_role = router->post("/api/v1/accounts/{id}/roles")
        .summary("Assign role")
        .description("Assign a role to an account")
        .tags({"rbac"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_assign_role(req); });
    router->add_route(assign_role.build());
    registry->register_route(assign_role.build());

    auto revoke_role = router->delete_("/api/v1/accounts/{accountId}/roles/{roleId}")
        .summary("Revoke role")
        .description("Revoke a role from an account")
        .tags({"rbac"})
        .auth_required()
        .roles({"admin"})
        .handler([this](const http_request& req) { return handle_revoke_role(req); });
    router->add_route(revoke_role.build());
    registry->register_route(revoke_role.build());

    auto account_roles = router->get("/api/v1/accounts/{id}/roles")
        .summary("Get account roles")
        .description("Get all roles assigned to an account")
        .tags({"rbac"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_get_account_roles(req); });
    router->add_route(account_roles.build());
    registry->register_route(account_roles.build());

    auto account_perms = router->get("/api/v1/accounts/{id}/permissions")
        .summary("Get account permissions")
        .description("Get effective permissions for an account")
        .tags({"rbac"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_get_account_permissions(req); });
    router->add_route(account_perms.build());
    registry->register_route(account_perms.build());

    // Session routes
    auto list_sessions = router->get("/api/v1/sessions")
        .summary("List sessions")
        .description("List session history")
        .tags({"sessions"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_list_sessions(req); });
    router->add_route(list_sessions.build());
    registry->register_route(list_sessions.build());

    auto session_stats = router->get("/api/v1/sessions/statistics")
        .summary("Get session statistics")
        .description("Get aggregated session statistics")
        .tags({"sessions"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_get_session_statistics(req); });
    router->add_route(session_stats.build());
    registry->register_route(session_stats.build());

    auto active_sessions = router->get("/api/v1/sessions/active")
        .summary("Get active sessions")
        .description("Get currently active sessions")
        .tags({"sessions"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_get_active_sessions(req); });
    router->add_route(active_sessions.build());
    registry->register_route(active_sessions.build());

    BOOST_LOG_SEV(lg(), info) << "IAM routes registered: 26 endpoints";
}

// Handler implementations

asio::awaitable<http_response> iam_routes::handle_login(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling login request";

    try {
        auto login_req = rfl::json::read<iam::messaging::login_request>(req.body);
        if (!login_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        auto result = account_service_.authenticate(
            login_req->username, login_req->password);

        if (!result) {
            BOOST_LOG_SEV(lg(), warn) << "Login failed for user: " << login_req->username;
            co_return http_response::unauthorized("Invalid credentials");
        }

        iam::messaging::login_response resp;
        resp.success = true;
        resp.account_id = result->id;
        resp.password_reset_required = result->password_reset_required;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Login error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_logout(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling logout request";

    if (!req.authenticated_user) {
        co_return http_response::unauthorized("Not authenticated");
    }

    try {
        // Mark session as logged out
        sessions_->remove_session(req.remote_address);
        co_return http_response::json(R"({"success":true})");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Logout error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_signup(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling signup request";

    try {
        // Check if signups are enabled
        if (!system_flags_->get_bool("system.user_signups")) {
            co_return http_response::forbidden("User signups are disabled");
        }

        auto signup_req = rfl::json::read<iam::messaging::signup_request>(req.body);
        if (!signup_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        iam::service::signup_service signup_service(ctx_, system_flags_);
        auto result = signup_service.signup(
            signup_req->username, signup_req->password, signup_req->email);

        if (!result) {
            co_return http_response::bad_request("Signup failed");
        }

        iam::messaging::signup_response resp;
        resp.success = true;
        resp.account_id = *result;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Signup error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_bootstrap_status(const http_request&) {
    BOOST_LOG_SEV(lg(), debug) << "Handling bootstrap status request";

    try {
        iam::service::bootstrap_mode_service bootstrap_service(ctx_);
        bool in_bootstrap = bootstrap_service.is_bootstrap_mode();

        iam::messaging::bootstrap_status_response resp;
        resp.in_bootstrap_mode = in_bootstrap;
        resp.message = in_bootstrap ?
            "System is in bootstrap mode - create initial admin" :
            "System is configured";

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Bootstrap status error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_create_initial_admin(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling create initial admin request";

    try {
        // Check if request is from localhost
        bool is_localhost = req.remote_address.starts_with("127.0.0.1") ||
                           req.remote_address.starts_with("::1");
        if (!is_localhost) {
            co_return http_response::forbidden("Bootstrap only allowed from localhost");
        }

        iam::service::bootstrap_mode_service bootstrap_service(ctx_);
        if (!bootstrap_service.is_bootstrap_mode()) {
            co_return http_response::forbidden("System is not in bootstrap mode");
        }

        auto admin_req = rfl::json::read<iam::messaging::create_initial_admin_request>(req.body);
        if (!admin_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        iam::service::account_setup_service setup_service(ctx_);
        auto result = setup_service.create_initial_admin(
            admin_req->username, admin_req->password, admin_req->email);

        if (!result) {
            co_return http_response::bad_request("Failed to create admin");
        }

        iam::messaging::create_initial_admin_response resp;
        resp.success = true;
        resp.account_id = *result;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Create initial admin error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_list_accounts(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling list accounts request";

    try {
        std::uint32_t offset = 0;
        std::uint32_t limit = 100;

        auto offset_str = req.get_query_param("offset");
        auto limit_str = req.get_query_param("limit");

        if (!offset_str.empty()) offset = std::stoul(offset_str);
        if (!limit_str.empty()) limit = std::stoul(limit_str);

        auto accounts = account_service_.list_accounts(offset, limit);
        auto total = account_service_.count_accounts();

        iam::messaging::list_accounts_response resp;
        resp.accounts = accounts;
        resp.total_count = total;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List accounts error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_create_account(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling create account request";

    try {
        auto create_req = rfl::json::read<iam::messaging::create_account_request>(req.body);
        if (!create_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        auto result = account_service_.create_account(
            create_req->username, create_req->password,
            create_req->totp_secret, create_req->email);

        if (!result) {
            co_return http_response::bad_request("Failed to create account");
        }

        iam::messaging::create_account_response resp;
        resp.account_id = *result;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Create account error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_delete_account(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling delete account request";

    try {
        auto account_id = req.get_path_param("id");
        if (account_id.empty()) {
            co_return http_response::bad_request("Account ID required");
        }

        auto uuid = boost::uuids::string_generator()(account_id);
        bool success = account_service_.delete_account(uuid);

        iam::messaging::delete_account_response resp;
        resp.success = success;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Delete account error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_update_account(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling update account request";

    try {
        auto account_id = req.get_path_param("id");
        if (account_id.empty()) {
            co_return http_response::bad_request("Account ID required");
        }

        auto update_req = rfl::json::read<iam::messaging::update_account_request>(req.body);
        if (!update_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        auto uuid = boost::uuids::string_generator()(account_id);
        bool success = account_service_.update_account(uuid, update_req->email);

        iam::messaging::update_account_response resp;
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

        auto history = account_service_.get_account_history(username);

        iam::messaging::get_account_history_response resp;
        resp.versions = history;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get account history error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_lock_accounts(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling lock accounts request";

    try {
        auto lock_req = rfl::json::read<iam::messaging::lock_account_request>(req.body);
        if (!lock_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        std::vector<iam::messaging::account_operation_result> results;
        for (const auto& id : lock_req->account_ids) {
            bool success = account_service_.lock_account(id);
            results.push_back({id, success, success ? "" : "Failed to lock"});
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

    try {
        auto unlock_req = rfl::json::read<iam::messaging::unlock_account_request>(req.body);
        if (!unlock_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        std::vector<iam::messaging::account_operation_result> results;
        for (const auto& id : unlock_req->account_ids) {
            bool success = account_service_.unlock_account(id);
            results.push_back({id, success, success ? "" : "Failed to unlock"});
        }

        iam::messaging::unlock_account_response resp;
        resp.results = results;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Unlock accounts error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_list_login_info(const http_request&) {
    BOOST_LOG_SEV(lg(), debug) << "Handling list login info request";

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

    try {
        auto reset_req = rfl::json::read<iam::messaging::reset_password_request>(req.body);
        if (!reset_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        std::vector<iam::messaging::account_operation_result> results;
        for (const auto& id : reset_req->account_ids) {
            bool success = account_service_.reset_password(id);
            results.push_back({id, success, success ? "" : "Failed to reset password"});
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

    if (!req.authenticated_user) {
        co_return http_response::unauthorized("Not authenticated");
    }

    try {
        auto change_req = rfl::json::read<iam::messaging::change_password_request>(req.body);
        if (!change_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        auto uuid = boost::uuids::string_generator()(req.authenticated_user->subject);
        bool success = account_service_.change_password(
            uuid, change_req->current_password, change_req->new_password);

        iam::messaging::change_password_response resp;
        resp.success = success;
        resp.message = success ? "Password changed" : "Failed to change password";

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Change password error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_update_my_email(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling update my email request";

    if (!req.authenticated_user) {
        co_return http_response::unauthorized("Not authenticated");
    }

    try {
        auto update_req = rfl::json::read<iam::messaging::update_my_email_request>(req.body);
        if (!update_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        auto uuid = boost::uuids::string_generator()(req.authenticated_user->subject);
        bool success = account_service_.update_account(uuid, update_req->email);

        iam::messaging::update_my_email_response resp;
        resp.success = success;

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

        auto role = auth_service_->get_role(role_id);
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

    try {
        auto account_id = req.get_path_param("id");
        if (account_id.empty()) {
            co_return http_response::bad_request("Account ID required");
        }

        auto assign_req = rfl::json::read<iam::messaging::assign_role_request>(req.body);
        if (!assign_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        auto uuid = boost::uuids::string_generator()(account_id);
        bool success = auth_service_->assign_role(uuid, assign_req->role_id);

        iam::messaging::assign_role_response resp;
        resp.success = success;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Assign role error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_revoke_role(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling revoke role request";

    try {
        auto account_id = req.get_path_param("accountId");
        auto role_id = req.get_path_param("roleId");

        if (account_id.empty() || role_id.empty()) {
            co_return http_response::bad_request("Account ID and Role ID required");
        }

        auto uuid = boost::uuids::string_generator()(account_id);
        auto role_uuid = boost::uuids::string_generator()(role_id);
        bool success = auth_service_->revoke_role(uuid, role_uuid);

        iam::messaging::revoke_role_response resp;
        resp.success = success;

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
        auto permissions = auth_service_->get_account_permissions(uuid);

        iam::messaging::get_account_permissions_response resp;
        resp.permissions = permissions;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get account permissions error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_list_sessions(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling list sessions request";

    try {
        auto account_id_str = req.get_query_param("account_id");
        std::uint32_t offset = 0;
        std::uint32_t limit = 100;

        auto offset_str = req.get_query_param("offset");
        auto limit_str = req.get_query_param("limit");

        if (!offset_str.empty()) offset = std::stoul(offset_str);
        if (!limit_str.empty()) limit = std::stoul(limit_str);

        std::optional<boost::uuids::uuid> account_id;
        if (!account_id_str.empty()) {
            account_id = boost::uuids::string_generator()(account_id_str);
        } else if (req.authenticated_user) {
            account_id = boost::uuids::string_generator()(req.authenticated_user->subject);
        }

        auto sessions = session_repo_.list_sessions(account_id, offset, limit);

        iam::messaging::list_sessions_response resp;
        resp.sessions = sessions;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List sessions error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_get_session_statistics(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get session statistics request";

    try {
        auto account_id_str = req.get_query_param("account_id");

        std::optional<boost::uuids::uuid> account_id;
        if (!account_id_str.empty()) {
            account_id = boost::uuids::string_generator()(account_id_str);
        }

        auto stats = session_repo_.get_statistics(account_id);

        iam::messaging::get_session_statistics_response resp;
        resp.statistics = stats;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get session statistics error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> iam_routes::handle_get_active_sessions(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get active sessions request";

    try {
        std::optional<boost::uuids::uuid> account_id;

        // Admin can see all, non-admin only sees own
        if (req.authenticated_user) {
            bool is_admin = false;
            for (const auto& role : req.authenticated_user->roles) {
                if (role == "admin") {
                    is_admin = true;
                    break;
                }
            }
            if (!is_admin) {
                account_id = boost::uuids::string_generator()(req.authenticated_user->subject);
            }
        }

        auto sessions = sessions_->get_active_sessions(account_id);

        iam::messaging::get_active_sessions_response resp;
        resp.sessions = sessions;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get active sessions error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

}
