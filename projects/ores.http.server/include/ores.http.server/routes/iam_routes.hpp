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
#ifndef ORES_HTTP_SERVER_ROUTES_IAM_ROUTES_HPP
#define ORES_HTTP_SERVER_ROUTES_IAM_ROUTES_HPP

#include <memory>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include <rfl/json.hpp>
#include "ores.http/net/router.hpp"
#include "ores.http/openapi/endpoint_registry.hpp"
#include "ores.http/middleware/jwt_authenticator.hpp"
#include "ores.iam/service/account_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/repository/session_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.geo/service/geolocation_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::http_server::routes {

/**
 * @brief Result of successful authorization check.
 */
struct auth_result {
    boost::uuids::uuid account_id;
    bool is_admin;
};

/**
 * @brief Registers IAM (Identity and Access Management) HTTP endpoints.
 *
 * Maps the following protocol messages to REST endpoints:
 *
 * Authentication:
 * - POST /api/v1/auth/login - login_request
 * - POST /api/v1/auth/logout - logout_request
 * - POST /api/v1/auth/signup - signup_request
 * - GET /api/v1/auth/bootstrap-status - bootstrap_status_request
 * - POST /api/v1/auth/bootstrap - create_initial_admin_request
 *
 * Account Management:
 * - GET /api/v1/accounts - get_accounts_request
 * - POST /api/v1/accounts - save_account_request
 * - DELETE /api/v1/accounts/{id} - delete_account_request
 * - PUT /api/v1/accounts/{id} - save_account_request
 * - GET /api/v1/accounts/{username}/history - get_account_history_request
 * - POST /api/v1/accounts/lock - lock_account_request
 * - POST /api/v1/accounts/unlock - unlock_account_request
 * - GET /api/v1/accounts/login-info - list_login_info_request
 * - POST /api/v1/accounts/reset-password - reset_password_request
 *
 * Current User Operations:
 * - POST /api/v1/me/change-password - change_password_request
 * - PUT /api/v1/me/email - update_my_email_request
 *
 * RBAC:
 * - GET /api/v1/roles - list_roles_request
 * - GET /api/v1/roles/{id} - get_role_request
 * - GET /api/v1/permissions - list_permissions_request
 * - POST /api/v1/accounts/{id}/roles - assign_role_request
 * - DELETE /api/v1/accounts/{id}/roles/{roleId} - revoke_role_request
 * - GET /api/v1/accounts/{id}/roles - get_account_roles_request
 * - GET /api/v1/accounts/{id}/permissions - get_account_permissions_request
 *
 * Sessions:
 * - GET /api/v1/sessions - list_sessions_request
 * - GET /api/v1/sessions/statistics - get_session_statistics_request
 * - GET /api/v1/sessions/active - get_active_sessions_request
 */
class iam_routes final {
public:
    iam_routes(database::context ctx,
        std::shared_ptr<variability::service::system_flags_service> system_flags,
        std::shared_ptr<comms::service::auth_session_service> sessions,
        std::shared_ptr<iam::service::authorization_service> auth_service,
        std::shared_ptr<http::middleware::jwt_authenticator> authenticator,
        std::shared_ptr<geo::service::geolocation_service> geo_service);

    /**
     * @brief Registers all IAM routes with the router.
     */
    void register_routes(std::shared_ptr<http::net::router> router,
        std::shared_ptr<http::openapi::endpoint_registry> registry);

private:
    inline static std::string_view logger_name = "ores.http.server.routes.iam_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    // Authentication handlers
    boost::asio::awaitable<http::domain::http_response>
    handle_login(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_logout(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_signup(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_bootstrap_status(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_create_initial_admin(const http::domain::http_request& req);

    // Account management handlers
    boost::asio::awaitable<http::domain::http_response>
    handle_list_accounts(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_create_account(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_delete_account(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_update_account(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get_account_history(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_lock_accounts(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_unlock_accounts(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_list_login_info(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_reset_password(const http::domain::http_request& req);

    // Current user handlers
    boost::asio::awaitable<http::domain::http_response>
    handle_change_password(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_update_my_email(const http::domain::http_request& req);

    // RBAC handlers
    boost::asio::awaitable<http::domain::http_response>
    handle_list_roles(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get_role(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_list_permissions(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_assign_role(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_revoke_role(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get_account_roles(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get_account_permissions(const http::domain::http_request& req);

    // Session handlers
    boost::asio::awaitable<http::domain::http_response>
    handle_list_sessions(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get_session_statistics(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get_active_sessions(const http::domain::http_request& req);

    /**
     * @brief Check authentication and optionally verify a specific permission.
     *
     * @param req The HTTP request containing authentication info
     * @param required_permission Permission to check (empty = no permission check)
     * @param operation_name Name of the operation for logging
     * @return auth_result on success, http_response error (401/403) on failure
     */
    std::expected<auth_result, http::domain::http_response> check_auth(
        const http::domain::http_request& req,
        std::string_view required_permission = "",
        std::string_view operation_name = "");

    /**
     * @brief Parse JSON request body into a typed struct.
     *
     * @tparam T The request type to parse into
     * @param req The HTTP request containing the body
     * @param operation_name Name of the operation for logging
     * @return Parsed request on success, http_response error (400) on failure
     */
    template<typename T>
    std::expected<T, http::domain::http_response> parse_body(
        const http::domain::http_request& req,
        std::string_view operation_name) {
        using ores::logging::warn;
        auto result = rfl::json::read<T>(req.body);
        if (!result) {
            BOOST_LOG_SEV(lg(), warn) << operation_name
                                      << ": invalid request body";
            return std::unexpected(
                http::domain::http_response::bad_request("Invalid request body"));
        }
        return *result;
    }

    database::context ctx_;
    iam::service::account_service account_service_;
    iam::repository::session_repository session_repo_;
    std::shared_ptr<variability::service::system_flags_service> system_flags_;
    std::shared_ptr<comms::service::auth_session_service> sessions_;
    std::shared_ptr<iam::service::authorization_service> auth_service_;
    std::shared_ptr<http::middleware::jwt_authenticator> authenticator_;
    std::shared_ptr<geo::service::geolocation_service> geo_service_;
};

}

#endif
