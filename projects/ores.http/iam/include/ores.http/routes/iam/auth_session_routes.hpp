/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_HTTP_ROUTES_AUTH_SESSION_ROUTES_HPP
#define ORES_HTTP_ROUTES_AUTH_SESSION_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/session_cookie.hpp"
#include "ores.http.api/domain/session_store.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>
#include <string_view>

namespace ores::http::routes::iam {

/**
 * @brief Signing in and out as resources, for a browser.
 *
 * =POST /api/v1/auth/sessions= opens a session: the gateway asks IAM who the caller
 * is, keeps the token it is given, and hands the browser an opaque identifier in a
 * cookie. =DELETE= ends it, and =/me= says who the caller is without ever handing the
 * token back. A machine client keeps its bearer token and is unaffected.
 */
class auth_session_routes final {
public:
    auth_session_routes(ores::nats::service::nats_client& session,
                        std::shared_ptr<http::domain::session_store> sessions,
                        std::shared_ptr<ores::security::jwt::jwt_authenticator> verifier,
                        http::domain::cookie_settings cookie);

    void register_routes(std::shared_ptr<http::net::router> router,
                         std::shared_ptr<http::openapi::endpoint_registry> registry);

    /// Opens a session and answers the principal, never the token.
    boost::asio::awaitable<http::domain::http_response>
    sign_in(const http::domain::http_request& req);

    /// Ends the session the caller's cookie names, and clears the cookie.
    boost::asio::awaitable<http::domain::http_response>
    sign_out(const http::domain::http_request& req);

    /// Answers who the caller is, from whichever proof reached the route.
    [[nodiscard]] http::domain::http_response me(const http::domain::http_request& req) const;

private:
    inline static std::string_view logger_name = "ores.http.routes.iam.auth_session";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    ores::nats::service::nats_client& session_;
    std::shared_ptr<http::domain::session_store> sessions_;
    std::shared_ptr<ores::security::jwt::jwt_authenticator> verifier_;
    http::domain::cookie_settings cookie_;
};

}

#endif
