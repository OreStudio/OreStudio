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
#ifndef ORES_HTTP_ROUTES_IAM_BROWSER_SESSION_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_BROWSER_SESSION_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/session_cookie.hpp"
#include "ores.http.api/domain/session_store.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/iam/browser_session.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::routes::iam {

/**
 * @brief The browser's session, at the BFF's addresses: sign in, read the
 * session, sign out, choose the party a login waits on, move to another party,
 * and change the account's own password.
 *
 * The gateway keeps the token and gives the browser a cookie. Beside the token
 * the session holds what the login answered, which the browser reads back, so
 * these routes own the record's view and keep it in step with each change.
 */
class browser_session_routes final {
public:
    using verifier_finder =
        std::function<std::shared_ptr<ores::security::jwt::jwt_authenticator>()>;

    browser_session_routes(ores::nats::service::nats_client& session,
                           std::shared_ptr<http::domain::session_store> sessions,
                           verifier_finder find_verifier,
                           http::domain::cookie_settings cookie);

    void register_routes(std::shared_ptr<http::net::router> router,
                         std::shared_ptr<http::openapi::endpoint_registry> registry);

    /**
     * @brief A session the login opened as the browser reads it, or what a login
     * waiting for a party answers.
     */
    [[nodiscard]] static std::string login_json(const ores::iam::messaging::login_response& login);

private:
    inline static std::string_view logger_name = "ores.http.routes.iam.browser_session_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    http::domain::http_response sign_in(const http::domain::http_request& req);
    http::domain::http_response read(const http::domain::http_request& req);
    http::domain::http_response sign_out(const http::domain::http_request& req);
    http::domain::http_response choose_party(const http::domain::http_request& req);
    http::domain::http_response switch_party(const http::domain::http_request& req);
    http::domain::http_response change_password(const http::domain::http_request& req);

    /// The identifier the request's cookie carries, when it carries one.
    [[nodiscard]] std::optional<std::string>
    identifier_of(const http::domain::http_request& req) const;

    /// The claims a token states, or nothing when it does not verify.
    [[nodiscard]] std::optional<ores::security::jwt::jwt_claims>
    claims_of(const std::string& token) const;

    ores::nats::service::nats_client& session_;
    std::shared_ptr<http::domain::session_store> sessions_;
    verifier_finder find_verifier_;
    http::domain::cookie_settings cookie_;
};

}

#endif
