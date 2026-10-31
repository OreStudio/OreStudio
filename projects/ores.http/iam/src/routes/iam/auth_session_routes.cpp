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
#include "ores.http/routes/iam/auth_session_routes.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/asio/co_spawn.hpp>
#include <rfl/json.hpp>

namespace ores::http::routes::iam {

namespace {

using http::domain::cookie_settings;
using http::domain::http_request;
using http::domain::http_response;
using http::domain::session_cookie;
using http::domain::session_record;

/// What the browser is told about itself: everything but the token.
std::string principal_of(const ores::security::jwt::jwt_claims& claims,
                         std::string_view username,
                         std::string_view email) {
    auto body = std::string(R"({"subject":")") + claims.subject + R"(")";
    body += R"(,"tenant":")" + claims.tenant_id.value_or(std::string{}) + R"(")";
    body += R"(,"party":")" + claims.party_id.value_or(std::string{}) + R"(")";
    body += R"(,"username":")" + std::string(username) + R"(")";
    if (!email.empty())
        body += R"(,"email":")" + std::string(email) + R"(")";
    body += R"(,"roles":[)";
    for (auto index = std::size_t{0}; index < claims.roles.size(); ++index) {
        if (index > 0)
            body += ',';
        body += '"' + claims.roles[index] + '"';
    }
    body += "]}";
    return body;
}

}

auth_session_routes::auth_session_routes(
    ores::nats::service::nats_client& session,
    std::shared_ptr<http::domain::session_store> sessions,
    std::shared_ptr<ores::security::jwt::jwt_authenticator> verifier,
    cookie_settings cookie)
    : session_(session)
    , sessions_(std::move(sessions))
    , verifier_(std::move(verifier))
    , cookie_(std::move(cookie)) {}

void auth_session_routes::register_routes(
    std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> registry) {

    BOOST_LOG_SEV(lg(), info) << "Registering the session door";

    /*
     * Signing in is reachable without a session, which is the point of it; ending a
     * session is reachable with whatever the caller still holds, so an expired
     * session can still be cleared.
     */
    auto sign_in_route = router->post("/api/v1/auth/sessions")
                             .summary("Sign in")
                             .description("Opens a session and sets the cookie it rides in")
                             .tags({"auth"})
                             .auth_optional()
                             .handler([this](const http_request& req) { return sign_in(req); });
    const auto sign_in_built = sign_in_route.build();
    router->add_route(sign_in_built);
    registry->register_route(sign_in_built);

    auto sign_out_route = router->delete_("/api/v1/auth/sessions")
                              .summary("Sign out")
                              .description("Ends the caller's session and clears the cookie")
                              .tags({"auth"})
                              .auth_optional()
                              .handler([this](const http_request& req) { return sign_out(req); });
    const auto sign_out_built = sign_out_route.build();
    router->add_route(sign_out_built);
    registry->register_route(sign_out_built);

    auto me_route =
        router->get("/api/v1/auth/sessions/me")
            .summary("Who am I")
            .description("The caller's identity, as the session or token states it")
            .tags({"auth"})
            .auth_required()
            .handler([this](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return me(req);
            });
    const auto me_built = me_route.build();
    router->add_route(me_built);
    registry->register_route(me_built);
}

boost::asio::awaitable<http_response> auth_session_routes::sign_in(const http_request& req) {
    using request_type = ores::iam::messaging::login_request;

    const auto parsed = rfl::json::read<request_type>(req.body);
    if (!parsed) {
        co_return http_response::bad_request("Invalid request body");
    }

    /*
     * The sign-in itself is IAM's: the gateway holds no credential and decides
     * nothing about who may enter. It keeps the token it is given and gives the
     * browser an identifier instead.
     */
    auto delegated = session_.with_delegation(std::string{});
    try {
        const auto reply =
            delegated.authenticated_request(parsed->nats_subject,
                                            ores::nats::default_wire_codec().encode(*parsed),
                                            ores::nats::service::default_request_timeout);

        if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
            it != reply.headers.end()) {
            co_return http_response::unauthorized(it->second);
        }

        const auto decoded =
            ores::nats::default_wire_codec().decode<request_type::response_type>(reply.data);
        if (!decoded) {
            co_return http_response::internal_error(std::string("Failed to decode response: ") +
                                                    decoded.error().what());
        }
        if (!decoded->success || decoded->token.empty()) {
            // The service states why in its own words and code; the browser is told
            // them rather than a status.
            auto body = rfl::json::write(*decoded);
            co_return http_response::json(body, http::domain::http_status::unauthorized);
        }

        auto* effective = verifier_.get();
        if (effective == nullptr || !effective->is_configured()) {
            BOOST_LOG_SEV(lg(), error) << "Signing in failed: no verifier is configured";
            co_return http_response::unauthorized("Authentication is not configured");
        }

        auto claims_result = effective->validate(decoded->token);
        if (!claims_result) {
            BOOST_LOG_SEV(lg(), error)
                << "Signing in failed: the token IAM minted does not verify: "
                << ores::security::jwt::to_string(claims_result.error());
            co_return http_response::internal_error("The token could not be read");
        }

        auto claims = std::move(claims_result.value());
        const auto principal = principal_of(claims, decoded->username, decoded->email);

        session_record record;
        record.token = decoded->token;
        record.claims = std::move(claims);
        const auto identifier = sessions_->open(std::move(record));

        auto response = http_response::json(principal);
        response.set_header("Set-Cookie", session_cookie::issue(cookie_, identifier));
        co_return response;
    } catch (const ores::nats::service::session_expired_error& e) {
        co_return http_response::unauthorized(e.what());
    } catch (const std::exception& e) {
        co_return http_response::internal_error(e.what());
    }
}

boost::asio::awaitable<http_response> auth_session_routes::sign_out(const http_request& req) {
    if (const auto identifier = session_cookie::read(req.get_header("cookie"), cookie_);
        identifier.has_value()) {
        sessions_->close(*identifier);
    }

    auto response = http_response::json(R"({"signedOut":true})");
    // Clearing is unconditional: a caller with a cookie this server does not know
    // still needs the browser to stop sending it.
    response.set_header("Set-Cookie", session_cookie::clear(cookie_));
    co_return response;
}

http_response auth_session_routes::me(const http_request& req) const {
    if (!req.authenticated_user.has_value())
        return http_response::unauthorized("Not signed in");

    const auto& claims = *req.authenticated_user;
    return http_response::json(principal_of(
        claims, claims.username.value_or(std::string{}), claims.email.value_or(std::string{})));
}

}
