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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_http_route_operation_implementation.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http/routes/iam/login_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <exception>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

namespace ores::http::routes::iam {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace messaging = ores::iam::messaging;

namespace {

/**
 * @brief The request a body states, or nothing when it states none.
 *
 * The request's own codec reads it, so this unit needs no shape of the types the
 * request holds.
 */
template <typename T>
std::optional<T> login_read_body(std::string_view body) {
    try {
        return ores::utility::codec::wire_traits<T>::read_json(body);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

}

boost::asio::awaitable<http_response> login_routes::handle_login(const http_request& req,
                                                                 nats_client& session) {
    using request_type = messaging::login_request;
    try {
        const auto parsed = login_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> login_routes::handle_logout(const http_request& req,
                                                                  nats_client& session) {
    using request_type = messaging::logout_request;
    try {
        co_return ores::http::forwarding::respond(req, session, request_type{});
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

void login_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                   std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                   nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering login routes";

    auto login_route =
        router->post("/api/v1/iam/ops/login")
            .summary("Login")
            .description("Forwards to the iam.v1.ops.login operation.")
            .tags({"iam"})
            .auth_optional()
            .rate_limit("credential")
            .body<messaging::login_request>()
            .response<messaging::login_response>()
            .handler([&session](const http_request& req) { return handle_login(req, session); });
    const auto login_built = login_route.build();
    router->add_route(login_built);
    registry->register_route(login_built);

    auto logout_route =
        router->post("/api/v1/iam/ops/logout")
            .summary("Logout")
            .description("Forwards to the iam.v1.ops.logout operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::logout_request>()
            .response<messaging::logout_response>()
            .handler([&session](const http_request& req) { return handle_logout(req, session); });
    const auto logout_built = logout_route.build();
    router->add_route(logout_built);
    registry->register_route(logout_built);

    BOOST_LOG_SEV(lg(), info) << "login routes registered: " << 2 << " endpoint(s)";
}

}
