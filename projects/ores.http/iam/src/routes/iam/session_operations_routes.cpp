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
#include "ores.http/routes/iam/session_operations_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/session_operations_protocol.hpp"
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
std::optional<T> session_operations_read_body(std::string_view body) {
    try {
        return ores::utility::codec::wire_traits<T>::read_json(body);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

}

boost::asio::awaitable<http_response>
session_operations_routes::handle_get_active_sessions(const http_request& req,
                                                      nats_client& session) {
    using request_type = messaging::get_active_sessions_request;
    try {
        co_return ores::http::forwarding::respond(req, session, request_type{});
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
session_operations_routes::handle_end_session(const http_request& req, nats_client& session) {
    using request_type = messaging::end_session_request;
    try {
        const auto parsed = session_operations_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

void session_operations_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering session_operations routes";

    auto get_active_sessions_route =
        router->post("/api/v1/iam/ops/get_active_sessions")
            .summary("Get active sessions")
            .description("Forwards to the iam.v1.ops.get_active_sessions operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::get_active_sessions_request>()
            .response<messaging::get_active_sessions_response>()
            .handler([&session](const http_request& req) {
                return handle_get_active_sessions(req, session);
            });
    const auto get_active_sessions_built = get_active_sessions_route.build();
    router->add_route(get_active_sessions_built);
    registry->register_route(get_active_sessions_built);

    auto end_session_route = router->post("/api/v1/iam/ops/end_session")
                                 .summary("End session")
                                 .description("Forwards to the iam.v1.ops.end_session operation.")
                                 .tags({"iam"})
                                 .auth_required()
                                 .body<messaging::end_session_request>()
                                 .response<messaging::end_session_response>()
                                 .handler([&session](const http_request& req) {
                                     return handle_end_session(req, session);
                                 });
    const auto end_session_built = end_session_route.build();
    router->add_route(end_session_built);
    registry->register_route(end_session_built);

    BOOST_LOG_SEV(lg(), info) << "session_operations routes registered: " << 2 << " endpoint(s)";
}

}
