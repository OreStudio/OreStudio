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
#include "ores.http/routes/iam/bootstrap_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
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
std::optional<T> bootstrap_read_body(std::string_view body) {
    try {
        return ores::utility::codec::wire_traits<T>::read_json(body);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

}

boost::asio::awaitable<http_response>
bootstrap_routes::handle_bootstrap_status(const http_request& req, nats_client& session) {
    using request_type = messaging::bootstrap_status_request;
    try {
        co_return ores::http::forwarding::respond(req, session, request_type{});
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
bootstrap_routes::handle_create_initial_admin(const http_request& req, nats_client& session) {
    using request_type = messaging::create_initial_admin_request;
    try {
        const auto parsed = bootstrap_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

void bootstrap_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering bootstrap routes";

    auto bootstrap_status_route =
        router->post("/api/v1/iam/ops/bootstrap_status")
            .summary("Bootstrap status")
            .description("Forwards to the iam.v1.ops.bootstrap_status operation.")
            .tags({"iam"})
            .auth_optional()
            .body<messaging::bootstrap_status_request>()
            .response<messaging::bootstrap_status_response>()
            .handler([&session](const http_request& req) {
                return handle_bootstrap_status(req, session);
            });
    const auto bootstrap_status_built = bootstrap_status_route.build();
    router->add_route(bootstrap_status_built);
    registry->register_route(bootstrap_status_built);

    auto create_initial_admin_route =
        router->post("/api/v1/iam/ops/create_initial_admin")
            .summary("Create initial admin")
            .description("Forwards to the iam.v1.ops.create_initial_admin operation.")
            .tags({"iam"})
            .auth_optional()
            .body<messaging::create_initial_admin_request>()
            .response<messaging::create_initial_admin_response>()
            .handler([&session](const http_request& req) {
                return handle_create_initial_admin(req, session);
            });
    const auto create_initial_admin_built = create_initial_admin_route.build();
    router->add_route(create_initial_admin_built);
    registry->register_route(create_initial_admin_built);

    BOOST_LOG_SEV(lg(), info) << "bootstrap routes registered: " << 2 << " endpoint(s)";
}

}
