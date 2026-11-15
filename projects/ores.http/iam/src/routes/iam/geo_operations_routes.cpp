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
#include "ores.http/routes/iam/geo_operations_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/geo_operations_protocol.hpp"
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
std::optional<T> geo_operations_read_body(std::string_view body) {
    try {
        return ores::utility::codec::wire_traits<T>::read_json(body);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

}

boost::asio::awaitable<http_response>
geo_operations_routes::handle_lookup_country(const http_request& req, nats_client& session) {
    using request_type = messaging::lookup_country_request;
    try {
        const auto parsed = geo_operations_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

void geo_operations_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering geo_operations routes";

    auto lookup_country_route =
        router->post("/api/v1/iam/ops/lookup_country")
            .summary("Lookup country")
            .description("Forwards to the iam.v1.ops.lookup_country operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::lookup_country_request>()
            .response<messaging::lookup_country_response>()
            .handler([&session](const http_request& req) {
                return handle_lookup_country(req, session);
            });
    const auto lookup_country_built = lookup_country_route.build();
    router->add_route(lookup_country_built);
    registry->register_route(lookup_country_built);

    BOOST_LOG_SEV(lg(), info) << "geo_operations routes registered: " << 1 << " endpoint(s)";
}

}
