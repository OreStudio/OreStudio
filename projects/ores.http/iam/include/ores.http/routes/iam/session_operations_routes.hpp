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
 * Template: cpp_http_route_operation_header.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_HTTP_ROUTES_SESSION_OPERATIONS_ROUTES_HPP
#define ORES_HTTP_ROUTES_SESSION_OPERATIONS_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>
#include <string_view>

namespace ores::http::routes::iam {

/**
 * @brief Every operation session_operations declares and exposes, as one HTTP route each.
 *
 * The unit is the model's own declared protocol addressed over HTTP, so a
 * message the model exposes appears as a route without an edit here and a
 * message it stops exposing -- or loses -- takes its route with it. A route
 * is a POST whose body is the canonical request, because a declared operation
 * states no verb and the request is the whole of what it sends.
 *
 * The unit authorises nobody. The service's generated handler owns the
 * operation's permission, and the caller's own token travels with every
 * request this unit forwards.
 */
class session_operations_routes {
private:
    inline static std::string_view logger_name = "ores.http.routes.iam.session_operations_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the session_operations operation routes.
     *
     * @param session The client every route forwards on. The caller's token
     * is taken from each HTTP request and delegated on it, so the unit holds
     * no authorisation state of its own.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/iam/ops/get_active_sessions — Get active sessions.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_get_active_sessions(const ores::http::domain::http_request& req,
                               ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/iam/ops/end_session — End session.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_end_session(const ores::http::domain::http_request& req,
                       ores::nats::service::nats_client& session);
};

}

#endif
