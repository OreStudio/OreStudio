/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
#ifndef ORES_HTTP_ROUTES_DQ_BUNDLE_ROSTER_ROUTES_HPP
#define ORES_HTTP_ROUTES_DQ_BUNDLE_ROSTER_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>
#include <string_view>

namespace ores::http::routes::dq {

/**
 * @brief The bundle roster, as one HTTP route.
 *
 * The roster is an operation and not an entity derivation: its answer is the
 * level a bundle publishes at and the number of rows each member has staged,
 * which no table holds. So the route is hand-written beside the generated
 * units rather than derived from a model, and it forwards to the
 * =dq.v1.ops.bundle_roster= operation the service owns.
 *
 * The route authorises nobody. The service's handler owns the read, and the
 * caller's token travels with the forward.
 */
class bundle_roster_routes {
private:
    inline static std::string_view logger_name = "ores.http.routes.dq.bundle_roster_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the roster route.
     *
     * @param session The client the route forwards on.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/dq/bundle_roster — Read the roster, or one bundle of it.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_roster(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session);
};

} // namespace ores::http::routes::dq

#endif
