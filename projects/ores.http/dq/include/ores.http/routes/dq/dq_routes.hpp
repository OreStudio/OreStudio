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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#ifndef ORES_HTTP_ROUTES_DQ_DQ_ROUTES_HPP
#define ORES_HTTP_ROUTES_DQ_DQ_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>

namespace ores::http::routes::dq {

/**
 * @brief Every DQ route unit, registered in one call.
 *
 * The aggregate is the one file in this part that lists the units, and the
 * route-units check fails when an opted-in model's unit is missing from it. A
 * unit that is not opted in has no header to include, so the list is also the
 * record of which DQ models are on the HTTP surface.
 */
class dq_routes {
public:
    /**
     * @brief Register every DQ unit on the router and the registry.
     *
     * @param router The router the routes are installed on.
     * @param registry The OpenAPI registry each route states itself in.
     * @param session The client each route forwards on; the caller's token
     * travels with every request, so the aggregate holds no state.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
