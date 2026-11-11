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
#ifndef ORES_HTTP_ROUTES_SYNTHETIC_FEED_OPERATION_ROUTES_HPP
#define ORES_HTTP_ROUTES_SYNTHETIC_FEED_OPERATION_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>

namespace ores::http::routes::synthetic {

/**
 * @brief The feed screen's operations: one feed or one folder's feeds started
 * and stopped, a page of the feeds a filter matches with whether each runs, and
 * the start and stop of a selection.
 *
 * A start or stop the service refuses conflicts with the feed's state, so it
 * answers 409 with the service's words. A refused page or selection is a bad
 * filter, so it answers 400.
 */
class feed_operation_routes {
public:
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
