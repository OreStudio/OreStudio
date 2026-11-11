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
#ifndef ORES_HTTP_ROUTES_SYNTHETIC_FEED_TICK_ROUTES_HPP
#define ORES_HTTP_ROUTES_SYNTHETIC_FEED_TICK_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/synthetic/feed_tick_buffers.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <memory>
#include <string>

namespace ores::http::routes::synthetic {

/**
 * @brief The feed screens' live reads: one feed's newest ticks, and the raster
 * of when each feed's ticks arrived in a window.
 *
 * Both read the tick buffers this unit fills by listening to each feed a read
 * names, as the BFF did, so the subscriptions follow what the screens look at.
 */
class feed_tick_routes {
public:
    /**
     * @brief Registers the routes on buffers that listen through the client's
     * transport.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief Registers the routes on the buffers given, which a test fills itself.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session,
                                std::shared_ptr<feed_tick_buffers> buffers);

    /**
     * @brief An instant as the browser's clock writes it: UTC to the millisecond.
     */
    [[nodiscard]] static std::string instant_text(std::chrono::system_clock::time_point at);
};

}

#endif
