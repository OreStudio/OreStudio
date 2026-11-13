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
#ifndef ORES_HTTP_ROUTES_MARKETDATA_SUBSCRIPTION_ROUTES_HPP
#define ORES_HTTP_ROUTES_MARKETDATA_SUBSCRIPTION_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.marketdata.api/domain/route.hpp"
#include "ores.marketdata.api/domain/subscription.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>
#include <vector>

namespace ores::http::routes::marketdata {

/**
 * @brief What a party takes from the market data service: its subscriptions to
 * the providers, and the one feed that serves each of them.
 *
 * The service owns the party of a subscription and of a route, so the writes
 * here name the publisher and never the party: the party comes from the
 * caller's own token.
 */
class subscription_routes {
public:
    /**
     * @brief The subscriptions as the screen reads them, with the party that
     * holds them and the rights they carry.
     */
    static std::string
    subscriptions_json(const std::vector<ores::marketdata::domain::subscription>& subscriptions);

    /**
     * @brief The routes as the screen reads them: the feed that serves each
     * party and publisher.
     */
    static std::string routes_json(const std::vector<ores::marketdata::domain::route>& routes);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
