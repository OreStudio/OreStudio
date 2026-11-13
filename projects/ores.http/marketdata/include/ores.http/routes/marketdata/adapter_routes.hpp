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
#ifndef ORES_HTTP_ROUTES_MARKETDATA_ADAPTER_ROUTES_HPP
#define ORES_HTTP_ROUTES_MARKETDATA_ADAPTER_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.marketdata.api/domain/feed.hpp"
#include "ores.marketdata.api/domain/session.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>
#include <vector>

namespace ores::http::routes::marketdata {

/**
 * @brief The adapter screens: every adapter the market data service tracks,
 * with what it serves, what it was asked to serve, and the kind of source it
 * reads.
 *
 * The status names one adapter per feed and says nothing about the source; the
 * catalogue does, because a feed names its session and the session's environment
 * says whether the values are simulated or come from a supplier.
 */
class adapter_routes {
public:
    /**
     * @brief The adapters as the screen reads them: each status with the kind of
     * source its feed reads, or nothing when the catalogue no longer holds it,
     * beside the service's own success and message.
     */
    static std::string
    adapters_json(const ores::marketdata::messaging::list_adapter_status_response& reply,
                  const std::vector<ores::marketdata::domain::feed>& feeds,
                  const std::vector<ores::marketdata::domain::session>& sessions);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
