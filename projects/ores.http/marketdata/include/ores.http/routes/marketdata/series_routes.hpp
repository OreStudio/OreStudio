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
#ifndef ORES_HTTP_ROUTES_MARKETDATA_SERIES_ROUTES_HPP
#define ORES_HTTP_ROUTES_MARKETDATA_SERIES_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>
#include <vector>

namespace ores::http::routes::marketdata {

/**
 * @brief The values of one series over time, for the series page: the
 * observations of the window asked for, oldest first.
 */
class series_routes {
public:
    /**
     * @brief The observations as the page reads them, oldest first, and how many
     * the series holds.
     *
     * The service answers newest first and the chart reads left to right in
     * time, so the answer states the rows the other way round.
     */
    static std::string
    observations_json(const std::vector<ores::marketdata::domain::market_observation>& observations,
                      std::uint64_t total);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
