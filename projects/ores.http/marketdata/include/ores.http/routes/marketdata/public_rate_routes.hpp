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
#ifndef ORES_HTTP_ROUTES_MARKETDATA_PUBLIC_RATE_ROUTES_HPP
#define ORES_HTTP_ROUTES_MARKETDATA_PUBLIC_RATE_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/uuid/uuid.hpp>
#include <memory>
#include <string>
#include <vector>

namespace ores::http::routes::marketdata {

/**
 * @brief The public rates: the fixings the store holds, and the terms a person
 * accepts before a public publisher may be read.
 *
 * The screens that weave the publishers and their series together are their own
 * routes. This unit reads the fixings a screen asks for, by series, and writes
 * the three answers a person gives to a publisher's terms.
 */
class public_rate_routes {
public:
    /**
     * @brief The fixings of each series asked for, oldest first, keyed by the
     * series id.
     *
     * Every series asked for is stated, with an empty list when it holds no
     * fixing in the store, so a screen reads a series' answer without testing
     * the key.
     */
    static std::string
    fixings_json(const std::vector<boost::uuids::uuid>& series,
                 const std::vector<ores::marketdata::domain::market_fixing>& fixings);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
