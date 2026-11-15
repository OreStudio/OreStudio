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
#include "ores.http/routes/marketdata/marketdata_routes.hpp"
#include "ores.http/routes/marketdata/adapter_routes.hpp"
#include "ores.http/routes/marketdata/curve_routes.hpp"
#include "ores.http/routes/marketdata/oresmd_routes.hpp"
#include "ores.http/routes/marketdata/public_rate_routes.hpp"
#include "ores.http/routes/marketdata/series_routes.hpp"
#include "ores.http/routes/marketdata/subscription_routes.hpp"

namespace ores::http::routes::marketdata {

void marketdata_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {
    adapter_routes::register_routes(router, registry, session);
    curve_routes::register_routes(router, registry, session);
    oresmd_routes::register_routes(router, registry, session);
    series_routes::register_routes(router, registry, session);
    public_rate_routes::register_routes(router, registry, session);
    subscription_routes::register_routes(router, registry, session);
}

}
