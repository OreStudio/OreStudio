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
#ifndef ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_ROUTES_HPP
#define ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>

namespace ores::http::routes::reporting {

/**
 * @brief The pricing engine configurations a party holds, read as one document.
 *
 * The analytics service keeps a configuration as three tables: the header, one
 * row for each product and one row for each parameter. The screens never see the
 * split, so a read answers the whole document and a write takes it whole.
 */
class pricing_engine_routes final {
public:
    /**
     * @brief Register the two reads: the page of engines, and one document with
     * the reports that bind it.
     */
    static void
    register_read_routes(std::shared_ptr<ores::http::net::router> router,
                         std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                         ores::nats::service::nats_client& session);
};

}

#endif
