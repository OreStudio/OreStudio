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
#ifndef ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_WRITES_HPP
#define ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_WRITES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>

namespace ores::http::routes::reporting {

/**
 * @brief The writes of a pricing engine document: the create, the change and the
 * removal.
 *
 * A write takes the whole document, as a read answers it, because a document is
 * three tables in the analytics service and a screen never sees the split. A
 * document that omits a product or a parameter the earlier version held removes
 * it, which is how the service's own document save reads a document.
 */
class pricing_engine_writes final {
public:
    /// Register POST, PUT and DELETE on the pricing engines' collection.
    static void
    register_write_routes(std::shared_ptr<ores::http::net::router> router,
                          std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                          ores::nats::service::nats_client& session);
};

}

#endif
