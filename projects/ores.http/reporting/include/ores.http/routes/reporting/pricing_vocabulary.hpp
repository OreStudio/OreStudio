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
#ifndef ORES_HTTP_ROUTES_REPORTING_PRICING_VOCABULARY_HPP
#define ORES_HTTP_ROUTES_REPORTING_PRICING_VOCABULARY_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>

namespace ores::http::routes::reporting {

/**
 * @brief The lookups a pricing engine editor draws its controls from.
 *
 * A form that offers a product type, a model and its engines, or the setting a
 * pairing carries, reads them all at once here, keyed by the names its controls
 * use. Every entry carries what the ORE documentation says about it and the
 * value set that states which values it accepts, because a form that offers a
 * value the tables refuse is worse than a form that offers none.
 *
 * The inventory states this route as one that is not an API: each collection it
 * answers is read at its own resource by a client that wants one of them. It is
 * served because the editor calls it, and the editor breaks without it.
 */
class pricing_vocabulary final {
public:
    /// Register GET /api/reporting/pricing-engines/vocabulary.
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
