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
#ifndef ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_IMPORT_HPP
#define ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_IMPORT_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>

namespace ores::http::routes::reporting {

/**
 * @brief The standard import in its two steps: the preview, which writes
 * nothing, and the import, which acts on the same answer.
 *
 * Both are the ORE service's. The gateway hands the document over and states
 * what comes back, a refusal included, because the person reads the reason on
 * the step. It reads no XML, builds none and validates none.
 *
 * The screen names the configuration by the id it shows, and the service reads
 * a document by the reporting configuration that record fills, so the gateway
 * joins the two ids. A body that names no configuration is joined to an empty
 * one, which is how a file for a configuration that does not exist yet reads.
 */
class pricing_engine_import final {
public:
    /// Register the two import routes.
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
