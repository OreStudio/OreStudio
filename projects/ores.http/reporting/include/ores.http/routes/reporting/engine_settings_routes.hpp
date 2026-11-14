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
#ifndef ORES_HTTP_ROUTES_REPORTING_ENGINE_SETTINGS_ROUTES_HPP
#define ORES_HTTP_ROUTES_REPORTING_ENGINE_SETTINGS_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>

namespace ores::http::routes::reporting {

/**
 * @brief The engine settings a party holds: a named configuration that reports
 * bind, as they bind their pricing engines.
 *
 * The service keeps a configuration as a central row and a part for each group
 * of settings. A part that has no row means the engine default for every
 * setting in it, and a setting that is null inside a row means the engine
 * default for that setting, so a screen can always say which value is in force.
 */
class engine_settings_routes final {
public:
    /**
     * @brief Register GET /api/reporting/engine-settings/documentation.
     *
     * @param session The NATS client the route forwards on, with the caller's
     * own token, so the services' permission checks govern.
     */
    static void
    register_documentation_route(std::shared_ptr<ores::http::net::router> router,
                                 std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                 ores::nats::service::nats_client& session);
};

}

#endif
