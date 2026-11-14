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
#ifndef ORES_HTTP_ROUTES_REPORTING_REPORTING_ROUTES_HPP
#define ORES_HTTP_ROUTES_REPORTING_REPORTING_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>

namespace ores::http::routes::reporting {

/**
 * @brief Registers every reporting route unit.
 *
 * This aggregate is the one list the host calls, and no facet emits it.
 */
class reporting_routes {
public:
    /**
     * @brief Register every reporting route unit on the router and registry.
     *
     * @param session The NATS client each unit forwards on, with the caller's
     * own token, so the services' permission checks govern.
     * @param storage_dir The root the file service serves objects from, which
     * the run's files are read out of.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session,
                                std::string storage_dir);
};

}

#endif
