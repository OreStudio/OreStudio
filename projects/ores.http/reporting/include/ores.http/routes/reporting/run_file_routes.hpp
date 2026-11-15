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
#ifndef ORES_HTTP_ROUTES_REPORTING_RUN_FILE_ROUTES_HPP
#define ORES_HTTP_ROUTES_REPORTING_RUN_FILE_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>

namespace ores::http::routes::reporting {

/**
 * @brief The files of one report run: what the engine read, what it wrote, and
 * the bytes of one of them.
 *
 * The tree is composed and not owned: the run is the reporting service's, the
 * batch, the workunits and the results are compute's, and what sits inside a
 * package is storage's. This unit states each link once and holds no model.
 */
class run_file_routes final {
public:
    /**
     * @param storage_dir The root the file service serves objects from, which
     * this unit reads a run's manifests out of.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session,
                                std::string storage_dir);
};

}

#endif
