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
#ifndef ORES_HTTP_ROUTES_SYNTHETIC_CALIBRATION_ROUTES_HPP
#define ORES_HTTP_ROUTES_SYNTHETIC_CALIBRATION_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>

namespace ores::http::routes::synthetic {

/**
 * @brief The calibration screen's actions: run a class's calibration, review a
 * proposed variance calibration, state or remove a class's policy, and pin or
 * clear one feed's own candidate.
 *
 * A policy and a review belong to the installation, so only the system tenant
 * makes them. A feed belongs to its tenant, so any tenant pins its own feeds.
 */
class calibration_routes {
public:
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
