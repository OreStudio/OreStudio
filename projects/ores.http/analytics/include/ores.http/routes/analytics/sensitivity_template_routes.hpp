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
#ifndef ORES_HTTP_ROUTES_ANALYTICS_SENSITIVITY_TEMPLATE_ROUTES_HPP
#define ORES_HTTP_ROUTES_ANALYTICS_SENSITIVITY_TEMPLATE_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>

namespace ores::http::routes::analytics {

/**
 * @brief The keys a pricing engine configuration puts on a product with the
 * SensitivityTemplate setting, and nothing more.
 *
 * The shift sizes a key implies belong to the sensitivity configuration, so this
 * screen manages the keys. A key names a method and, for the methods that need
 * one, the asset class of the product it belongs to; both are read from their
 * own tables rather than stated by this part, so a method or a class added to a
 * table reaches the form without a change here.
 *
 * A create states a key the table does not hold and a correction states the
 * version it read, so a key written meanwhile is refused rather than replaced.
 * A key's versions stay when it is removed, so its history outlives it.
 */
class sensitivity_template_routes final {
public:
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
