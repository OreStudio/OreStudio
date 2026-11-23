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
#ifndef ORES_HTTP_ROUTES_TRADING_PRODUCT_ROUTES_HPP
#define ORES_HTTP_ROUTES_TRADING_PRODUCT_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.trading.api/messaging/product_schema_operation_protocol.hpp"
#include <memory>
#include <string>

namespace ores::http::routes::trading {

/**
 * @brief The products a screen can book, and the description of one: the
 * fields of its element and of every schema type they reach, the inputs and
 * codes of the scripts its forms run, and the forms.
 *
 * The answers name each field, in the shape the BFF answers the same paths
 * with, so a screen reads one shape from either server.
 */
class product_routes {
public:
    /**
     * @brief The list a screen reads: each product's code, name and construction.
     */
    static std::string
    list_json(const ores::trading::messaging::list_products_response& response);

    /**
     * @brief The description a screen reads.
     */
    static std::string
    describe_json(const ores::trading::messaging::describe_product_response& response);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
