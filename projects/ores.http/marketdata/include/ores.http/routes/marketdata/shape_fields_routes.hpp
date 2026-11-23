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
#ifndef ORES_HTTP_ROUTES_MARKETDATA_SHAPE_FIELDS_ROUTES_HPP
#define ORES_HTTP_ROUTES_MARKETDATA_SHAPE_FIELDS_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>

namespace ores::http::routes::marketdata {

/**
 * @brief What the oresmd editor reads to mark a URI's fields: each field a
 * family requires or forbids, and the rules that name a URI's family.
 *
 * The shapes and their entries are records of their own, served by their own
 * units. These two routes read them joined, so the editor never holds a code.
 */
class shape_fields_routes final {
public:
    /// The fields' answer, asked about at each read.
    static ores::http::domain::http_response
    fields_answer(const ores::http::domain::http_request& req, std::string body);

    /// The rules' answer, asked about at each read.
    static ores::http::domain::http_response
    rules_answer(const ores::http::domain::http_request& req, std::string body);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
