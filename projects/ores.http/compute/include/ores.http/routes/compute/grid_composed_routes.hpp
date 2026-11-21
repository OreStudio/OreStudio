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
#ifndef ORES_HTTP_ROUTES_COMPUTE_GRID_COMPOSED_ROUTES_HPP
#define ORES_HTTP_ROUTES_COMPUTE_GRID_COMPOSED_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>

namespace ores::http::routes::compute {

/**
 * @brief The grid as the screens read it: its summary, its series and who uses
 * it, each answered in one call and joined from the reads the data lives in.
 */
class grid_composed_routes final {
public:
    /// The grid summary's answer, kept fresh for ten seconds.
    static ores::http::domain::http_response
    summary_answer(const ores::http::domain::http_request& req, std::string body);

    /// The grid series' answer, kept fresh for ten seconds.
    static ores::http::domain::http_response
    series_answer(const ores::http::domain::http_request& req, std::string body);

    /// The grid usage's answer, kept fresh for thirty seconds.
    static ores::http::domain::http_response
    usage_answer(const ores::http::domain::http_request& req, std::string body);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
