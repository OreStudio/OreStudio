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
#ifndef ORES_HTTP_ROUTES_WORKFLOW_PROGRESS_ROUTES_HPP
#define ORES_HTTP_ROUTES_WORKFLOW_PROGRESS_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>

namespace ores::http::routes::workflow {

/**
 * @brief The run's progress, as a journey's rail renders it.
 *
 * The view is one read of the run's steps, and the engine's answer is already
 * the shape the rail draws, so the route spells nothing: it forwards and keeps
 * the answer for the few seconds a page follows a running run.
 */
class progress_routes final {
public:
    /**
     * @brief The run's answer, kept fresh for five seconds.
     *
     * Five seconds is the interval a page asks again while it follows a run, so
     * the answer is kept for exactly as long as it is still being read.
     */
    static ores::http::domain::http_response
    progress_answer(const ores::http::domain::http_request& req, std::string body);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
