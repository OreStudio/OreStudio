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
#ifndef ORES_HTTP_ROUTES_TRADING_TRADING_VIEW_ROUTES_HPP
#define ORES_HTTP_ROUTES_TRADING_TRADING_VIEW_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>

namespace ores::http::routes::trading {

/**
 * @brief The trading area's composed views: the blotter and the book tree.
 *
 * A composed view owes four things, and the owner is why it exists: its own
 * name, the schema its screen reads, the component that answers it, and the
 * computation with the cache policy it is kept under. Both views are answered
 * by ores.trading from the collections it and the reference data own, and each
 * is kept by the caller alone, because each caller's session decides what it
 * holds.
 */
class trading_view_routes {
public:
    /**
     * @brief The book tree's answer: every portfolio and every book, kept fresh
     * for a minute.
     */
    static ores::http::domain::http_response
    tree_answer(const ores::http::domain::http_request& req, std::string body);

    /**
     * @brief The blotter's answer, kept fresh for half a minute.
     *
     * The rail's figures come from every row a filter matches, so the view is
     * kept by the caller and read again when it goes stale.
     */
    static ores::http::domain::http_response
    blotter_answer(const ores::http::domain::http_request& req, std::string body);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
