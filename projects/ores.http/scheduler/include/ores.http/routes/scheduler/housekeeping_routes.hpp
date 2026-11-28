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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_HTTP_ROUTES_SCHEDULER_HOUSEKEEPING_ROUTES_HPP
#define ORES_HTTP_ROUTES_SCHEDULER_HOUSEKEEPING_ROUTES_HPP

#include "ores.http.api/domain/http_response.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>

namespace ores::http::net {
class router;
}

namespace ores::http::openapi {
class endpoint_registry;
}

namespace ores::http::domain {
struct http_request;
}

namespace ores::http::routes::scheduler {

/**
 * @brief The housekeeping screen's reads and its one write.
 *
 * The screen states whether each store the installation keeps is bounded, what
 * bounds it, what it holds and when it last ran; opens one store's series; and
 * changes a bound. Every one of them is a scheduler protocol call, so the routes
 * forward and shape the answer rather than deciding anything: the verdict is the
 * view's, and the policy a write states is composed from the catalogue the read
 * already came from.
 */
class housekeeping_routes final {
public:
    housekeeping_routes() = delete;

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

private:
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_stores(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session);

    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_samples(const ores::http::domain::http_request& req,
                   ores::nats::service::nats_client& session);

    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_policy(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session);
};

}

#endif
