/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_HTTP_SERVER_ROUTES_ASSETS_ROUTES_HPP
#define ORES_HTTP_SERVER_ROUTES_ASSETS_ROUTES_HPP

#include <memory>
#include "ores.http/net/router.hpp"
#include "ores.http/openapi/endpoint_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::http_server::routes {

/**
 * @brief Registers Assets HTTP endpoints.
 *
 * Maps the following protocol messages to REST endpoints:
 *
 * Asset Management:
 * - POST /api/v1/assets/images - get_images_request (batch retrieval)
 */
class assets_routes final {
public:
    assets_routes(database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions);

    /**
     * @brief Registers all Assets routes with the router.
     */
    void register_routes(std::shared_ptr<http::net::router> router,
        std::shared_ptr<http::openapi::endpoint_registry> registry);

private:
    inline static std::string_view logger_name = "ores.http.server.routes.assets_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    boost::asio::awaitable<http::domain::http_response>
    handle_get_images(const http::domain::http_request& req);

    database::context ctx_;
    std::shared_ptr<comms::service::auth_session_service> sessions_;
};

}

#endif
