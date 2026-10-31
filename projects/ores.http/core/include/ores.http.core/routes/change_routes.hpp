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
#ifndef ORES_HTTP_CORE_ROUTES_CHANGE_ROUTES_HPP
#define ORES_HTTP_CORE_ROUTES_CHANGE_ROUTES_HPP

#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http.core/export.hpp"
#include "ores.http.core/stream/change_hub.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/asio/awaitable.hpp>
#include <chrono>
#include <memory>

namespace ores::http::server::routes {

/**
 * @brief Registers the change stream.
 *
 * Maps one endpoint:
 *
 * Changes:
 * - GET /api/v1/changes - a stream of change frames for the caller's session
 */
class ORES_HTTP_CORE_EXPORT change_routes final {
public:
    change_routes(std::shared_ptr<stream::change_hub> hub,
                  std::chrono::milliseconds keep_alive);

    /**
     * @brief Registers the change routes with the router.
     */
    void register_routes(std::shared_ptr<http::net::router> router,
                         std::shared_ptr<http::openapi::endpoint_registry> registry);

private:
    inline static std::string_view logger_name = "ores.http.server.routes.change_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    boost::asio::awaitable<domain::http_response>
    handle_changes(const domain::http_request& req);

    std::shared_ptr<stream::change_hub> hub_;
    std::chrono::milliseconds keep_alive_;
};

}

#endif
