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
#ifndef ORES_HTTP_CORE_ROUTES_SITE_ROUTES_HPP
#define ORES_HTTP_CORE_ROUTES_SITE_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http.core/export.hpp"
#include "ores.http.core/site/site_configuration.hpp"
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes {

/**
 * @brief The routes the served client reads about the server itself.
 *
 * The liveness answer, what the client needs to render itself, the
 * deployment's plumbing for the developer page, and the trail the browser
 * keeps. None of them is a resource: each answers from the server's own state.
 */
class ORES_HTTP_CORE_EXPORT site_routes final {
public:
    /**
     * @brief One line of the browser's trail, as it arrives.
     */
    struct client_entry final {
        std::string at;
        std::string level;
        std::string logger;
        std::string message;
        /// Each fact as the text the journal writes.
        std::vector<std::pair<std::string, std::string>> fields;
    };

    /**
     * @brief Register the routes. Without a site, only the liveness answer and
     * the client's trail are served.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                std::optional<ores::http::site::loaded_site> site);

    /**
     * @brief GET /api/site — what the client needs to render itself.
     */
    [[nodiscard]] static ores::http::domain::http_response
    site_state(const ores::http::site::loaded_site& site);

    /**
     * @brief GET /api/site/deployment — the deployment's plumbing, only where the
     * developer surface is offered.
     */
    [[nodiscard]] static ores::http::domain::http_response
    deployment(const ores::http::site::loaded_site& site);

    /**
     * @brief The trail a POST /api/client-log body states, or nothing when it is
     * not one the browser may send.
     */
    [[nodiscard]] static std::optional<std::vector<client_entry>>
    read_client_log(const std::string& body);

    /**
     * @brief POST /api/client-log — writes each line into the journal under the
     * session's id.
     */
    [[nodiscard]] static ores::http::domain::http_response
    handle_client_log(const ores::http::domain::http_request& req);
};

}

#endif
