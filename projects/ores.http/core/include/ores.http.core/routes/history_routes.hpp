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
#ifndef ORES_HTTP_CORE_ROUTES_HISTORY_ROUTES_HPP
#define ORES_HTTP_CORE_ROUTES_HISTORY_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http.core/export.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>
#include <span>
#include <string_view>

namespace ores::http::routes {

/**
 * @brief The one history read every screen's history tab calls.
 *
 * A screen names a record by its entity type and its key. The component that
 * owns the type answers on its own history subject, so the route derives the
 * subject from the type, as every NATS caller does. Only the types a screen
 * reads history for are served; any other is a 404, as it was in the BFF.
 */
class ORES_HTTP_CORE_EXPORT history_routes final {
public:
    /**
     * @brief Register GET /api/history.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The entity types a screen reads history for, sorted.
     */
    [[nodiscard]] static std::span<const std::string_view> served_types();

    /**
     * @brief Whether a screen reads history for this entity type.
     */
    [[nodiscard]] static bool serves(std::string_view entity_type);

    /**
     * @brief GET /api/history — every version of one record, newest first.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_history(const ores::http::domain::http_request& req,
                   ores::nats::service::nats_client& session);
};

}

#endif
