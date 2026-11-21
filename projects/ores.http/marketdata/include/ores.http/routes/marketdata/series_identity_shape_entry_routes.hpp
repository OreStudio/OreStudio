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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_http_route_header.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_HTTP_ROUTES_SERIES_IDENTITY_SHAPE_ENTRY_ROUTES_HPP
#define ORES_HTTP_ROUTES_SERIES_IDENTITY_SHAPE_ENTRY_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>
#include <string_view>

namespace ores::http::routes::marketdata {

/**
 * @brief Every verb asset series_identity_shape_entries answer, as one HTTP route each.
 *
 * The unit is the entity's own derivation addressed over HTTP, so a verb the
 * model gains appears as a route without an edit here and a verb it loses
 * takes its route with it. What each route carries follows from its shape: a
 * paged read is addressed by its query, a write by its canonical request
 * body, and everything else by the key in its path.
 *
 * The unit authorises nobody. The service's generated handler owns the
 * entity's permission, and the caller's own token travels with every request
 * this unit forwards.
 */
class series_identity_shape_entry_routes {
private:
    inline static std::string_view logger_name =
        "ores.http.routes.marketdata.series_identity_shape_entry_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the series_identity_shape_entries routes.
     *
     * @param session The client every route forwards on. The caller's token
     * is taken from each HTTP request and delegated on it, so the unit holds
     * no authorisation state of its own.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/market-data/series_identity_shape_entries — List series identity shape
     * entries.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_list(const ores::http::domain::http_request& req,
                ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/market-data/series_identity_shape_entries/{id} — Get one series identity
     * shape entry.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_get(const ores::http::domain::http_request& req,
               ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/market-data/series_identity_shape_entries/get-many — Get many series
     * identity shape entries.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_get_many(const ores::http::domain::http_request& req,
                    ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/market-data/series_identity_shape_entries — Create one series identity
     * shape entry.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_add(const ores::http::domain::http_request& req,
               ores::nats::service::nats_client& session);

    /**
     * @brief PUT /api/v1/market-data/series_identity_shape_entries — Replace one series identity
     * shape entry.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_set(const ores::http::domain::http_request& req,
               ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/market-data/series_identity_shape_entries/put-many — Write many series
     * identity shape entries.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_put_many(const ores::http::domain::http_request& req,
                    ores::nats::service::nats_client& session);

    /**
     * @brief DELETE /api/v1/market-data/series_identity_shape_entries/{id} — Delete one series
     * identity shape entry.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_delete(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/market-data/series_identity_shape_entries/delete-many — Delete many
     * series identity shape entries.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_delete_many(const ores::http::domain::http_request& req,
                       ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/market-data/series_identity_shape_entries/by-shape-code/{shape_code} —
     * List the series identity shape entries that reference one shape code.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_by_shape_code(const ores::http::domain::http_request& req,
                         ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/market-data/series_identity_shape_entries/{id}/versions — List the
     * recorded versions of one series identity shape entry.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_versions(const ores::http::domain::http_request& req,
                    ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/market-data/series_identity_shape_entries/{id}/versions/{version} — Get
     * one recorded version of one series identity shape entry.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_version(const ores::http::domain::http_request& req,
                   ores::nats::service::nats_client& session);
};

}

#endif
