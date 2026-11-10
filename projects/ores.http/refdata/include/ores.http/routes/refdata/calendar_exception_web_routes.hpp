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
 * Template: cpp_http_route_web_records.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_HTTP_ROUTES_CALENDAR_EXCEPTION_WEB_ROUTES_HPP
#define ORES_HTTP_ROUTES_CALENDAR_EXCEPTION_WEB_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>
#include <string_view>

namespace ores::http::routes::refdata {

/**
 * @brief The record routes the screens read and write calendar-exceptions through.
 *
 * The screens name the resource by its key in the address and read it in the
 * shapes the BFF answered with: rows and a total on a page, every row on a
 * whole read, one row on a key read, and no body on a write. Each route
 * forwards the caller's own token, so the service's permission check governs.
 */
class calendar_exception_web_routes {
private:
    inline static std::string_view logger_name =
        "ores.http.routes.refdata.calendar_exception_web_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the calendar-exceptions record routes.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The resource as the catalogue of resources describes it, as JSON.
     */
    static std::string_view catalogue_entry();

    /**
     * @brief GET /api/refdata/calendar-exceptions — one page with its total, or every row.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_list(const ores::http::domain::http_request& req,
                ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/refdata/calendar-exceptions/key/{key} — one row, named by its key.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_key(const ores::http::domain::http_request& req,
               ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/refdata/calendar-exceptions/by/{parent} — every row of one parent.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_by(const ores::http::domain::http_request& req,
              ores::nats::service::nats_client& session);

    /**
     * @brief PUT /api/refdata/calendar-exceptions — writes one row.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_put(const ores::http::domain::http_request& req,
               ores::nats::service::nats_client& session);

    /**
     * @brief DELETE /api/refdata/calendar-exceptions — removes one row, named by its key fields.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_delete(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session);
};

}

#endif
