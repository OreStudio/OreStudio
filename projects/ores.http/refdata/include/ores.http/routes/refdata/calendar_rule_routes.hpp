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
#ifndef ORES_HTTP_ROUTES_CALENDAR_RULE_ROUTES_HPP
#define ORES_HTTP_ROUTES_CALENDAR_RULE_ROUTES_HPP

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
 * @brief Every verb asset calendar_rules answer, as one HTTP route each.
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
class calendar_rule_routes {
private:
    inline static std::string_view logger_name = "ores.http.routes.refdata.calendar_rule_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the calendar_rules routes.
     *
     * @param session The client every route forwards on. The caller's token
     * is taken from each HTTP request and delegated on it, so the unit holds
     * no authorisation state of its own.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/reference-data/calendar_rules — List calendar rules.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_list(const ores::http::domain::http_request& req,
                ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/reference-data/calendar_rules/{id} — Get one calendar rule.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_get(const ores::http::domain::http_request& req,
               ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/reference-data/calendar_rules/get-many — Get many calendar rules.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_get_many(const ores::http::domain::http_request& req,
                    ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/reference-data/calendar_rules — Create one calendar rule.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_add(const ores::http::domain::http_request& req,
               ores::nats::service::nats_client& session);

    /**
     * @brief PUT /api/v1/reference-data/calendar_rules — Replace one calendar rule.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_set(const ores::http::domain::http_request& req,
               ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/reference-data/calendar_rules/put-many — Write many calendar rules.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_put_many(const ores::http::domain::http_request& req,
                    ores::nats::service::nats_client& session);

    /**
     * @brief DELETE /api/v1/reference-data/calendar_rules/{id} — Delete one calendar rule.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_delete(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/v1/reference-data/calendar_rules/delete-many — Delete many calendar rules.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_delete_many(const ores::http::domain::http_request& req,
                       ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/reference-data/calendar_rules/by-calendar-code/{calendar_code} — List the
     * calendar rules that reference one calendar code.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_by_calendar_code(const ores::http::domain::http_request& req,
                            ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/reference-data/calendar_rules/{id}/versions — List the recorded versions
     * of one calendar rule.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_versions(const ores::http::domain::http_request& req,
                    ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/v1/reference-data/calendar_rules/{id}/versions/{version} — Get one recorded
     * version of one calendar rule.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_version(const ores::http::domain::http_request& req,
                   ores::nats::service::nats_client& session);
};

}

#endif
