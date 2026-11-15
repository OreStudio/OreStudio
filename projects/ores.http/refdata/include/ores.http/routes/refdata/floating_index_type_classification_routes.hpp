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
 * Template: cpp_http_route_web_classification.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_HTTP_ROUTES_FLOATING_INDEX_TYPE_CLASSIFICATION_ROUTES_HPP
#define ORES_HTTP_ROUTES_FLOATING_INDEX_TYPE_CLASSIFICATION_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::routes::refdata {

/**
 * @brief The routes the classification screen maintains the floating-index-type list through.
 *
 * Each route forwards the caller's own token, so the service's permission
 * check governs. A list of spellings an ORE document must write exactly is
 * read and labelled here but not written.
 */
class floating_index_type_classification_routes {
private:
    inline static std::string_view logger_name =
        "ores.http.routes.refdata.floating_index_type_classification_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the floating-index-type list's routes.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The list as the catalogue describes it, as JSON, with its row count.
     */
    static std::string catalogue_entry(std::optional<std::uint64_t> count);

    /**
     * @brief How many rows the list holds, read without reading the rows.
     */
    static std::uint64_t count(const ores::http::domain::http_request& req,
                               ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/classifications/floating-index-type — every row, each with its label.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_list(const ores::http::domain::http_request& req,
                ores::nats::service::nats_client& session);

    /**
     * @brief POST /api/classifications/floating-index-type/rows — adds a row.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_create(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session);

    /**
     * @brief PUT /api/classifications/floating-index-type/rows/{code} — corrects a row against the
     * version read.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_update(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session);

    /**
     * @brief PUT /api/classifications/floating-index-type/order — writes the display order of the
     * rows that moved.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_order(const ores::http::domain::http_request& req,
                 ores::nats::service::nats_client& session);

    /**
     * @brief DELETE /api/classifications/floating-index-type/rows/{code} — removes a row.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_delete(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session);

    /**
     * @brief PUT /api/classifications/floating-index-type/rows/{code}/label — gives a row a label,
     * or takes it away.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_label(const ores::http::domain::http_request& req,
                 ores::nats::service::nats_client& session);
};

}

#endif
