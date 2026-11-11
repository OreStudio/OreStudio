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
#ifndef ORES_HTTP_ROUTES_SYNTHETIC_FEED_KEY_ROUTES_HPP
#define ORES_HTTP_ROUTES_SYNTHETIC_FEED_KEY_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>
#include <string_view>

namespace ores::http::routes::synthetic {

/**
 * @brief The key reads of the two feed resources the synthetic screens open.
 *
 * A feed's own record names no provider and does not say whether it runs, so
 * the screen's read of one feed is a composed view: the feed's row, the
 * provider of the collection it sits under, whether the service runs it now,
 * and where its run started. The generated record units read every other
 * resource by key; these two are written here.
 */
class feed_key_routes {
private:
    inline static std::string_view logger_name = "ores.http.routes.synthetic.feed_key_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the key reads of the FX and the IR curve feeds.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/synthetic/fx-feeds/key/{key} — one FX feed, composed.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_fx_feed(const ores::http::domain::http_request& req,
                   ores::nats::service::nats_client& session);

    /**
     * @brief GET /api/synthetic/ir-curve-feeds/key/{key} — one IR curve feed, composed.
     */
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_ir_curve_feed(const ores::http::domain::http_request& req,
                         ores::nats::service::nats_client& session);
};

}

#endif
