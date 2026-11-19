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
#ifndef ORES_HTTP_ROUTES_MARKETDATA_QUOTE_ROUTES_HPP
#define ORES_HTTP_ROUTES_MARKETDATA_QUOTE_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/marketdata/quote_board.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <functional>
#include <memory>
#include <vector>

namespace ores::http::routes::marketdata {

/**
 * @brief The quotes view's routes: the feeds the caller may watch, the board's quotes of
 * those feeds, and one provider's series over a span.
 *
 * The broker checks no subject, so every read asks the market data service which feeds
 * the caller may watch for display, and the board answers those alone.
 */
class quote_routes {
public:
    /// The feeds the caller of a request may watch for display.
    using entitled_feeds = std::function<std::vector<ores::marketdata::messaging::watchable_feed>(
        const ores::http::domain::http_request&)>;
    /// The ticks of one provider's series, read as the caller of a request.
    using provider_history = std::function<ores::marketdata::messaging::get_series_ticks_response(
        const ores::http::domain::http_request&,
        const ores::marketdata::messaging::get_provider_ticks_request&)>;

    /// The most quotes one read of the board returns.
    static constexpr std::size_t most_quotes = 20'000;

    /**
     * @brief Registers the routes on a board that listens through the client's transport,
     * asking the market data service as the caller.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /// Registers the routes on the board and the answers given, which a test states itself.
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                std::shared_ptr<quote_board> board,
                                entitled_feeds entitled,
                                provider_history history);
};

}

#endif
