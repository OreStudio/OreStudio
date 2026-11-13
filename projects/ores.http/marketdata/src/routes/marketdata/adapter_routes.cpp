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
#include "ores.http/routes/marketdata/adapter_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.api/messaging/session_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <map>
#include <string>

namespace ores::http::routes::marketdata {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace marketdata_messaging = ores::marketdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

}

std::string
adapter_routes::adapters_json(const std::vector<marketdata_messaging::adapter_status>& adapters,
                              const std::vector<ores::marketdata::domain::feed>& feeds,
                              const std::vector<ores::marketdata::domain::session>& sessions) {
    std::map<std::string, std::string> session_of_feed;
    for (const auto& feed : feeds)
        session_of_feed[feed.name] = feed.session_code;
    std::map<std::string, std::string> environment_of_session;
    for (const auto& session : sessions)
        environment_of_session[session.code] = session.environment;

    std::vector<std::string> rows;
    for (const auto& adapter : adapters) {
        std::string kind;
        const auto feed = session_of_feed.find(adapter.adapter_id);
        if (feed != session_of_feed.end()) {
            const auto session = environment_of_session.find(feed->second);
            if (session != environment_of_session.end())
                kind = session->second;
        }
        rows.push_back(
            rr::object_with_members(json::write(adapter), {{"kind", json::write(kind)}}));
    }
    /* The answer keeps the BFF's success and message beside the rows, so a
     * screen that reads either keeps working. */
    return "{\"adapters\":" + rr::array_of(rows) + ",\"success\":true,\"message\":\"\"}";
}

void adapter_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"market-data"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/marketdata/adapters")
            .summary("Every adapter the service tracks, with the kind of source it reads")
            .description("The status names one adapter per feed and says nothing about the "
                         "source. The catalogue does: a feed names its session, and the session's "
                         "environment says whether the values are simulated or come from a "
                         "supplier. A source the catalogue no longer holds states none.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_adapter_status_request>([&] {
                    const auto reply = forwarding::forward(
                        req, session, marketdata_messaging::list_adapter_status_request{});
                    if (!reply.success)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   reply.message.empty() ?
                                       "The market data service refused the adapters." :
                                       reply.message);
                    const auto feeds =
                        forwarding::forward_all(req,
                                                session,
                                                marketdata_messaging::list_feeds_request{},
                                                &marketdata_messaging::list_feeds_response::feeds);
                    const auto sessions = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_sessions_request{},
                        &marketdata_messaging::list_sessions_response::sessions);
                    return http_response::json(adapters_json(reply.adapters, feeds, sessions));
                });
            }));
}

}
