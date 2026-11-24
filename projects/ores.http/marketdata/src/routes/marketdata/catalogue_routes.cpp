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
#include "ores.http/routes/marketdata/catalogue_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <chrono>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::marketdata {

namespace {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace marketdata_domain = ores::marketdata::domain;
namespace marketdata_messaging = ores::marketdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// How long the catalogue stays fresh: a publisher adds a feed rarely.
constexpr std::chrono::seconds catalogue_stale_after{5 * 60};

auto& catalogue_lg() {
    static auto instance = ores::logging::make_logger("ores.http.marketdata.catalogue_routes");
    return instance;
}

/// One row of the catalogue, as the wire names its members.
template <typename Row>
std::string row_json(const Row& row) {
    return ores::utility::codec::wire_traits<Row>::write_json(row);
}

}

http_response catalogue_routes::catalogue_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(catalogue_stale_after));
}

void catalogue_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto built =
        router->get("/api/marketdata/catalogue")
            .catalogue("web_get_marketdata_catalogue", "market-data", "catalogue", false)
            .summary("The providers and the feeds that serve them")
            .description("A composed view owned by ores.marketdata: every provider the area "
                         "holds, ordered by its code, and every feed ordered by the publisher "
                         "it belongs to, its default feed first and then by name. Two reads, "
                         "and the gateway sorts them. The answer carries an ETag and stays "
                         "fresh for 5 minutes.")
            .tags({"marketdata"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_providers_request>([&] {
                    rr::refuse_unknown_params(req, {});

                    auto providers = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_providers_request{},
                        &marketdata_messaging::list_providers_response::providers);
                    std::ranges::sort(providers, {}, &marketdata_domain::provider::code);

                    auto feeds =
                        forwarding::forward_all(req,
                                                session,
                                                marketdata_messaging::list_feeds_request{},
                                                &marketdata_messaging::list_feeds_response::feeds);
                    std::ranges::sort(feeds, [](const auto& left, const auto& right) {
                        if (left.publisher_code != right.publisher_code)
                            return left.publisher_code < right.publisher_code;
                        if (left.is_default != right.is_default)
                            return right.is_default;
                        return left.name < right.name;
                    });

                    std::vector<std::string> provider_rows;
                    provider_rows.reserve(providers.size());
                    for (const auto& provider : providers)
                        provider_rows.push_back(row_json(provider));
                    std::vector<std::string> feed_rows;
                    feed_rows.reserve(feeds.size());
                    for (const auto& feed : feeds)
                        feed_rows.push_back(row_json(feed));

                    ORES_LOG_SEV(catalogue_lg(), ores::logging::debug)
                        << "The catalogue holds " << provider_rows.size() << " providers and "
                        << feed_rows.size() << " feeds";
                    return catalogue_answer(req,
                                            "{\"providers\":" + rr::array_of(provider_rows) +
                                                ",\"feeds\":" + rr::array_of(feed_rows) + "}");
                });
            })
            .build();
    router->add_route(built);
    registry->register_route(built);
}

}
