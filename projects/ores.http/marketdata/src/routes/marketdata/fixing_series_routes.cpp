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
#include "ores.http/routes/marketdata/fixing_series_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.api/messaging/feed_scope_protocol.hpp"
#include "ores.marketdata.api/messaging/market_fixing_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_protocol.hpp"
#include "ores.marketdata.api/messaging/session_protocol.hpp"
#include "ores.marketdata.api/messaging/supplier_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::marketdata {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace marketdata_domain = ores::marketdata::domain;
namespace marketdata_messaging = ores::marketdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The sentence the read states when its address names no item.
constexpr std::string_view fixing_series_refused = "An item's fixings are named by its URI.";

/// How long an item's series stays fresh: its fixings arrive daily, not by the second.
constexpr std::chrono::seconds fixing_series_stale_after{5 * 60};

auto& fixing_series_lg() {
    static auto instance = ores::logging::make_logger("ores.http.marketdata.fixing_series_routes");
    return instance;
}

/// A date as the wire writes it.
std::string date_text(const std::chrono::year_month_day& date) {
    return ores::utility::codec::text_form<std::chrono::year_month_day>::to_text(date);
}

/// One fixing as the screen reads it: its date and its value, as the store wrote it.
std::string fixing_json(const marketdata_domain::market_fixing& fixing) {
    return "{\"date\":" + json::write(date_text(fixing.fixing_date)) +
           ",\"value\":" + json::write(fixing.value) + "}";
}

}

http_response fixing_series_routes::fixing_series_answer(const http_request& req,
                                                         std::string body) {
    return ores::http::net::view_answer(
        req,
        std::move(body),
        ores::http::net::view_cache_policy::fresh_for(fixing_series_stale_after));
}

void fixing_series_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto built =
        router->get("/api/marketdata/fixing-series")
            .catalogue("web_get_marketdata_fixing_series", "market-data", "fixing_series", false)
            .summary("One item's fixing series, with the publisher and feeds behind it")
            .description("A composed view owned by ores.marketdata: the series the oresmd URI "
                         "names, its fixings oldest first, the publisher that fills it, and the "
                         "feeds that carry it with the supplier behind each session. A URI the "
                         "store holds no series for answers a null series rather than an empty "
                         "one, so a screen says the item is unknown instead of the item being "
                         "quiet. The answer carries an ETag and stays fresh for 5 minutes.")
            .tags({"marketdata"})
            .auth_required()
            .query_param("uri", "string", "", true, "The item, as an oresmd URI")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_market_series_request>([&] {
                    rr::refuse_unknown_params(req, {"uri"});
                    const auto uri = req.get_query_param("uri");
                    if (uri.empty())
                        rr::invalid(std::string(fixing_series_refused));

                    const auto series_rows = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_market_series_request{},
                        &marketdata_messaging::list_market_series_response::market_series);
                    const auto found = std::ranges::find(
                        series_rows, uri, &marketdata_domain::market_series::oresmd_uri);
                    if (found == series_rows.end())
                        return fixing_series_answer(req, "{\"series\":null}");

                    const auto& series = *found;
                    const auto series_key = boost::uuids::to_string(series.id);

                    marketdata_messaging::list_market_fixings_request fixings_read;
                    fixings_read.filter.emplace();
                    fixings_read.filter->series_id_one_of =
                        std::vector<boost::uuids::uuid>{series.id};
                    auto fixings = forwarding::forward_all(
                        req,
                        session,
                        fixings_read,
                        &marketdata_messaging::list_market_fixings_response::market_fixings);
                    std::ranges::sort(fixings, {}, &marketdata_domain::market_fixing::fixing_date);

                    const auto publishers = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_providers_request{},
                        &marketdata_messaging::list_providers_response::providers);
                    const auto feed_rows =
                        forwarding::forward_all(req,
                                                session,
                                                marketdata_messaging::list_feeds_request{},
                                                &marketdata_messaging::list_feeds_response::feeds);
                    const auto scopes = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_feed_scopes_request{},
                        &marketdata_messaging::list_feed_scopes_response::scopes);
                    const auto sessions = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_sessions_request{},
                        &marketdata_messaging::list_sessions_response::sessions);
                    const auto suppliers = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_suppliers_request{},
                        &marketdata_messaging::list_suppliers_response::suppliers);

                    std::set<std::string> carried_by;
                    for (const auto& scope : scopes)
                        if (scope.item == uri)
                            carried_by.insert(boost::uuids::to_string(scope.feed_id));

                    std::map<std::string, const marketdata_domain::session*> session_of;
                    for (const auto& one : sessions)
                        session_of.emplace(one.code, &one);
                    std::map<std::string, const marketdata_domain::supplier*> supplier_of;
                    for (const auto& one : suppliers)
                        supplier_of.emplace(one.code, &one);

                    std::vector<std::string> feed_rows_written;
                    for (const auto& feed : feed_rows) {
                        if (!carried_by.contains(boost::uuids::to_string(feed.id)))
                            continue;
                        std::string environment;
                        std::string supplier_name;
                        const auto session_found = session_of.find(feed.session_code);
                        if (session_found != session_of.end()) {
                            environment = session_found->second->environment;
                            const auto supplier_found =
                                supplier_of.find(session_found->second->supplier_code);
                            if (supplier_found != supplier_of.end())
                                supplier_name = supplier_found->second->name;
                        }
                        feed_rows_written.push_back(
                            "{\"id\":" + json::write(boost::uuids::to_string(feed.id)) +
                            ",\"name\":" + json::write(feed.name) +
                            ",\"service\":" + json::write(feed.service) +
                            ",\"session_code\":" + json::write(feed.session_code) +
                            ",\"environment\":" + json::write(environment) +
                            ",\"supplier_name\":" + json::write(supplier_name) + "}");
                    }

                    std::optional<std::string> publisher_code;
                    std::optional<std::string> publisher_name;
                    const auto publisher = std::ranges::find(
                        publishers, series.provider_code, &marketdata_domain::provider::code);
                    if (publisher != publishers.end()) {
                        publisher_code = publisher->code;
                        publisher_name = publisher->name;
                    }

                    std::vector<std::string> values;
                    values.reserve(fixings.size());
                    for (const auto& fixing : fixings)
                        values.push_back(fixing_json(fixing));
                    const auto first =
                        fixings.empty() ? std::string{} : date_text(fixings.front().fixing_date);
                    const auto last =
                        fixings.empty() ? std::string{} : date_text(fixings.back().fixing_date);

                    ORES_LOG_SEV(fixing_series_lg(), ores::logging::debug)
                        << "The item's series " << series_key << " holds " << values.size()
                        << " fixings and " << feed_rows_written.size() << " feeds";

                    std::string publisher_json("null");
                    if (publisher_code)
                        publisher_json = "{\"code\":" + json::write(*publisher_code) +
                                         ",\"name\":" + json::write(*publisher_name) + "}";

                    return fixing_series_answer(
                        req,
                        "{\"series\":{\"id\":" + json::write(series_key) +
                            ",\"oresmd_uri\":" + json::write(series.oresmd_uri) +
                            ",\"provider_code\":" + json::write(series.provider_code) +
                            ",\"version\":" + std::to_string(series.version) + "}" +
                            ",\"publisher\":" + publisher_json + ",\"values\":{\"first\":" +
                            json::write(first) + ",\"last\":" + json::write(last) + ",\"count\":" +
                            std::to_string(values.size()) + ",\"rows\":" + rr::array_of(values) +
                            "}" + ",\"feeds\":" + rr::array_of(feed_rows_written) + "}");
                });
            })
            .build();
    router->add_route(built);
    registry->register_route(built);
}

}
