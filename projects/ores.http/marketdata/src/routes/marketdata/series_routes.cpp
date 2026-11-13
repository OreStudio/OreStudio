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
#include "ores.http/routes/marketdata/series_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.marketdata.api/messaging/market_observation_protocol.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
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

/// The unit's own helpers, named apart from the other units that build into
/// one translation unit with it.
namespace series {

/// The most observations one read returns. A request for more is clamped to it.
constexpr std::uint32_t most_observations = 1000;

/// The number of observations a read returns when it names none.
constexpr std::uint32_t default_observations = 500;

/// How far back each window reaches, in seconds. A month is thirty days and a
/// year is three hundred and sixty five, so a window is a length and not a
/// calendar period.
constexpr std::int64_t hour_window = 60 * 60;
constexpr std::int64_t day_window = 24 * 60 * 60;
constexpr std::int64_t month_window = 30 * 24 * 60 * 60;
constexpr std::int64_t year_window = 365 * 24 * 60 * 60;

/// The windows the series page offers, and the one a read takes when it names
/// none.
constexpr std::string_view all_window = "all";

/// The parameters the read names.
constexpr std::initializer_list<std::string_view> parameters{"limit", "window"};

/// The earliest observation a window reads, or nothing when it reads all of
/// them.
///
/// The instant is taken from the deployment's clock, because the observation
/// times were stamped against it and not the browser's.
std::optional<std::string> observed_since(const std::string& window) {
    if (window.empty() || window == all_window)
        return std::nullopt;
    std::int64_t seconds = 0;
    if (window == "hour")
        seconds = hour_window;
    else if (window == "day")
        seconds = day_window;
    else if (window == "month")
        seconds = month_window;
    else if (window == "year")
        seconds = year_window;
    else
        rr::invalid("A series window is hour, day, month, year or all.");
    const auto since = std::chrono::system_clock::now() - std::chrono::seconds{seconds};
    return ores::platform::time::datetime::to_iso8601_utc(since);
}

/// The count the read returns, which is the cap on the answer.
std::uint32_t limit_of(const http_request& req) {
    const auto raw = req.get_query_param("limit");
    if (rr::trimmed(raw).empty())
        return default_observations;
    const auto wanted = rr::read<std::uint32_t>(raw, "limit");
    if (wanted < 1)
        rr::invalid("A series read returns at least one observation.");
    return std::min(wanted, most_observations);
}

}

}

std::string series_routes::observations_json(
    const std::vector<ores::marketdata::domain::market_observation>& observations,
    std::uint64_t total) {
    std::vector<std::string> rows;
    for (const auto& row : observations)
        rows.push_back("{\"at\":" + json::write(row.observation_datetime) + ",\"value\":" +
                       json::write(row.value) + ",\"source\":" + json::write(row.source) + "}");
    std::ranges::reverse(rows);
    return "{\"observations\":" + rr::array_of(rows) + ",\"total\":" + std::to_string(total) + "}";
}

void series_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace series;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"market-data"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/series/{id}/observations")
            .summary("One series' observations inside a window, oldest first")
            .description("The window is turned into the earliest observation the read takes "
                         "here rather than in the browser, because the deployment's clock is the "
                         "one the observations were stamped against. The count stays the cap on "
                         "the answer, so a window wider than the cap returns its newest "
                         "observations.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<
                    marketdata_messaging::list_by_series_id_market_observations_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address does not name a series.");
                    rr::refuse_unknown_params(req, parameters);
                    marketdata_messaging::list_by_series_id_market_observations_request msg;
                    msg.series_id = boost::uuids::string_generator()(raw);
                    msg.scope = ores::utility::domain::scope::direct;
                    msg.limit = limit_of(req);
                    msg.filter.emplace();
                    msg.filter->observation_datetime_from =
                        observed_since(req.get_query_param("window"));
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    return http_response::json(
                        observations_json(reply.market_observations, reply.total));
                });
            }));
}

}
