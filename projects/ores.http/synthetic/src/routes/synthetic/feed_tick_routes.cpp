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
#include "ores.http/routes/synthetic/feed_tick_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config_codec.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_codec.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_overview_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <charconv>
#include <format>
#include <initializer_list>
#include <set>
#include <string_view>

namespace ores::http::routes::synthetic {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace forwarding = ores::http::forwarding;
namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace synthetic_messaging = ores::synthetic::messaging;

namespace {

/// The most feeds a raster draws, the ones that run first.
constexpr int raster_lanes = 120;

/// A whole number query parameter from lowest to highest, its default when left out, or a refusal.
int tick_query_number(
    const http_request& req, const std::string& name, int lowest, int highest, int fallback) {
    const auto raw = req.get_query_param(name);
    if (raw.empty())
        return fallback;
    int value = 0;
    const auto [end, error] = std::from_chars(raw.data(), raw.data() + raw.size(), value);
    if (error != std::errc{} || end != raw.data() + raw.size() || value < lowest || value > highest)
        rr::invalid("The " + name + " is a whole number from " + std::to_string(lowest) + " to " +
                    std::to_string(highest) + ".");
    return value;
}

/// Refuses a query parameter the read does not name, as the BFF's strict reads did.
void refuse_unknown(const http_request& req, std::initializer_list<std::string_view> names) {
    for (const auto& [name, value] : req.query_params)
        if (std::ranges::find(names, std::string_view(name)) == names.end())
            rr::invalid("This read cannot take " + name + ".");
}

/// The first row of a feed list that names one feed, as the session reads it.
template <typename List, auto Rows>
std::string source_name_of(const http_request& req,
                           nats_client& session,
                           const std::string& resource,
                           const std::string& id) {
    List msg;
    msg.limit = 1;
    msg.filter.emplace();
    msg.filter->id_one_of = std::vector{boost::uuids::string_generator()(id)};
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    const auto& rows = reply.*Rows;
    if (rows.empty())
        rr::refuse(
            http_status::not_found, "not-found", "There is no " + resource + " row " + id + ".");
    return rows.front().source_name;
}

/// The source names of the feeds the service runs now, demanded or manual.
std::set<std::string, std::less<>> running_sources(const http_request& req, nats_client& session) {
    const auto reply = forwarding::forward(req, session, synthetic_messaging::list_feeds_request{});
    if (!reply.success)
        rr::refuse(http_status::bad_gateway,
                   "upstream-unavailable",
                   "The running feeds could not be read.");
    std::set<std::string, std::less<>> out;
    for (const auto& feed : reply.running_feeds)
        out.insert(feed.source_name);
    return out;
}

}

std::string feed_tick_routes::instant_text(std::chrono::system_clock::time_point at) {
    return std::format("{:%FT%T}Z", std::chrono::floor<std::chrono::milliseconds>(at));
}

void feed_tick_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    register_routes(
        std::move(router),
        std::move(registry),
        session,
        std::make_shared<feed_tick_buffers>(feed_tick_buffers::nats_subscriber(session)));
}

void feed_tick_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session,
    std::shared_ptr<feed_tick_buffers> buffers) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"synthetic"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/synthetic/feeds/{id}/ticks")
            .summary("Read one feed's newest ticks, oldest first")
            .description("The feed is read as the session first, so a feed the session cannot "
                         "see has no ticks to show. The resource names the feed's kind. The "
                         "gateway starts listening to a feed at its first read, so that read "
                         "answers no ticks and the next ones answer what was heard since.")
            .handler([&session,
                      buffers](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::list_fx_spot_generation_configs_request>(
                    [&] {
                        refuse_unknown(req, {"limit", "resource"});
                        const auto limit = tick_query_number(
                            req, "limit", 1, static_cast<int>(feed_tick_buffers::capacity), 200);
                        auto resource = req.get_query_param("resource");
                        if (resource.empty())
                            resource = "fx-feeds";
                        const auto id = req.get_path_param("id");
                        if (!rr::is_uuid(id))
                            rr::invalid("A feed is named by its identifier.");

                        std::string source;
                        if (resource == "fx-feeds")
                            source = source_name_of<
                                synthetic_messaging::list_fx_spot_generation_configs_request,
                                &synthetic_messaging::list_fx_spot_generation_configs_response::
                                    fx_spot_generation_configs>(req, session, resource, id);
                        else if (resource == "ir-curve-feeds")
                            source = source_name_of<
                                synthetic_messaging::list_ir_curve_generation_configs_request,
                                &synthetic_messaging::list_ir_curve_generation_configs_response::
                                    ir_curve_generation_configs>(req, session, resource, id);
                        else
                            rr::invalid("A feed's ticks are read through fx-feeds or "
                                        "ir-curve-feeds.");

                        buffers->watch(source);
                        std::string ticks = "[";
                        for (const auto& tick :
                             buffers->ticks(source, static_cast<std::size_t>(limit))) {
                            ticks += ticks.size() == 1 ? "{" : ",{";
                            ticks += "\"at\":" + json::write(instant_text(tick.at));
                            ticks += ",\"value\":" + json::write(tick.value);
                            ticks += ",\"uri\":" + json::write(tick.uri) + "}";
                        }
                        return http_response::json("{\"sourceName\":" + json::write(source) +
                                                   ",\"ticks\":" + ticks + "]}");
                    });
            }));

    add(router->get("/api/synthetic/feed-raster")
            .summary("Read when each feed's ticks arrived in a window, the feeds that run first")
            .description("A stopped feed keeps its row with no ticks, so a reader sees that it "
                         "is silent. The read listens to the feeds that run and stops listening "
                         "to the rest.")
            .handler([&session,
                      buffers](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::list_folder_feeds_request>([&] {
                    refuse_unknown(req, {"seconds"});
                    const auto seconds = tick_query_number(req, "seconds", 1, 600, 120);

                    synthetic_messaging::list_folder_feeds_request every;
                    every.offset = 0;
                    every.limit = raster_lanes;
                    const auto page = forwarding::forward(req, session, every);
                    if (!page.success)
                        rr::refuse(http_status::bad_gateway, "upstream-unavailable", page.message);
                    const auto runs = running_sources(req, session);

                    // The feeds that run first within the page the service answers,
                    // its first 120 in label order, as the BFF read them; the
                    // service's label order holds inside each group.
                    auto rows = page.feeds;
                    std::ranges::stable_partition(
                        rows, [&](const auto& feed) { return runs.contains(feed.source_name); });
                    if (rows.size() > static_cast<std::size_t>(raster_lanes))
                        rows.resize(static_cast<std::size_t>(raster_lanes));

                    std::vector<std::string> running;
                    for (const auto& row : rows)
                        if (runs.contains(row.source_name))
                            running.push_back(row.source_name);
                    buffers->keep_only(running);

                    const auto since =
                        std::chrono::system_clock::now() - std::chrono::seconds(seconds);
                    std::string feeds = "[";
                    for (const auto& row : rows) {
                        const bool runs_now = runs.contains(row.source_name);
                        std::string times = "[";
                        if (runs_now) {
                            buffers->watch(row.source_name);
                            for (const auto& at : buffers->arrival_times(row.source_name, since))
                                times +=
                                    (times.size() == 1 ? "" : ",") + json::write(instant_text(at));
                        }
                        feeds += feeds.size() == 1 ? "{" : ",{";
                        feeds += "\"id\":" + json::write(row.id);
                        feeds += ",\"kind\":" + json::write(row.kind);
                        feeds += ",\"engine\":" + json::write(row.engine);
                        feeds += ",\"sourceName\":" + json::write(row.source_name);
                        feeds += ",\"label\":" + json::write(row.label);
                        feeds += ",\"assetClass\":" + json::write(row.asset_class_code);
                        feeds += std::string(",\"running\":") + (runs_now ? "true" : "false");
                        feeds += ",\"ticks\":" + times + "]";
                        feeds += ",\"expectedPerSecond\":" +
                                 json::write(static_cast<double>(row.ticks_per_hour) / 3600.0) +
                                 "}";
                    }
                    return http_response::json("{\"seconds\":" + std::to_string(seconds) +
                                               ",\"feeds\":" + feeds + "]}");
                });
            }));
}

}
