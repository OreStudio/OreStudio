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
#include "ores.http/routes/synthetic/feed_operation_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_overview_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_selection_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <cctype>
#include <charconv>
#include <chrono>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace ores::http::routes::synthetic {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace forwarding = ores::http::forwarding;
namespace json = ores::utility::codec::json;
namespace marketdata_messaging = ores::marketdata::messaging;
namespace rr = ores::http::net::record_routes;
namespace synthetic_messaging = ores::synthetic::messaging;

namespace {

/// The most feeds a selection may pick or leave out.
constexpr std::size_t feed_selection_most_ids = 2000;

/// The bodies the screen sends, beside the wire_fields each is read with.
namespace feed_operation {

/**
 * The feeds to start or stop: the picked ones, or the filter's less the left
 * out. Every member may be left out.
 */
struct selection_body final {
    std::optional<std::string> folder;
    std::optional<std::string> provider;
    std::optional<std::string> assetClass;
    std::optional<std::string> state;
    std::optional<bool> shouldRun;
    std::optional<std::string> search;
    std::optional<std::vector<std::string>> feedIds;
    std::optional<std::vector<std::string>> excludedIds;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<selection_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"folder", &selection_body::folder},
                                              field{"provider", &selection_body::provider},
                                              field{"assetClass", &selection_body::assetClass},
                                              field{"state", &selection_body::state},
                                              field{"shouldRun", &selection_body::shouldRun},
                                              field{"search", &selection_body::search},
                                              field{"feedIds", &selection_body::feedIds},
                                              field{"excludedIds", &selection_body::excludedIds}};
}

}

/// What narrows the feed table, each member checked as the BFF checked it.
struct feed_filter final {
    std::string folder;
    std::string provider;
    std::string asset_class;
    std::string state;
    bool should_run = false;
    std::string search;
};

const std::string filter_sentence =
    "A feed filter names a folder of up to 100 characters, a provider and an asset class of up "
    "to 64, a state of all, started or stopped, and words of up to 200.";

/// A filter member trimmed, or a refusal when it is longer than allowed.
std::string bounded_text(const std::string& raw, std::size_t most) {
    auto value = rr::trimmed(raw);
    if (rr::characters_of(value) > most)
        rr::invalid(filter_sentence);
    return value;
}

/// The filter a page or a selection states, checked.
feed_filter filter_of(const std::string& folder,
                      const std::string& provider,
                      const std::string& asset_class,
                      const std::string& state,
                      bool should_run,
                      const std::string& search) {
    static const std::set<std::string, std::less<>> states{"", "all", "started", "stopped"};
    if (!states.contains(state))
        rr::invalid(filter_sentence);
    return {bounded_text(folder, 100),
            bounded_text(provider, 64),
            bounded_text(asset_class, 64),
            state,
            should_run,
            bounded_text(search, 200)};
}

/// A truth value named in an address, read as the BFF read it.
bool truth_of(const std::string& raw) {
    std::string value = rr::trimmed(raw);
    std::ranges::transform(
        value, value.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    static const std::set<std::string, std::less<>> yes{"true", "1", "yes", "on", "y", "enabled"};
    static const std::set<std::string, std::less<>> no{"false", "0", "no", "off", "n", "disabled"};
    if (yes.contains(value))
        return true;
    if (no.contains(value))
        return false;
    rr::invalid(filter_sentence);
}

/// A whole number named in an address, from lowest to highest, or its default when left out.
int whole_of(
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

/// The answer of a start or stop that states only the service's words.
http_response message_answer(const std::string& message) {
    return http_response::json("{\"message\":" + json::write(message) + "}");
}

/// A feed's row, written as an object, with whether it runs added as its last member.
std::string with_running(std::string row, bool running) {
    const std::string member = std::string("\"running\":") + (running ? "true" : "false");
    if (row.size() < 2 || row.back() != '}')
        throw std::logic_error("A feed row is not written as an object.");
    row.pop_back();
    return row + (row.size() == 1 ? "" : ",") + member + "}";
}

/// The feed or folder the address names.
std::string id_of(const http_request& req) {
    auto id = req.get_path_param("id");
    if (!rr::is_uuid(id))
        rr::invalid("A feed or folder is named by its identifier.");
    return id;
}

/// A start or stop the service refused, which conflicts with the feed's state.
void require_done(bool success, const std::string& message) {
    if (!success)
        rr::refuse(http_status::conflict, "conflict", message);
}

/// The selection a start or stop states, on the request the service reads.
template <typename Request>
Request selection_of(const http_request& req) {
    const auto body = rr::read_body<feed_operation::selection_body>(req.body);
    if (!body)
        rr::invalid(filter_sentence);
    const auto filter = filter_of(body->folder.value_or(""),
                                  body->provider.value_or(""),
                                  body->assetClass.value_or(""),
                                  body->state.value_or(""),
                                  body->shouldRun.value_or(false),
                                  body->search.value_or(""));
    Request msg;
    msg.folder_id = filter.folder;
    msg.provider_code = filter.provider;
    msg.asset_class = filter.asset_class;
    msg.state = filter.state;
    msg.should_run = filter.should_run;
    msg.search = filter.search;
    msg.feed_ids = body->feedIds.value_or(std::vector<std::string>{});
    msg.excluded_ids = body->excludedIds.value_or(std::vector<std::string>{});
    const auto too_long = [](const std::string& id) {
        return id.size() > 40;
    };
    if (msg.feed_ids.size() > feed_selection_most_ids ||
        msg.excluded_ids.size() > feed_selection_most_ids ||
        std::ranges::any_of(msg.feed_ids, too_long) ||
        std::ranges::any_of(msg.excluded_ids, too_long))
        rr::invalid("A selection names up to 2000 feeds to pick and 2000 to leave out.");
    return msg;
}

}

void feed_operation_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"synthetic"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/synthetic/feeds/{id}/start")
            .summary("Start one feed")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::start_feed_request>([&] {
                    synthetic_messaging::start_feed_request msg;
                    msg.config_id = id_of(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    require_done(reply.success, reply.message);
                    return message_answer(reply.message);
                });
            }));

    add(router->post("/api/synthetic/feeds/{id}/stop")
            .summary("Stop one feed")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::stop_feed_request>([&] {
                    synthetic_messaging::stop_feed_request msg;
                    msg.config_id = id_of(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    require_done(reply.success, reply.message);
                    return message_answer(reply.message);
                });
            }));

    add(router->post("/api/synthetic/folders/{id}/start")
            .summary("Start every feed under a folder, its subtree included")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::start_feeds_under_folder_request>([&] {
                    marketdata_messaging::start_feeds_under_folder_request msg;
                    msg.folder_id = id_of(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    require_done(reply.success, reply.message);
                    return http_response::json(
                        "{\"message\":" + json::write(reply.message) +
                        ",\"started\":" + std::to_string(reply.started) +
                        ",\"alreadyRunning\":" + std::to_string(reply.already_running) +
                        ",\"skipped\":" + std::to_string(reply.skipped) +
                        ",\"byKind\":" + json::write(reply.by_kind) + "}");
                });
            }));

    add(router->post("/api/synthetic/folders/{id}/stop")
            .summary("Stop every feed under a folder, its subtree included")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::stop_feeds_under_folder_request>([&] {
                    marketdata_messaging::stop_feeds_under_folder_request msg;
                    msg.folder_id = id_of(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    require_done(reply.success, reply.message);
                    return http_response::json("{\"message\":" + json::write(reply.message) +
                                               ",\"stopped\":" + std::to_string(reply.stopped) +
                                               ",\"byKind\":" + json::write(reply.stopped_by_kind) +
                                               "}");
                });
            }));

    add(router->get("/api/synthetic/feed-folders")
            .summary("The synthetic folder tree")
            .description("A composed view owned by the synthetic service: the children of one "
                         "folder, or the roots, with the feeds under each at any depth and how "
                         "many of each kind, so no screen reads the feeds. One folder call; the "
                         "service counts. The answer carries an ETag and is revalidated at each "
                         "read, because the caller's own folder writes change it.")
            .response<feed_folder_tree>()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::list_feed_folders_request>([&] {
                    rr::refuse_unknown_params(req, {"parent"});
                    synthetic_messaging::list_feed_folders_request msg;
                    msg.parent_id = rr::trimmed(req.get_query_param("parent"));
                    if (rr::characters_of(msg.parent_id) > 100)
                        rr::invalid("A folder tree read names a parent of up to 100 characters.");
                    auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   reply.message.empty() ?
                                       "The synthetic service refused the folder tree read." :
                                       reply.message);
                    return ores::http::net::view_answer(
                        req,
                        json::write(feed_folder_tree{std::move(reply.folders)}),
                        ores::http::net::view_cache_policy::revalidated());
                });
            }));

    add(router->get("/api/synthetic/folder-feeds")
            .summary("Read a page of the feeds a filter matches, with whether each runs")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::list_folder_feeds_request>([&] {
                    static const std::set<std::string, std::less<>> known{"folder",
                                                                          "provider",
                                                                          "assetClass",
                                                                          "state",
                                                                          "shouldRun",
                                                                          "search",
                                                                          "offset",
                                                                          "limit"};
                    for (const auto& [name, value] : req.query_params)
                        if (!known.contains(name))
                            rr::invalid("A feed page cannot be narrowed by " + name + ".");
                    const auto should_run = req.query_params.contains("shouldRun") &&
                                            truth_of(req.get_query_param("shouldRun"));
                    const auto filter = filter_of(req.get_query_param("folder"),
                                                  req.get_query_param("provider"),
                                                  req.get_query_param("assetClass"),
                                                  req.get_query_param("state"),
                                                  should_run,
                                                  req.get_query_param("search"));
                    synthetic_messaging::list_folder_feeds_request msg;
                    msg.folder_id = filter.folder;
                    msg.provider_code = filter.provider;
                    msg.asset_class = filter.asset_class;
                    msg.state = filter.state;
                    msg.should_run = filter.should_run;
                    msg.search = filter.search;
                    msg.offset = whole_of(req, "offset", 0, 1'000'000, 0);
                    msg.limit = whole_of(req, "limit", 1, 200, 60);
                    const auto page = forwarding::forward(req, session, msg);
                    if (!page.success)
                        rr::invalid(page.message);

                    // A feed runs when the service runs a generator for it,
                    // demanded or manual, and the running set names each one.
                    const auto running = forwarding::forward(
                        req, session, synthetic_messaging::list_feeds_request{});
                    if (!running.success)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   "The running feeds could not be read.");
                    std::set<std::string, std::less<>> runs;
                    for (const auto& feed : running.running_feeds)
                        runs.insert(feed.source_name);

                    std::string feeds = "[";
                    for (const auto& feed : page.feeds) {
                        feeds += (feeds.size() == 1 ? "" : ",") +
                                 with_running(json::write(feed), runs.contains(feed.source_name));
                    }
                    return http_response::json("{\"total\":" + std::to_string(page.total) +
                                               ",\"feeds\":" + feeds + "]}");
                });
            }));

    add(router->post("/api/synthetic/feed-selection/start")
            .summary("Start the picked feeds, or the feeds a filter matches less those left out")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::start_selected_feeds_request>([&] {
                    const auto msg =
                        selection_of<synthetic_messaging::start_selected_feeds_request>(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success)
                        rr::invalid(reply.message);
                    return http_response::json(
                        "{\"message\":" + json::write(reply.message) +
                        ",\"started\":" + std::to_string(reply.started) +
                        ",\"alreadyRunning\":" + std::to_string(reply.already_running) +
                        ",\"skipped\":" + std::to_string(reply.skipped) + "}");
                });
            }));

    add(router->post("/api/synthetic/feed-selection/stop")
            .summary("Stop the picked feeds, or the feeds a filter matches less those left out")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::stop_selected_feeds_request>([&] {
                    const auto msg =
                        selection_of<synthetic_messaging::stop_selected_feeds_request>(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success)
                        rr::invalid(reply.message);
                    return http_response::json("{\"message\":" + json::write(reply.message) +
                                               ",\"stopped\":" + std::to_string(reply.stopped) +
                                               "}");
                });
            }));
}

}
