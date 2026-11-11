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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http/routes/synthetic/calibration_routes.hpp"
#include "ores.http/routes/synthetic/feed_operation_routes.hpp"
#include "ores.http/routes/synthetic/market_regime_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::synthetic::calibration_routes;
using ores::http::routes::synthetic::feed_folder_tree;
using ores::http::routes::synthetic::feed_operation_routes;
using ores::http::routes::synthetic::market_regime_routes;
using ores::http::synthetic::tests::answer_at;
using namespace ores::logging;
namespace json = ores::utility::codec::json;
namespace synthetic_messaging = ores::synthetic::messaging;

namespace {

template <typename Routes>
http_status view_status(const std::string& path,
                        const std::string& pattern,
                        const std::map<std::string, std::string>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    return answer_at<Routes>(http_method::get, path, req, pattern).status;
}

synthetic_messaging::calibrated_feed
feed_resolved(bool from_fixing, bool refused, bool explicit_items) {
    synthetic_messaging::calibrated_feed feed;
    feed.config_id = "c";
    feed.source_name = "s";
    feed.start.from_fixing = from_fixing;
    feed.refused = refused;
    feed.explicit_items = explicit_items;
    return feed;
}

bool states(const std::string& text, const std::string& member) {
    return text.find(member) != std::string::npos;
}

}

TEST_CASE("calibration_view_counts_the_coverage_of_the_class", "[synthetic_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the calibration view's coverage and shape";

    synthetic_messaging::calibrate_response resolved;
    resolved.success = true;
    resolved.policy.from_policy_row = true;
    resolved.policy.window_days = 90;
    resolved.policy.provider_order = {"ecb"};
    resolved.feeds = {feed_resolved(true, false, false),
                      feed_resolved(false, false, true),
                      feed_resolved(false, true, false)};
    resolved.feeds[2].window_days_override = 30;

    const auto view = calibration_routes::calibration_of("fx", resolved, 4, true);
    CHECK(view.coverage.total == 3);
    CHECK(view.coverage.fromFixing == 1);
    CHECK(view.coverage.keptFixed == 1);
    CHECK(view.coverage.refused == 1);
    CHECK(view.coverage.overridden == 2);

    const auto text = json::write(view);
    for (const auto& member :
         {R"("assetClass":"fx")",
          R"("policyWritable":true)",
          R"("from_policy_row":true)",
          R"("window_days":90)",
          R"("provider_order":["ecb"])",
          R"("version":4)",
          R"("coverage":{"total":3,"fromFixing":1,"keptFixed":1,"refused":1,"overridden":2})",
          R"("feeds":[{"config_id":"c")"})
        CHECK(states(text, member));

    const auto defaults = json::write(calibration_routes::calibration_of(
        "fx", synthetic_messaging::calibrate_response{}, std::nullopt, false));
    // A class with no policy row states no version; the screen reads that as null.
    CHECK_FALSE(states(defaults, R"("version")"));
    CHECK(states(defaults, R"("policyWritable":false)"));
}

TEST_CASE("market_trace_view_names_each_member_as_the_screen_reads_it", "[synthetic_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the market trace view's shape";

    synthetic_messaging::get_market_trace_response reply;
    reply.success = true;
    reply.run_seed = 7;
    reply.heap_size = 3;
    reply.series = 2;
    synthetic_messaging::market_trace_event event;
    event.kind = "tick";
    event.source_name = "eur";
    event.operational_gap = 0.5;
    event.state = "calm";
    reply.events = {event};
    synthetic_messaging::market_series_state law;
    law.source_name = "eur";
    law.mode = "hawkes";
    law.effective_rate = 2.0;
    law.branching_ratio = 0.25;
    reply.series_states = {law};

    const auto text = json::write(market_regime_routes::trace_of(reply));
    for (const auto& member : {R"("runSeed":7)",
                               R"("ticksPerSecond":)",
                               R"("heapSize":3)",
                               R"("latenessSeconds":)",
                               R"("maxLatenessSeconds":)",
                               R"("series":2)",
                               R"("events":[{"kind":"tick","sourceName":"eur","at":)",
                               R"("operationalGap":0.5)",
                               R"("wallGap":)",
                               R"("compensatedGap":)",
                               R"("state":"calm")",
                               R"("feeds":[{"sourceName":"eur","mode":"hawkes")",
                               R"("effectiveRate":2)",
                               R"("branchingRatio":0.25)"})
        CHECK(states(text, member));
}

TEST_CASE("feed_folder_view_names_each_member_as_the_screen_reads_it", "[synthetic_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the folder tree view's shape";

    synthetic_messaging::feed_folder folder;
    folder.id = "f1";
    folder.name = "Rates";
    folder.fx_feeds = 2;
    folder.has_children = true;
    const auto text = json::write(feed_folder_tree{{folder}});
    for (const auto& member : {R"("folders":[{"id":"f1","parent_id":"","name":"Rates")",
                               R"("provider_code":)",
                               R"("fx_feeds":2)",
                               R"("ir_feeds":0)",
                               R"("has_children":true)"})
        CHECK(states(text, member));
}

TEST_CASE("synthetic_views_refuse_a_malformed_query", "[synthetic_view]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the views' refusals";

    const std::string calibration = "/api/synthetic/calibration/{assetClass}";
    CHECK(view_status<calibration_routes>("/api/synthetic/calibration/FX", calibration, {}) ==
          http_status::bad_request);
    CHECK(view_status<calibration_routes>("/api/synthetic/calibration/fx",
                                          calibration,
                                          {{"configId", "feed"}}) == http_status::bad_request);
    CHECK(view_status<calibration_routes>("/api/synthetic/calibration/fx", calibration, {}) ==
          http_status::bad_gateway);

    const std::string trace = "/api/synthetic/market-trace";
    CHECK(view_status<market_regime_routes>(trace, trace, {{"seconds", "0"}}) ==
          http_status::bad_request);
    CHECK(view_status<market_regime_routes>(trace, trace, {{"limit", "20001"}}) ==
          http_status::bad_request);
    CHECK(view_status<market_regime_routes>(trace, trace, {{"limit", "2.5"}}) ==
          http_status::bad_request);
    CHECK(view_status<market_regime_routes>(trace, trace, {{"source", std::string(257, 's')}}) ==
          http_status::bad_request);
    CHECK(view_status<market_regime_routes>(trace, trace, {{"since", "1"}}) ==
          http_status::bad_request);
    CHECK(view_status<market_regime_routes>(trace, trace, {{"seconds", "0.5e1"}}) ==
          http_status::bad_gateway);

    const std::string folders = "/api/synthetic/feed-folders";
    CHECK(view_status<feed_operation_routes>(folders, folders, {{"depth", "2"}}) ==
          http_status::bad_request);
    CHECK(view_status<feed_operation_routes>(
              folders, folders, {{"parent", std::string(101, 'p')}}) == http_status::bad_request);
    CHECK(view_status<feed_operation_routes>(folders, folders, {{"parent", " f0 "}}) ==
          http_status::bad_gateway);
}
