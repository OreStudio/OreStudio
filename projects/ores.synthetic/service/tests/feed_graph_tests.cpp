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
#include "../src/feed_graph.hpp"
#include "../src/feed_node_journal.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <string>
#include <vector>

namespace {

using ores::synthetic::messaging::feed_graph_node;
using ores::synthetic::service::build_feed_graph;
using ores::synthetic::service::derive_states;
using ores::synthetic::service::feed_graph;
using ores::synthetic::service::feed_kind;
using ores::synthetic::service::feed_start_target;
using ores::synthetic::service::graph_source;

const feed_kind spot_kind{.kind = std::string(ores::synthetic::feed::fx_spot_feed_kind)};
const feed_kind curve_kind{.kind = std::string(ores::synthetic::feed::ir_curve_feed_kind)};
const feed_kind surface_kind{.kind = std::string(ores::synthetic::feed::fx_vol_surface_feed_kind)};

const std::string spot = "synthetic.bloomberg.fx.fx_spot.audusd";
const std::string aud = "synthetic.bloomberg.ir.yield_curve.aonia.aud";
const std::string usd = "synthetic.bloomberg.ir.yield_curve.sofr.usd";
const std::string latent = "synthetic.latent.fx.fx_vol_surface.audusd";
const std::string bbg_view = "synthetic.bloomberg.fx.fx_option.audusd";
const std::string ice_view = "synthetic.ice.fx.fx_option.audusd";

feed_start_target upstream(const feed_kind& kind, const std::string& name) {
    feed_start_target t;
    t.row.kind = &kind;
    t.row.candidate.source_name = name;
    t.container_provider_code = "bloomberg";
    t.container_is_system = true;
    return t;
}

feed_start_target surface() {
    feed_start_target t;
    t.row.kind = &surface_kind;
    t.row.candidate.source_name = latent;
    t.row.candidate.fx_pair = std::pair{std::string("AUD"), std::string("USD")};
    t.row.candidate.inputs = {.spot = spot, .base_curve = aud, .quote_curve = usd};
    t.row.candidate.contributor_providers = {{"c-bbg", "bloomberg"}, {"c-ice", "ice"}};
    t.container_provider_code = "latent";
    t.container_is_system = true;
    return t;
}

feed_graph audusd() {
    return build_feed_graph({upstream(spot_kind, spot),
                             upstream(curve_kind, aud),
                             upstream(curve_kind, usd),
                             surface()},
                            {{.name = "bloomberg-a", .provider_code = "bloomberg"},
                             {.name = "ice-a", .provider_code = "ice"}});
}

const feed_graph_node& node(const feed_graph& g, const std::string& name) {
    const auto found = std::ranges::find(g.nodes, name, &feed_graph_node::name);
    REQUIRE(found != g.nodes.end());
    return *found;
}

}

TEST_CASE("the graph names every node and the input each reads", "[feed_graph]") {
    const auto g = audusd();
    CHECK(node(g, spot).kind == "upstream");
    CHECK(node(g, latent).kind == "latent");
    CHECK(node(g, ice_view).kind == "view");
    CHECK(node(g, "ice-a").kind == "source");
    const auto has = [&g](const std::string& from, const std::string& to, const std::string& role) {
        return std::ranges::any_of(g.edges, [&](const auto& e) {
            return e.node == from && e.input == to && e.role == role;
        });
    };
    CHECK(has(latent, spot, "spot"));
    CHECK(has(latent, aud, "base_curve"));
    CHECK(has(latent, usd, "quote_curve"));
    CHECK(has(ice_view, latent, "surface"));
    CHECK(has(ice_view, "ice-a", "source"));
    CHECK(has(spot, "bloomberg-a", "source"));
}

TEST_CASE("every node runs when nothing is stopped", "[feed_graph]") {
    auto g = audusd();
    derive_states(g, {});
    for (const auto& n : g.nodes)
        CHECK(n.state == "running");
}

TEST_CASE("a stopped upstream starves the surface and every view, each naming its cause",
          "[feed_graph]") {
    auto g = audusd();
    derive_states(g, {spot});
    CHECK(node(g, spot).state == "stopped");
    CHECK(node(g, latent).state == "starved");
    CHECK(node(g, latent).cause == spot);
    CHECK(node(g, bbg_view).state == "starved");
    CHECK(node(g, bbg_view).cause == latent);
    CHECK(node(g, ice_view).cause == latent);
    CHECK(node(g, aud).state == "running");
}

TEST_CASE("a stopped view stops that view only", "[feed_graph]") {
    auto g = audusd();
    derive_states(g, {ice_view});
    CHECK(node(g, ice_view).state == "stopped");
    CHECK(node(g, bbg_view).state == "running");
    CHECK(node(g, latent).state == "running");
}

TEST_CASE("a stopped source starves its own upstreams and views only", "[feed_graph]") {
    auto g = audusd();
    derive_states(g, {"ice-a"});
    CHECK(node(g, ice_view).state == "starved");
    CHECK(node(g, ice_view).cause == "ice-a");
    CHECK(node(g, bbg_view).state == "running");
    CHECK(node(g, latent).state == "running");
}

TEST_CASE("a stopped node keeps its own reason when its input stops too", "[feed_graph]") {
    auto g = audusd();
    derive_states(g, {spot, latent});
    CHECK(node(g, latent).state == "stopped");
    CHECK(node(g, latent).cause.empty());
    CHECK(node(g, ice_view).cause == latent);
}

TEST_CASE("a surface that names a curve no feed publishes is starved on it", "[feed_graph]") {
    auto s = surface();
    s.row.candidate.inputs.quote_curve = "synthetic.nobody.ir.x";
    auto g = build_feed_graph({upstream(spot_kind, spot), upstream(curve_kind, aud), s}, {});
    derive_states(g, {});
    CHECK(node(g, latent).state == "starved");
    CHECK(node(g, latent).cause == "synthetic.nobody.ir.x");
}

TEST_CASE("the part of the graph around a node holds its upstreams and downstreams",
          "[feed_graph]") {
    const auto g = ores::synthetic::service::around(audusd(), latent);
    const auto names = [&g] {
        std::set<std::string> out;
        for (const auto& n : g.nodes)
            out.insert(n.name);
        return out;
    }();
    CHECK(names.contains(spot));
    CHECK(names.contains(usd));
    CHECK(names.contains(ice_view));
    CHECK(names.contains("bloomberg-a"));
    CHECK(ores::synthetic::service::around(audusd(), "synthetic.nobody").nodes.empty());
}

TEST_CASE("a surface that reads itself is starved on the cycle, not running", "[feed_graph]") {
    auto s = surface();
    s.row.candidate.inputs.spot = latent;
    auto g = build_feed_graph({upstream(curve_kind, aud), upstream(curve_kind, usd), s}, {});
    derive_states(g, {});
    CHECK(node(g, latent).state == "starved");
    CHECK(node(g, latent).cause == latent);
}

TEST_CASE("an edge read twice is kept once", "[feed_graph]") {
    auto g = build_feed_graph({upstream(spot_kind, spot), upstream(spot_kind, spot)},
                              {{.name = "bloomberg-a", .provider_code = "bloomberg"}});
    CHECK(std::ranges::count_if(g.edges, [](const auto& e) { return e.node == spot; }) == 1);
}

TEST_CASE("a surface holding a diagnostic is failing, and starves its views on it",
          "[feed_graph]") {
    auto g = audusd();
    derive_states(g, {}, {{latent, "no market: curve:USD"}});
    CHECK(node(g, latent).state == "failing");
    CHECK(node(g, latent).reason == "no market: curve:USD");
    CHECK(node(g, ice_view).state == "starved");
    CHECK(node(g, ice_view).cause == latent);
}

TEST_CASE("a stopped or starved node is not failing, whatever it holds", "[feed_graph]") {
    auto g = audusd();
    derive_states(g, {spot}, {{latent, "no market: spot"}, {spot, "a diagnostic"}});
    CHECK(node(g, spot).state == "stopped");
    CHECK(node(g, latent).state == "starved");
    CHECK(node(g, latent).cause == spot);
}

TEST_CASE("a stopped node says who stopped it and why", "[feed_graph]") {
    auto g = audusd();
    derive_states(g, {spot}, {}, {{spot, "Stopped by alice (common.other): a test."}});
    CHECK(node(g, spot).reason == "Stopped by alice (common.other): a test.");
}

TEST_CASE("the journal says a diagnostic once raised, its clearing, and each state change",
          "[feed_graph]") {
    std::vector<std::string> said;
    ores::synthetic::service::feed_node_journal journal(
        [&said](const std::string& node, std::string_view level, const std::string& message) {
            said.push_back(node + "|" + std::string(level) + "|" + message);
        });
    journal.raise(latent, "no market: curve:USD");
    journal.raise(latent, "no market: curve:USD");
    CHECK(journal.diagnostics().at(latent) == "no market: curve:USD");
    journal.clear(latent);
    journal.clear(latent);
    CHECK(journal.diagnostics().empty());
    REQUIRE(said.size() == 2);
    CHECK(said[0] == latent + "|warn|Failing: no market: curve:USD");
    CHECK(said[1] == latent + "|info|Produces again.");

    said.clear();
    auto before = audusd();
    derive_states(before, {});
    auto after = audusd();
    derive_states(after, {ice_view});
    journal.say_changes(before, after);
    REQUIRE(said.size() == 1);
    CHECK(said[0].starts_with(ice_view + "|warn|Now stopped"));
}
