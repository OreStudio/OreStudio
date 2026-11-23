/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro
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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_GRAPH_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_GRAPH_HPP

#include "feed_kind_registry.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief The synthetic feed graph: its nodes, each named by the naming scheme, and its edges, each
 * a node and one input it reads.
 *
 * See doc/knowledge/architecture/synthetic_feed_graph.org.
 */
struct feed_graph final {
    std::vector<ores::synthetic::messaging::feed_graph_node> nodes;
    std::vector<ores::synthetic::messaging::feed_graph_edge> edges;
};

/** @brief One simulated source the service serves: its name and its provider. */
struct graph_source final {
    std::string name;
    std::string provider_code;
};

/**
 * @brief The graph of the system collections' feeds and the sources that publish them.
 *
 * An upstream is a spot or curve feed, and reads the sources of its collection's provider. A
 * latent surface reads its spot and its two curves, by the names its config stores. A view is
 * one contributor's quotes of a surface, and reads the surface and its provider's sources. An
 * input a node names that is no node of the graph stays an edge, so the node starves on it.
 */
inline feed_graph build_feed_graph(const std::vector<feed_start_target>& targets,
                                   const std::vector<graph_source>& sources) {
    namespace msg = ores::synthetic::messaging;
    using ores::synthetic::feed::fx_option_view_name;
    using ores::synthetic::feed::fx_vol_surface_feed_kind;

    feed_graph out;
    std::map<std::string, std::vector<std::string>> sources_of;
    for (const auto& source : sources) {
        out.nodes.push_back(
            {.name = source.name, .kind = "source", .provider_code = source.provider_code});
        sources_of[source.provider_code].push_back(source.name);
    }
    const auto edge = [&out](const std::string& node, const std::string& input, std::string role) {
        if (!input.empty())
            out.edges.push_back({.node = node, .input = input, .role = std::move(role)});
    };
    const auto source_edges = [&](const std::string& node, const std::string& provider) {
        if (const auto found = sources_of.find(provider); found != sources_of.end())
            for (const auto& source : found->second)
                edge(node, source, "source");
    };

    for (const auto& target : targets) {
        if (!target.container_is_system)
            continue;
        const auto& c = target.row.candidate;
        if (target.row.kind->kind != fx_vol_surface_feed_kind) {
            out.nodes.push_back({.name = c.source_name,
                                 .kind = "upstream",
                                 .provider_code = target.container_provider_code,
                                 .feed_config_id = c.feed_config_id});
            source_edges(c.source_name, target.container_provider_code);
            continue;
        }
        out.nodes.push_back({.name = c.source_name,
                             .kind = "latent",
                             .provider_code = target.container_provider_code,
                             .feed_config_id = c.feed_config_id});
        edge(c.source_name, c.inputs.spot, "spot");
        edge(c.source_name, c.inputs.base_curve, "base_curve");
        edge(c.source_name, c.inputs.quote_curve, "quote_curve");
        if (!c.fx_pair)
            continue;
        for (const auto& [contributor, provider] : c.contributor_providers) {
            const auto view = fx_option_view_name(provider, c.fx_pair->first, c.fx_pair->second);
            out.nodes.push_back({.name = view, .kind = "view", .provider_code = provider});
            edge(view, c.source_name, "surface");
            source_edges(view, provider);
        }
    }
    std::ranges::sort(out.nodes, {}, &msg::feed_graph_node::name);
    const auto [first, last] = std::ranges::unique(out.nodes, {}, &msg::feed_graph_node::name);
    out.nodes.erase(first, last);
    std::ranges::sort(out.edges, [](const auto& a, const auto& b) {
        return std::tie(a.node, a.input) < std::tie(b.node, b.input);
    });
    return out;
}

/**
 * @brief Derives every node's state by the graph's rule.
 *
 * A node is stopped when an operator stopped it, starved when an input does not run, and running
 * otherwise. Stopped comes first, so a stopped node keeps its own reason. A starved node names
 * the first input, in edge order, that does not run, and says that input's own state and reason,
 * so the chain reads back to the first cause. An input that is no node of the graph does not
 * run. The graph has no cycles, because every edge points to a kind that comes before it.
 */
inline void derive_states(feed_graph& graph, const std::set<std::string>& stopped) {
    std::map<std::string, std::size_t> index;
    for (std::size_t i = 0; i < graph.nodes.size(); ++i)
        index.emplace(graph.nodes[i].name, i);
    std::map<std::string, std::vector<std::string>> inputs;
    for (const auto& e : graph.edges)
        inputs[e.node].push_back(e.input);

    std::set<std::string> done;
    const auto resolve = [&](const auto& self, std::size_t i) -> void {
        auto& node = graph.nodes[i];
        if (done.contains(node.name))
            return;
        done.insert(node.name);
        if (stopped.contains(node.name)) {
            node.state = "stopped";
            node.reason = "An operator stopped it.";
            return;
        }
        for (const auto& input : inputs[node.name]) {
            const auto found = index.find(input);
            if (found == index.end()) {
                node.state = "starved";
                node.cause = input;
                node.reason = "It reads " + input + ", which no feed of the graph publishes.";
                return;
            }
            self(self, found->second);
            const auto& upstream = graph.nodes[found->second];
            if (upstream.state != "running") {
                node.state = "starved";
                node.cause = input;
                node.reason = input + " is " + upstream.state + ": " + upstream.reason;
                return;
            }
        }
        node.state = "running";
    };
    for (std::size_t i = 0; i < graph.nodes.size(); ++i)
        resolve(resolve, i);
}

/**
 * @brief The part of the graph around one node: the node, every node it reads directly or
 * through others, every node that reads it directly or through others, and the edges among them.
 * An empty graph when no node has that name.
 */
inline feed_graph around(const feed_graph& graph, const std::string& name) {
    const auto known =
        std::ranges::find(graph.nodes, name, &ores::synthetic::messaging::feed_graph_node::name);
    if (known == graph.nodes.end())
        return {};
    std::set<std::string> kept{name};
    const auto walk = [&](bool up) {
        std::vector<std::string> todo{name};
        std::set<std::string> seen{name};
        while (!todo.empty()) {
            const auto at = todo.back();
            todo.pop_back();
            for (const auto& e : graph.edges) {
                const auto& from = up ? e.node : e.input;
                const auto& to = up ? e.input : e.node;
                if (from == at && seen.insert(to).second) {
                    todo.push_back(to);
                    kept.insert(to);
                }
            }
        }
    };
    walk(true);
    walk(false);
    feed_graph out;
    for (const auto& n : graph.nodes)
        if (kept.contains(n.name))
            out.nodes.push_back(n);
    for (const auto& e : graph.edges)
        if (kept.contains(e.node) && kept.contains(e.input))
            out.edges.push_back(e);
    return out;
}

}

#endif
