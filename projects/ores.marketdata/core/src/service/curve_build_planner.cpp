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
#include "ores.marketdata.core/service/curve_build_planner.hpp"
#include <algorithm>
#include <optional>
#include <unordered_map>
#include <utility>

namespace ores::marketdata::service {

namespace {

std::vector<std::string> repeated_ids(const std::span<const curve_build_node> nodes) {
    std::unordered_map<std::string, std::size_t> counts;
    for (const auto& node : nodes)
        ++counts[node.id];

    std::vector<std::string> repeated;
    for (const auto& node : nodes)
        if (counts[node.id] > 1 && std::ranges::find(repeated, node.id) == repeated.end())
            repeated.push_back(node.id);
    return repeated;
}

/**
 * @brief Each node's own place in the set, by id.
 */
std::unordered_map<std::string, std::size_t>
index_by_id(const std::span<const curve_build_node> nodes) {
    std::unordered_map<std::string, std::size_t> index;
    index.reserve(nodes.size());
    for (std::size_t i = 0; i < nodes.size(); ++i)
        index.emplace(nodes[i].id, i);
    return index;
}

/**
 * @brief The one input that keeps each unbuildable node out, and the nodes that
 * no input keeps out.
 *
 * An input the set does not hold keeps its node out, and so does an input that
 * is itself kept out: an artefact that cannot build is not an input the others
 * can read. Both are settled by repeating the sweep until no node changes,
 * since an unbuildable node can be found after one that reads it.
 */
std::vector<std::optional<curve_build_edge>>
unbuildable_inputs(const std::span<const curve_build_node> nodes,
                   const std::unordered_map<std::string, std::size_t>& index) {
    std::vector<std::optional<curve_build_edge>> blocking(nodes.size());

    bool changed = true;
    while (changed) {
        changed = false;
        for (std::size_t i = 0; i < nodes.size(); ++i) {
            if (blocking[i])
                continue;
            for (const auto& input : nodes[i].inputs) {
                const auto it = index.find(input);
                if (it != index.end() && !blocking[it->second])
                    continue;
                blocking[i] = curve_build_edge{nodes[i].id, input};
                changed = true;
                break;
            }
        }
    }
    return blocking;
}

}

std::expected<curve_build_plan, curve_build_refusal>
plan_curve_builds(const std::span<const curve_build_node> nodes) {
    auto repeated = repeated_ids(nodes);
    if (!repeated.empty())
        return std::unexpected(
            curve_build_refusal{curve_build_refusal_kind::repeated_node, std::move(repeated)});

    const auto index = index_by_id(nodes);
    const auto blocking = unbuildable_inputs(nodes, index);

    curve_build_plan plan;
    // The nodes an input keeps out are reported in the order the set names them,
    // so a reader can pair the plan with the set it was made from.
    for (std::size_t i = 0; i < nodes.size(); ++i)
        if (blocking[i])
            plan.unbuildable.push_back(*blocking[i]);

    // Only the nodes every input allows take part in the ordering: an edge from a
    // node that cannot build would keep its readers out of a level for ever.
    std::vector<std::size_t> waiting_for(nodes.size(), 0);
    std::vector<std::vector<std::size_t>> readers(nodes.size());
    for (std::size_t i = 0; i < nodes.size(); ++i) {
        if (blocking[i])
            continue;
        for (const auto& input : nodes[i].inputs) {
            const auto producer = index.at(input);
            ++waiting_for[i];
            readers[producer].push_back(i);
        }
    }

    std::vector<std::size_t> current;
    for (std::size_t i = 0; i < nodes.size(); ++i)
        if (!blocking[i] && waiting_for[i] == 0)
            current.push_back(i);

    std::size_t placed = 0;
    while (!current.empty()) {
        auto& level = plan.levels.emplace_back();
        for (const auto node : current)
            level.push_back(nodes[node].id);
        placed += current.size();

        std::vector<std::size_t> next;
        for (const auto node : current)
            for (const auto reader : readers[node])
                if (--waiting_for[reader] == 0)
                    next.push_back(reader);
        current = std::move(next);
    }

    // A node every input allows but no level took reads, directly or through
    // another, from itself. Retrying the same edges cannot clear that.
    const auto buildable =
        std::ranges::count_if(blocking, [](const auto& one) { return !one.has_value(); });
    if (placed == static_cast<std::size_t>(buildable))
        return plan;

    curve_build_refusal refusal{curve_build_refusal_kind::cycle, {}};
    for (std::size_t i = 0; i < nodes.size(); ++i)
        if (!blocking[i] && waiting_for[i] > 0)
            refusal.node_ids.push_back(nodes[i].id);
    return std::unexpected(std::move(refusal));
}

}
