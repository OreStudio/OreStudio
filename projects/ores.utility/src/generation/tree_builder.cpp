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
#include "ores.utility/generation/tree_builder.hpp"

#include <algorithm>

namespace ores::utility::generation {

std::vector<tree_node> generate_tree(
    std::size_t total_count,
    std::size_t max_depth,
    generation_engine& engine) {

    if (total_count == 0)
        return {};

    std::vector<tree_node> nodes;
    nodes.reserve(total_count);

    // Root node
    nodes.push_back({0, 0, std::nullopt});
    if (total_count == 1 || max_depth == 0)
        return nodes;

    // Track which nodes are at each depth level (by index into nodes vector).
    std::vector<std::vector<std::size_t>> nodes_at_depth;
    nodes_at_depth.push_back({0}); // depth 0: root

    std::size_t remaining = total_count - 1;

    for (std::size_t depth = 1; depth <= max_depth && remaining > 0; ++depth) {
        const auto& parents = nodes_at_depth[depth - 1];
        const auto parent_count = parents.size();

        // Determine how many children to create at this depth level.
        // If this is the last allowed depth, use all remaining nodes.
        // Otherwise, allocate a portion (at least 1 per parent, up to remaining).
        std::size_t children_at_level;
        if (depth == max_depth) {
            children_at_level = remaining;
        } else {
            // Allocate roughly half the remaining nodes at this level,
            // with some randomness, but at least parent_count children
            // (so every parent gets at least one child).
            const auto min_children = std::min(parent_count, remaining);
            const auto max_children = std::min(remaining,
                std::max(min_children, remaining / 2 + 1));
            children_at_level = static_cast<std::size_t>(
                engine.random_int(
                    static_cast<int>(min_children),
                    static_cast<int>(max_children)));
        }

        if (children_at_level == 0)
            break;

        std::vector<std::size_t> level_nodes;
        level_nodes.reserve(children_at_level);

        for (std::size_t i = 0; i < children_at_level; ++i) {
            // Distribute children across parents round-robin with randomness.
            const auto parent_idx = parents[i % parent_count];
            const auto node_idx = nodes.size();
            nodes.push_back({node_idx, depth, parent_idx});
            level_nodes.push_back(node_idx);
        }

        nodes_at_depth.push_back(std::move(level_nodes));
        remaining -= children_at_level;
    }

    return nodes;
}

}
