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
#ifndef ORES_UTILITY_GENERATION_TREE_BUILDER_HPP
#define ORES_UTILITY_GENERATION_TREE_BUILDER_HPP

#include <cstddef>
#include <optional>
#include <vector>
#include "ores.utility/generation/generation_engine.hpp"

namespace ores::utility::generation {

/**
 * @brief Represents a node in a generated tree structure.
 */
struct tree_node final {
    std::size_t index;
    std::size_t depth;
    std::optional<std::size_t> parent_index;
};

/**
 * @brief Generates a tree of @p total_count nodes with maximum depth
 * @p max_depth.
 *
 * Returns nodes in parent-first order (safe for insertion into databases
 * with foreign key constraints). The first node is always a root at depth 0.
 *
 * Children are distributed breadth-first across existing nodes at each
 * depth level, with random variation controlled by the engine.
 *
 * @param total_count Total number of nodes to generate (must be >= 1).
 * @param max_depth Maximum tree depth (0 = root only, 1 = root + children).
 * @param engine Random engine for controlling distribution.
 * @return Nodes in parent-first order.
 */
std::vector<tree_node> generate_tree(
    std::size_t total_count,
    std::size_t max_depth,
    generation_engine& engine);

}

#endif
