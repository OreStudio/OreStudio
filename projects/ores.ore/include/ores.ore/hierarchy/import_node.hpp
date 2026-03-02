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
#ifndef ORES_ORE_HIERARCHY_IMPORT_NODE_HPP
#define ORES_ORE_HIERARCHY_IMPORT_NODE_HPP

#include <string>
#include <optional>
#include <cstddef>
#include <vector>
#include <filesystem>

namespace ores::ore::hierarchy {

/**
 * @brief A node in the ORE import hierarchy (portfolio or book).
 *
 * Represents a flat node in the hierarchy derived from the directory
 * structure. Portfolio nodes are inferred from intermediate path
 * segments; book nodes correspond to portfolio XML files.
 */
struct import_node {
    /**
     * @brief Display name of the node (portfolio or book name).
     */
    std::string name;

    /**
     * @brief Type of this node.
     */
    enum class node_type { portfolio, book } type;

    /**
     * @brief Index of the parent node in the flat list, if any.
     *
     * nullopt means this node is a root-level node.
     */
    std::optional<std::size_t> parent_index;

    /**
     * @brief Source ORE XML files for book nodes.
     *
     * Always empty for portfolio nodes.
     */
    std::vector<std::filesystem::path> source_files;
};

}

#endif
