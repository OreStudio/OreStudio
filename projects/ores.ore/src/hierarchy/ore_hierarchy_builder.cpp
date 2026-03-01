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
#include "ores.ore/hierarchy/ore_hierarchy_builder.hpp"

namespace ores::ore::hierarchy {

using namespace ores::logging;

ore_hierarchy_builder::ore_hierarchy_builder(
    std::vector<std::filesystem::path> portfolio_files,
    std::filesystem::path root,
    std::vector<std::string> exclusions)
    : portfolio_files_(std::move(portfolio_files)),
      root_(std::move(root)),
      exclusions_(std::move(exclusions)) {}

std::vector<std::string>
ore_hierarchy_builder::filtered_components(
    const std::filesystem::path& relative) const {
    std::vector<std::string> components;
    for (const auto& c : relative) {
        const auto name = c.string();
        if (name == "." || name == "..") {
            continue;
        }
        bool excluded = false;
        for (const auto& excl : exclusions_) {
            if (name == excl) {
                excluded = true;
                break;
            }
        }
        if (!excluded) {
            components.push_back(name);
        }
    }
    return components;
}

std::size_t ore_hierarchy_builder::find_or_add_portfolio(
    const std::string& name,
    std::optional<std::size_t> parent,
    std::vector<import_node>& nodes) const {
    // Search for an existing portfolio with the same name and parent
    for (std::size_t i = 0; i < nodes.size(); ++i) {
        if (nodes[i].type == import_node::node_type::portfolio &&
            nodes[i].name == name &&
            nodes[i].parent_index == parent) {
            return i;
        }
    }
    // Not found — create it
    import_node node;
    node.name = name;
    node.type = import_node::node_type::portfolio;
    node.parent_index = parent;
    nodes.push_back(std::move(node));
    return nodes.size() - 1;
}

std::vector<import_node> ore_hierarchy_builder::build() {
    BOOST_LOG_SEV(lg(), debug) << "Building hierarchy from "
                               << portfolio_files_.size() << " portfolio files";

    std::vector<import_node> nodes;

    for (const auto& file : portfolio_files_) {
        const auto relative =
            std::filesystem::relative(file, root_);
        const auto components = filtered_components(relative);

        if (components.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "No usable path components for: " << file;
            continue;
        }

        // Interior segments (all except the last) are portfolio names
        std::optional<std::size_t> parent;
        for (std::size_t i = 0; i + 1 < components.size(); ++i) {
            parent = find_or_add_portfolio(components[i], parent, nodes);
        }

        // Leaf: book named after the file stem
        const auto stem =
            std::filesystem::path(components.back()).stem().string();

        import_node book_node;
        book_node.name = stem;
        book_node.type = import_node::node_type::book;
        book_node.parent_index = parent;
        book_node.source_files = {file};
        nodes.push_back(std::move(book_node));
    }

    BOOST_LOG_SEV(lg(), info) << "Hierarchy built: "
                              << nodes.size() << " nodes";
    return nodes;
}

}
