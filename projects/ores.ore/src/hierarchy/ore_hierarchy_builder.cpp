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

#include <algorithm>
#include <cctype>

namespace ores::ore::hierarchy {

using namespace ores::logging;

namespace {

// Convert a raw directory name to a human-readable portfolio name.
// Replaces underscores with spaces (e.g. "TA001_Equity_Option" → "TA001 Equity Option").
std::string make_portfolio_name(const std::string& dir_name) {
    std::string s = dir_name;
    std::ranges::replace(s, '_', ' ');
    return s;
}

// Convert a raw portfolio XML file stem to a human-readable book name.
//
// ORE file naming conventions:
//   portfolio_capfloor.xml  → "Book capfloor"
//   portfolio.xml           → "Book <parent portfolio name>"
//   portfolio100.xml        → "Book 100"
//   my_custom_file.xml      → "Book my custom file"
//
// Rules applied in order:
//   1. Strip leading "portfolio_" prefix (with underscore).
//   2. Strip leading "portfolio" prefix when only digits follow (e.g. "portfolio100").
//   3. Replace underscores with spaces.
//   4. If nothing descriptive remains, fall back to the parent portfolio name.
//   5. Prefix with "Book ".
std::string make_book_name(const std::string& stem,
                           std::optional<std::size_t> parent_idx,
                           const std::vector<import_node>& nodes) {
    std::string s = stem;

    if (s.starts_with("portfolio_")) {
        s = s.substr(10); // strip "portfolio_"
    } else if (s.starts_with("portfolio")) {
        std::string rest = s.substr(9); // everything after "portfolio"
        bool all_digits = !rest.empty() &&
            std::ranges::all_of(rest, [](char c){ return std::isdigit(c) != 0; });
        if (rest.empty() || all_digits)
            s = rest; // "" or "100", "2", etc.
    }

    std::ranges::replace(s, '_', ' ');

    // Fall back to parent portfolio name when nothing descriptive remains
    if (s.empty() && parent_idx && *parent_idx < nodes.size())
        s = nodes[*parent_idx].name;

    return s.empty() ? "Book" : "Book " + s;
}

} // namespace

ore_hierarchy_builder::ore_hierarchy_builder(
    std::vector<std::filesystem::path> portfolio_files,
    std::filesystem::path root,
    std::unordered_set<std::string> exclusions)
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
        if (!exclusions_.contains(name)) {
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
            parent = find_or_add_portfolio(
                make_portfolio_name(components[i]), parent, nodes);
        }

        // Leaf: book named after the file stem (human-readable)
        const auto stem =
            std::filesystem::path(components.back()).stem().string();

        import_node book_node;
        book_node.name = make_book_name(stem, parent, nodes);
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
