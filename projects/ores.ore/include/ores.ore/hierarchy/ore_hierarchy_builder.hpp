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
#ifndef ORES_ORE_HIERARCHY_ORE_HIERARCHY_BUILDER_HPP
#define ORES_ORE_HIERARCHY_ORE_HIERARCHY_BUILDER_HPP

#include <filesystem>
#include <optional>
#include <string>
#include <vector>
#include "ores.ore/hierarchy/import_node.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::ore::hierarchy {

/**
 * @brief Builds a flat list of portfolio/book import nodes from a set of
 * portfolio XML file paths.
 *
 * Algorithm:
 * 1. For each portfolio file, compute its path relative to root.
 * 2. Strip any path components whose name appears in the exclusions list.
 * 3. Intermediate segments become de-duplicated portfolio nodes.
 * 4. The leaf segment (file stem) becomes a book node referencing the file.
 */
class ore_hierarchy_builder {
private:
    inline static std::string_view logger_name =
        "ores.ore.hierarchy.ore_hierarchy_builder";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the builder.
     *
     * @param portfolio_files Paths to ORE portfolio XML files.
     * @param root            Root directory used for computing relative paths.
     * @param exclusions      Directory name segments to strip (e.g. {"Input"}).
     */
    explicit ore_hierarchy_builder(
        std::vector<std::filesystem::path> portfolio_files,
        std::filesystem::path root,
        std::vector<std::string> exclusions = {});

    /**
     * @brief Builds the hierarchy nodes.
     *
     * The returned vector is ordered: a parent always appears before its
     * children, which guarantees that import_node::parent_index always
     * refers to an earlier entry.
     *
     * @return Flat list of import nodes.
     */
    std::vector<import_node> build();

private:
    std::vector<std::string> filtered_components(
        const std::filesystem::path& relative) const;

    std::size_t find_or_add_portfolio(
        const std::string& name,
        std::optional<std::size_t> parent,
        std::vector<import_node>& nodes) const;

    std::vector<std::filesystem::path> portfolio_files_;
    std::filesystem::path root_;
    std::vector<std::string> exclusions_;
};

}

#endif
