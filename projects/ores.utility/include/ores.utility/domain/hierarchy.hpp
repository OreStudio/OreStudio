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
#ifndef ORES_UTILITY_DOMAIN_HIERARCHY_HPP
#define ORES_UTILITY_DOMAIN_HIERARCHY_HPP

#include "ores.utility/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::utility::domain {

/**
 * @brief A single row of a self-referencing parent-hierarchy, as returned
 * (flat) by a recursive-CTE SQL query.
 *
 * Entity-agnostic: carries only the id/parent-id/name triple needed to
 * assemble a tree. Callers map their domain entity (e.g. party,
 * counterparty, book) into a list of these before handing it to
 * build_tree().
 */
struct hierarchy_flat_row final {
    boost::uuids::uuid id{};
    std::string name;
    std::optional<boost::uuids::uuid> parent_id;
};

/**
 * @brief A single node in a generic, nested parent-child hierarchy tree.
 *
 * This is the assembled (nested) counterpart of hierarchy_flat_row: each
 * node owns its children directly, so consumers (e.g. a Qt tree model
 * builder) can walk the nesting without any parent-linking logic of
 * their own.
 */
struct hierarchy_node final {
    boost::uuids::uuid id{};
    std::string name;
    std::optional<boost::uuids::uuid> parent_id;
    std::vector<hierarchy_node> children;
};

/**
 * @brief Assembles a flat list of hierarchy_flat_row values into a forest
 * of hierarchy_node trees.
 *
 * Root nodes are rows with no parent_id, or rows whose parent_id does not
 * refer to any other row in the input (orphans). Orphans become additional
 * roots in the returned vector rather than being dropped or causing a
 * crash, since badly-seeded data can violate the "one root per tenant"
 * convention that party/counterparty hierarchies otherwise follow.
 *
 * Rows whose id appears more than once in the input are handled on a
 * best-effort basis: the first occurrence wins and later duplicates are
 * discarded.
 *
 * @param rows Flat hierarchy rows, in any order.
 * @return A forest of hierarchy_node trees (one entry per root).
 */
ORES_UTILITY_EXPORT std::vector<hierarchy_node>
build_tree(const std::vector<hierarchy_flat_row>& rows);

}

#endif
