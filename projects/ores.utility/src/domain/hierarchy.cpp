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
#include "ores.utility/domain/hierarchy.hpp"
#include <boost/functional/hash.hpp>
#include <boost/uuid/uuid_hash.hpp>
#include <unordered_map>
#include <unordered_set>

namespace ores::utility::domain {

namespace {

using uuid_hash = boost::hash<boost::uuids::uuid>;

hierarchy_node
build_node(const boost::uuids::uuid& id,
           const std::unordered_map<boost::uuids::uuid, hierarchy_flat_row, uuid_hash>& rowsById,
           const std::unordered_map<boost::uuids::uuid, std::vector<boost::uuids::uuid>, uuid_hash>&
               childrenOf,
           std::unordered_set<boost::uuids::uuid, uuid_hash>& placed) {
    const auto& row = rowsById.at(id);
    placed.insert(id);

    hierarchy_node node;
    node.id = row.id;
    node.name = row.name;
    node.parent_id = row.parent_id;

    auto childrenIt = childrenOf.find(id);
    if (childrenIt != childrenOf.end()) {
        node.children.reserve(childrenIt->second.size());
        for (const auto& childId : childrenIt->second) {
            // A child already placed is a cycle or a second claim on one
            // child; descending again would recurse forever or duplicate the
            // subtree.
            if (placed.contains(childId))
                continue;
            node.children.push_back(build_node(childId, rowsById, childrenOf, placed));
        }
    }

    return node;
}

} // namespace

std::vector<hierarchy_node> build_tree(const std::vector<hierarchy_flat_row>& rows) {
    // Keep the first occurrence of any duplicated id (malformed input
    // handled on a best-effort basis), preserving input order for roots.
    std::unordered_map<boost::uuids::uuid, hierarchy_flat_row, uuid_hash> rowsById;
    std::unordered_set<boost::uuids::uuid, uuid_hash> seenIds;
    std::vector<boost::uuids::uuid> orderedIds;

    for (const auto& row : rows) {
        if (seenIds.contains(row.id))
            continue;
        seenIds.insert(row.id);
        rowsById.emplace(row.id, row);
        orderedIds.push_back(row.id);
    }

    // Group children by parent id, so each parent's children can be
    // gathered without repeated linear scans.
    std::unordered_map<boost::uuids::uuid, std::vector<boost::uuids::uuid>, uuid_hash> childrenOf;
    std::vector<boost::uuids::uuid> rootIds;

    for (const auto& id : orderedIds) {
        const auto& row = rowsById.at(id);
        if (row.parent_id && rowsById.contains(*row.parent_id))
            childrenOf[*row.parent_id].push_back(id);
        else
            // No parent_id, or parent_id does not refer to any row in the
            // input (orphan): treat as an additional root rather than
            // dropping it or crashing.
            rootIds.push_back(id);
    }

    std::unordered_set<boost::uuids::uuid, uuid_hash> placed;
    std::vector<hierarchy_node> roots;
    roots.reserve(rootIds.size());
    for (const auto& id : rootIds) {
        if (placed.contains(id))
            continue;
        roots.push_back(build_node(id, rowsById, childrenOf, placed));
    }

    // A parent cycle has no root, so nobody reaches its rows: a cycle is a
    // ring with no entry. Promote every row left unplaced to a root, in input
    // order, so the forest holds every row exactly once. The cycle is broken
    // at the row it is first entered, which becomes a root.
    for (const auto& id : orderedIds) {
        if (placed.contains(id))
            continue;
        roots.push_back(build_node(id, rowsById, childrenOf, placed));
    }

    return roots;
}

}
