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
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[hierarchy]");

ores::utility::domain::hierarchy_flat_row make_row(const boost::uuids::uuid& id,
                                                   std::optional<boost::uuids::uuid> parent_id,
                                                   const std::string& name) {
    ores::utility::domain::hierarchy_flat_row row;
    row.id = id;
    row.parent_id = parent_id;
    row.name = name;
    return row;
}

} // namespace

TEST_CASE("build_tree_assembles_multi_level_tree", tags) {
    auto rootId = boost::uuids::random_generator()();
    auto childId = boost::uuids::random_generator()();
    auto grandchildId = boost::uuids::random_generator()();

    std::vector<ores::utility::domain::hierarchy_flat_row> rows{
        make_row(rootId, std::nullopt, "Root"),
        make_row(childId, rootId, "Child"),
        make_row(grandchildId, childId, "Grandchild"),
    };

    auto forest = ores::utility::domain::build_tree(rows);

    REQUIRE(forest.size() == 1);
    const auto& root = forest[0];
    CHECK(root.name == "Root");
    CHECK(!root.parent_id.has_value());
    REQUIRE(root.children.size() == 1);
    const auto& child = root.children[0];
    CHECK(child.name == "Child");
    REQUIRE(child.children.size() == 1);
    CHECK(child.children[0].name == "Grandchild");
}

TEST_CASE("build_tree_supports_multiple_roots", tags) {
    auto rootAId = boost::uuids::random_generator()();
    auto rootBId = boost::uuids::random_generator()();
    auto childOfAId = boost::uuids::random_generator()();

    std::vector<ores::utility::domain::hierarchy_flat_row> rows{
        make_row(rootAId, std::nullopt, "Root A"),
        make_row(rootBId, std::nullopt, "Root B"),
        make_row(childOfAId, rootAId, "Child of A"),
    };

    auto forest = ores::utility::domain::build_tree(rows);

    REQUIRE(forest.size() == 2);

    bool foundRootA = false;
    bool foundRootB = false;
    for (const auto& node : forest) {
        if (node.name == "Root A") {
            foundRootA = true;
            REQUIRE(node.children.size() == 1);
            CHECK(node.children[0].name == "Child of A");
        } else if (node.name == "Root B") {
            foundRootB = true;
            CHECK(node.children.empty());
        }
    }
    CHECK(foundRootA);
    CHECK(foundRootB);
}

TEST_CASE("build_tree_treats_orphan_nodes_as_additional_roots", tags) {
    auto rootId = boost::uuids::random_generator()();
    auto orphanId = boost::uuids::random_generator()();
    auto missingParentId = boost::uuids::random_generator()();

    std::vector<ores::utility::domain::hierarchy_flat_row> rows{
        make_row(rootId, std::nullopt, "Root"),
        // orphan's parent_id does not match any id in the input list.
        make_row(orphanId, missingParentId, "Orphan"),
    };

    auto forest = ores::utility::domain::build_tree(rows);

    // Both nodes should surface as roots: build_tree must not crash or
    // silently drop the orphan.
    REQUIRE(forest.size() == 2);

    bool foundRoot = false;
    bool foundOrphan = false;
    for (const auto& node : forest) {
        if (node.name == "Root")
            foundRoot = true;
        if (node.name == "Orphan")
            foundOrphan = true;
    }
    CHECK(foundRoot);
    CHECK(foundOrphan);
}

TEST_CASE("build_tree_handles_empty_input", tags) {
    std::vector<ores::utility::domain::hierarchy_flat_row> rows;
    auto forest = ores::utility::domain::build_tree(rows);

    CHECK(forest.empty());
}

TEST_CASE("build_tree_keeps_first_occurrence_of_duplicate_ids", tags) {
    auto rootId = boost::uuids::random_generator()();
    auto duplicateId = boost::uuids::random_generator()();

    std::vector<ores::utility::domain::hierarchy_flat_row> rows{
        make_row(rootId, std::nullopt, "Root"),
        make_row(duplicateId, rootId, "First"),
        // Same id as above, second occurrence should be discarded.
        make_row(duplicateId, std::nullopt, "Second"),
    };

    auto forest = ores::utility::domain::build_tree(rows);

    REQUIRE(forest.size() == 1);
    const auto& root = forest[0];
    CHECK(root.name == "Root");
    REQUIRE(root.children.size() == 1);
    CHECK(root.children[0].name == "First");
}
