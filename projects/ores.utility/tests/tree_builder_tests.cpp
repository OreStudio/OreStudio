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
#include "ores.utility/generation/generation_engine.hpp"

#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[tree_builder]");

using namespace ores::utility::generation;

}

TEST_CASE("empty_tree_when_zero_count", tags) {
    generation_engine engine(42);
    auto nodes = generate_tree(0, 5, engine);
    REQUIRE(nodes.empty());
}

TEST_CASE("single_node_tree", tags) {
    generation_engine engine(42);
    auto nodes = generate_tree(1, 5, engine);

    REQUIRE(nodes.size() == 1);
    CHECK(nodes[0].index == 0);
    CHECK(nodes[0].depth == 0);
    CHECK(!nodes[0].parent_index.has_value());
}

TEST_CASE("flat_tree_with_max_depth_zero", tags) {
    generation_engine engine(42);
    auto nodes = generate_tree(5, 0, engine);

    // max_depth 0 means only the root is allowed.
    REQUIRE(nodes.size() == 1);
    CHECK(nodes[0].depth == 0);
}

TEST_CASE("flat_tree_with_max_depth_one", tags) {
    generation_engine engine(42);
    auto nodes = generate_tree(5, 1, engine);

    REQUIRE(nodes.size() == 5);
    CHECK(nodes[0].depth == 0);
    CHECK(!nodes[0].parent_index.has_value());

    for (std::size_t i = 1; i < nodes.size(); ++i) {
        CHECK(nodes[i].depth == 1);
        CHECK(nodes[i].parent_index.has_value());
        CHECK(*nodes[i].parent_index == 0);
    }
}

TEST_CASE("total_count_respected", tags) {
    generation_engine engine(42);
    for (std::size_t count : {1, 3, 5, 10, 20}) {
        auto nodes = generate_tree(count, 5, engine);
        CHECK(nodes.size() == count);
    }
}

TEST_CASE("parent_first_ordering", tags) {
    generation_engine engine(42);
    auto nodes = generate_tree(10, 4, engine);

    for (const auto& node : nodes) {
        if (node.parent_index.has_value()) {
            CHECK(*node.parent_index < node.index);
        }
    }
}

TEST_CASE("max_depth_respected", tags) {
    generation_engine engine(42);
    for (std::size_t max_depth : {1, 2, 3, 4}) {
        auto nodes = generate_tree(20, max_depth, engine);
        for (const auto& node : nodes) {
            CHECK(node.depth <= max_depth);
        }
    }
}

TEST_CASE("reproducible_with_same_seed", tags) {
    generation_engine engine1(42);
    auto nodes1 = generate_tree(10, 3, engine1);

    generation_engine engine2(42);
    auto nodes2 = generate_tree(10, 3, engine2);

    REQUIRE(nodes1.size() == nodes2.size());
    for (std::size_t i = 0; i < nodes1.size(); ++i) {
        CHECK(nodes1[i].index == nodes2[i].index);
        CHECK(nodes1[i].depth == nodes2[i].depth);
        CHECK(nodes1[i].parent_index == nodes2[i].parent_index);
    }
}

TEST_CASE("all_nodes_have_valid_parent_references", tags) {
    generation_engine engine(42);
    auto nodes = generate_tree(15, 4, engine);

    for (const auto& node : nodes) {
        if (node.parent_index.has_value()) {
            CHECK(*node.parent_index < nodes.size());
            CHECK(nodes[*node.parent_index].depth == node.depth - 1);
        }
    }
}
