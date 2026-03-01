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
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][hierarchy][hierarchy_builder]");

using nt = ores::ore::hierarchy::import_node::node_type;

// Helper: find node by name and type
const ores::ore::hierarchy::import_node* find_node(
    const std::vector<ores::ore::hierarchy::import_node>& nodes,
    const std::string& name, nt type) {
    for (const auto& n : nodes) {
        if (n.name == name && n.type == type)
            return &n;
    }
    return nullptr;
}

std::size_t find_index(
    const std::vector<ores::ore::hierarchy::import_node>& nodes,
    const std::string& name, nt type) {
    for (std::size_t i = 0; i < nodes.size(); ++i) {
        if (nodes[i].name == name && nodes[i].type == type)
            return i;
    }
    return nodes.size();  // sentinel
}

}

using ores::ore::hierarchy::ore_hierarchy_builder;
using ores::ore::hierarchy::import_node;
using namespace ores::logging;

// =============================================================================
// Single-level tests (no subdirectories)
// =============================================================================

TEST_CASE("build_single_flat_file_produces_one_book", tags) {
    auto lg(make_logger(test_suite));

    const std::filesystem::path root = "/data/ore";
    const std::vector<std::filesystem::path> files = {
        root / "portfolio.xml"
    };

    ore_hierarchy_builder builder(files, root);
    const auto nodes = builder.build();

    BOOST_LOG_SEV(lg, debug) << "Nodes: " << nodes.size();

    REQUIRE(nodes.size() == 1);
    CHECK(nodes[0].name == "portfolio");
    CHECK(nodes[0].type == nt::book);
    CHECK(!nodes[0].parent_index.has_value());
    REQUIRE(nodes[0].source_files.size() == 1);
    CHECK(nodes[0].source_files[0] == root / "portfolio.xml");
}

TEST_CASE("build_multiple_flat_files_produce_multiple_books", tags) {
    auto lg(make_logger(test_suite));

    const std::filesystem::path root = "/data/ore";
    const std::vector<std::filesystem::path> files = {
        root / "portfolio_swaps.xml",
        root / "portfolio_options.xml"
    };

    ore_hierarchy_builder builder(files, root);
    const auto nodes = builder.build();

    BOOST_LOG_SEV(lg, debug) << "Nodes: " << nodes.size();

    REQUIRE(nodes.size() == 2);

    const auto* swaps = find_node(nodes, "portfolio_swaps", nt::book);
    const auto* opts = find_node(nodes, "portfolio_options", nt::book);
    REQUIRE(swaps != nullptr);
    REQUIRE(opts != nullptr);
    CHECK(!swaps->parent_index.has_value());
    CHECK(!opts->parent_index.has_value());
}

// =============================================================================
// Subdirectory tests
// =============================================================================

TEST_CASE("build_one_level_subdirectory_creates_portfolio_and_book", tags) {
    auto lg(make_logger(test_suite));

    const std::filesystem::path root = "/data/ore";
    const std::vector<std::filesystem::path> files = {
        root / "Rates" / "portfolio_irs.xml"
    };

    ore_hierarchy_builder builder(files, root);
    const auto nodes = builder.build();

    BOOST_LOG_SEV(lg, debug) << "Nodes: " << nodes.size();

    REQUIRE(nodes.size() == 2);

    const auto* rates = find_node(nodes, "Rates", nt::portfolio);
    const auto* book = find_node(nodes, "portfolio_irs", nt::book);
    REQUIRE(rates != nullptr);
    REQUIRE(book != nullptr);

    const std::size_t rates_idx = find_index(nodes, "Rates", nt::portfolio);
    REQUIRE(book->parent_index.has_value());
    CHECK(*book->parent_index == rates_idx);
}

TEST_CASE("build_two_level_subdirectory_creates_nested_portfolios", tags) {
    auto lg(make_logger(test_suite));

    const std::filesystem::path root = "/data/ore";
    const std::vector<std::filesystem::path> files = {
        root / "Rates" / "Linear" / "portfolio_irs.xml"
    };

    ore_hierarchy_builder builder(files, root);
    const auto nodes = builder.build();

    BOOST_LOG_SEV(lg, debug) << "Nodes: " << nodes.size();

    REQUIRE(nodes.size() == 3);

    const auto* rates = find_node(nodes, "Rates", nt::portfolio);
    const auto* linear = find_node(nodes, "Linear", nt::portfolio);
    const auto* book = find_node(nodes, "portfolio_irs", nt::book);
    REQUIRE(rates != nullptr);
    REQUIRE(linear != nullptr);
    REQUIRE(book != nullptr);

    const auto rates_idx = find_index(nodes, "Rates", nt::portfolio);
    const auto linear_idx = find_index(nodes, "Linear", nt::portfolio);

    REQUIRE(linear->parent_index.has_value());
    CHECK(*linear->parent_index == rates_idx);

    REQUIRE(book->parent_index.has_value());
    CHECK(*book->parent_index == linear_idx);
}

// =============================================================================
// Dedup tests
// =============================================================================

TEST_CASE("build_deduplicates_shared_portfolio_parent", tags) {
    auto lg(make_logger(test_suite));

    const std::filesystem::path root = "/data/ore";
    const std::vector<std::filesystem::path> files = {
        root / "Rates" / "portfolio_irs.xml",
        root / "Rates" / "portfolio_ccs.xml"
    };

    ore_hierarchy_builder builder(files, root);
    const auto nodes = builder.build();

    BOOST_LOG_SEV(lg, debug) << "Nodes: " << nodes.size();

    // 1 portfolio (Rates) + 2 books
    REQUIRE(nodes.size() == 3);

    long portfolio_count = std::count_if(nodes.begin(), nodes.end(),
        [](const auto& n){ return n.type == nt::portfolio; });
    CHECK(portfolio_count == 1);

    long book_count = std::count_if(nodes.begin(), nodes.end(),
        [](const auto& n){ return n.type == nt::book; });
    CHECK(book_count == 2);
}

// =============================================================================
// Exclusion tests
// =============================================================================

TEST_CASE("build_strips_excluded_path_components", tags) {
    auto lg(make_logger(test_suite));

    const std::filesystem::path root = "/data/ore";
    const std::vector<std::filesystem::path> files = {
        root / "Input" / "portfolio.xml"
    };

    ore_hierarchy_builder builder(files, root, {"Input"});
    const auto nodes = builder.build();

    BOOST_LOG_SEV(lg, debug) << "Nodes: " << nodes.size();

    // "Input" excluded → no intermediate portfolio → just a flat book
    REQUIRE(nodes.size() == 1);
    CHECK(nodes[0].name == "portfolio");
    CHECK(nodes[0].type == nt::book);
    CHECK(!nodes[0].parent_index.has_value());
}

TEST_CASE("build_strips_excluded_intermediate_segment", tags) {
    auto lg(make_logger(test_suite));

    const std::filesystem::path root = "/data/ore";
    const std::vector<std::filesystem::path> files = {
        root / "Rates" / "Input" / "portfolio_irs.xml"
    };

    ore_hierarchy_builder builder(files, root, {"Input"});
    const auto nodes = builder.build();

    BOOST_LOG_SEV(lg, debug) << "Nodes: " << nodes.size();

    // "Input" stripped → Rates portfolio + book
    REQUIRE(nodes.size() == 2);
    CHECK(find_node(nodes, "Rates", nt::portfolio) != nullptr);
    CHECK(find_node(nodes, "portfolio_irs", nt::book) != nullptr);
}

// =============================================================================
// Edge cases
// =============================================================================

TEST_CASE("build_empty_file_list_produces_no_nodes", tags) {
    ore_hierarchy_builder builder({}, "/data/ore");
    const auto nodes = builder.build();
    CHECK(nodes.empty());
}
