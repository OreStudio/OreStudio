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
#include "ores.logging/make_logger.hpp"
#include "ores.workflow.core/service/workflow_graph.hpp"
#include <catch2/catch_test_macros.hpp>
#include <algorithm>
#include <string>
#include <vector>

namespace {

const std::string test_suite("ores.workflow.core.workflow_graph_tests");
const std::string tags("[graph][unit]");
using ores::workflow::service::workflow_graph;
using ores::workflow::service::workflow_node;

/** Where a name sits in an order, or the size when it is absent. */
std::size_t position_of(const std::vector<std::string>& order, const std::string& name) {
    const auto it = std::ranges::find(order, name);
    return it == order.end() ? order.size() : static_cast<std::size_t>(it - order.begin());
}

}

TEST_CASE("a linear chain is ordered as it was written", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    // The chain the reporting path builds, reduced to its shape.
    const workflow_graph graph({{"gather_trades", {}},
                                {"gather_market_data", {}},
                                {"assemble_bundle", {"gather_trades", "gather_market_data"}},
                                {"prepare_ore_package",
                                 {"assemble_bundle", "gather_trades", "gather_market_data"}},
                                {"submit_compute", {"prepare_ore_package"}},
                                {"finalise", {"submit_compute"}}});

    REQUIRE_FALSE(graph.incoherent().has_value());
    CHECK(graph.size() == 6);
    CHECK(position_of(graph.order(), "gather_trades") <
          position_of(graph.order(), "assemble_bundle"));
    CHECK(position_of(graph.order(), "assemble_bundle") <
          position_of(graph.order(), "prepare_ore_package"));
    CHECK(position_of(graph.order(), "prepare_ore_package") <
          position_of(graph.order(), "submit_compute"));
    CHECK(position_of(graph.order(), "finalise") == 5);
}

TEST_CASE("an input that no step produces is refused", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const workflow_graph graph({{"one", {}}, {"two", {"nowhere"}}});

    REQUIRE(graph.incoherent().has_value());
    CHECK(graph.incoherent()->find("nowhere") != std::string::npos);
    CHECK(graph.ready({}, {}).empty());
}

TEST_CASE("two steps with one name are refused", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    // A consumer cannot say which of them it reads, so the chain cannot be run
    // however the run is doing. A fan-out answers this by naming each producer
    // for what it produces -- the batch key -- so the names stay distinct.
    const workflow_graph graph({{"gather", {}}, {"gather", {}}, {"combine", {"gather"}}});

    REQUIRE(graph.incoherent().has_value());
    CHECK(graph.incoherent()->find("two steps") != std::string::npos);
}

TEST_CASE("a step that reads one the engine reaches later is refused", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    // The graph derives an order and would run this chain the other way round.
    // The engine advances along the declaration, so it would dispatch "one"
    // first and find "two" had not run. A chain the engine cannot walk is a
    // definition mistake, so it is refused before the run starts.
    const workflow_graph graph({{"one", {"two"}}, {"two", {}}});

    REQUIRE(graph.incoherent().has_value());
    CHECK(graph.incoherent()->find("after it") != std::string::npos);
    CHECK(graph.ready({}, {}).empty());
}

TEST_CASE("a step is ready only once everything it reads has answered", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const workflow_graph graph({{"gather_trades", {}},
                                {"gather_market_data", {}},
                                {"assemble_bundle", {"gather_trades", "gather_market_data"}}});

    // Both producers are dispatchable at once, and the consumer of both is not.
    auto ready = graph.ready({}, {});
    CHECK(std::ranges::find(ready, "gather_trades") != ready.end());
    CHECK(std::ranges::find(ready, "gather_market_data") != ready.end());
    CHECK(std::ranges::find(ready, "assemble_bundle") == ready.end());

    // One of the two is not enough.
    ready = graph.ready({"gather_trades"}, {"gather_trades", "gather_market_data"});
    CHECK(ready.empty());

    // Both of them are.
    ready = graph.ready({"gather_trades", "gather_market_data"},
                        {"gather_trades", "gather_market_data"});
    REQUIRE(ready.size() == 1);
    CHECK(ready.front() == "assemble_bundle");

    // A step already dispatched is not offered again, however it answered.
    CHECK(graph
              .ready({"gather_trades", "gather_market_data"},
                     {"gather_trades", "gather_market_data", "assemble_bundle"})
              .empty());
}

TEST_CASE("a consumer waits on every producer that writes its input", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    // Two books, each gathered by its own batch key, and one step that reads
    // the pair. The ordinal cannot describe this: the consumer is ready when
    // the last of the producers answers, not when a position comes round.
    const workflow_graph graph({{"gather:book_a", {}},
                                {"gather:book_b", {}},
                                {"assemble", {"gather:book_a", "gather:book_b"}}});

    REQUIRE_FALSE(graph.incoherent().has_value());
    CHECK(graph.ready({"gather:book_a"}, {"gather:book_a", "gather:book_b"}).empty());

    const auto ready =
        graph.ready({"gather:book_a", "gather:book_b"}, {"gather:book_a", "gather:book_b"});
    REQUIRE(ready.size() == 1);
    CHECK(ready.front() == "assemble");
}
