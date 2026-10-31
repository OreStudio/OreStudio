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
#include "ores.http.core/stream/change_coalescer.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using namespace ores::http::stream;
using namespace ores::logging;

namespace {

change_event event(std::string party, std::string entity) {
    return {
        .tenant = "acme",
        .party = std::move(party),
        .component = "iam",
        .entity = std::move(entity),
    };
}

}

TEST_CASE("a_burst_of_rows_becomes_one_frame_per_collection", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a burst is coalesced into one frame";

    change_coalescer coalescer;
    for (auto index = 0; index < 1000; ++index)
        coalescer.record(event("ops", "book"));

    REQUIRE(coalescer.pending() == 1);

    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 1);
    REQUIRE(frames.front().component == "iam");
    REQUIRE(frames.front().entity == "book");
    REQUIRE(coalescer.pending() == 0);
}

TEST_CASE("two_collections_in_one_window_become_two_frames", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that each collection gets its own frame";

    change_coalescer coalescer;
    coalescer.record(event("ops", "book"));
    coalescer.record(event("ops", "portfolio"));

    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 2);
    REQUIRE(frames[0].entity == "book");
    REQUIRE(frames[1].entity == "portfolio");
}

TEST_CASE("a_change_is_never_folded_into_another_party", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that one party's change stays its own";

    change_coalescer coalescer;
    coalescer.record(event("ops", "book"));
    coalescer.record(event("sales", "book"));

    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 2);
    for (const auto& frame : frames) {
        REQUIRE(frame.tenant == "acme");
        REQUIRE(frame.entity == "book");
    }
    REQUIRE(frames[0].party == "ops");
    REQUIRE(frames[1].party == "sales");
}

TEST_CASE("a_collection_name_is_the_component_s_own", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that two components may name one collection";

    change_coalescer coalescer;
    coalescer.record({.tenant = "acme", .party = "", .component = "iam", .entity = "orders"});
    coalescer.record({.tenant = "acme", .party = "", .component = "trading", .entity = "orders"});

    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 2);
    REQUIRE(frames[0].component == "iam");
    REQUIRE(frames[1].component == "trading");
}

TEST_CASE("a_second_window_emits_only_what_changed_in_it", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a drained change is not repeated";

    change_coalescer coalescer;
    coalescer.record(event("ops", "book"));
    REQUIRE(coalescer.drain().size() == 1);

    REQUIRE(coalescer.drain().empty());

    coalescer.record(event("ops", "portfolio"));
    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 1);
    REQUIRE(frames.front().entity == "portfolio");
}

TEST_CASE("the_same_collection_written_twice_is_one_frame", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a repeated change is one frame";

    change_coalescer coalescer;
    coalescer.record(event("ops", "book"));
    coalescer.record(event("ops", "book"));

    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 1);
}

TEST_CASE("a_change_with_no_tenant_or_no_collection_is_not_queued", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a change nobody could be shown is refused";

    change_coalescer coalescer;
    coalescer.record({.tenant = "", .party = "ops", .component = "iam", .entity = "book"});
    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = ""});

    REQUIRE(coalescer.pending() == 0);
    REQUIRE(coalescer.drain().empty());
}
