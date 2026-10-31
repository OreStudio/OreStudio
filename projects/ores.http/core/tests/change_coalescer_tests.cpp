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

change_event event(std::string party, std::string entity, std::string id) {
    return {
        .tenant = "acme",
        .party = std::move(party),
        .entity = std::move(entity),
        .id = std::move(id),
    };
}

}

TEST_CASE("a_burst_of_rows_becomes_one_frame_per_entity", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a burst is coalesced into one frame";

    change_coalescer coalescer;
    for (auto index = 0; index < 1000; ++index)
        coalescer.record(event("ops", "book", "b" + std::to_string(index)));

    REQUIRE(coalescer.pending() == 1000);

    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 1);
    REQUIRE(frames.front().entity == "book");
    REQUIRE(frames.front().ids.size() == 1000);
    // A client compares ticks, so the identifiers are ordered.
    REQUIRE(frames.front().ids.front() == "b0");
    REQUIRE(frames.front().ids.back() == "b999");
    REQUIRE(coalescer.pending() == 0);
}

TEST_CASE("two_entities_in_one_window_become_two_frames", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that each entity gets its own frame";

    change_coalescer coalescer;
    coalescer.record(event("ops", "book", "b1"));
    coalescer.record(event("ops", "portfolio", "p1"));

    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 2);
    REQUIRE(frames[0].entity == "book");
    REQUIRE(frames[1].entity == "portfolio");
}

TEST_CASE("a_change_is_never_folded_into_another_party", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that one party's change stays its own";

    change_coalescer coalescer;
    coalescer.record(event("ops", "book", "b1"));
    coalescer.record(event("sales", "book", "b2"));

    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 2);
    for (const auto& frame : frames) {
        REQUIRE(frame.tenant == "acme");
        REQUIRE(frame.ids.size() == 1);
        if (frame.party == "ops")
            REQUIRE(frame.ids.front() == "b1");
        else
            REQUIRE(frame.ids.front() == "b2");
    }
}

TEST_CASE("a_second_window_emits_only_what_changed_in_it", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a drained change is not repeated";

    change_coalescer coalescer;
    coalescer.record(event("ops", "book", "b1"));
    REQUIRE(coalescer.drain().size() == 1);

    REQUIRE(coalescer.drain().empty());

    coalescer.record(event("ops", "book", "b2"));
    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 1);
    REQUIRE(frames.front().ids == std::vector<std::string>{"b2"});
}

TEST_CASE("the_same_row_written_twice_is_one_dirty_identifier", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a repeated change is one identifier";

    change_coalescer coalescer;
    coalescer.record(event("ops", "book", "b1"));
    coalescer.record(event("ops", "book", "b1"));

    const auto frames = coalescer.drain();

    REQUIRE(frames.size() == 1);
    REQUIRE(frames.front().ids.size() == 1);
}

TEST_CASE("a_change_with_no_identifier_is_not_queued", "[change_coalescer]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an unnamed change is refused";

    change_coalescer coalescer;
    coalescer.record({.tenant = "acme", .party = "ops", .entity = "book", .id = ""});
    coalescer.record({.tenant = "acme", .party = "ops", .entity = "", .id = "b1"});

    REQUIRE(coalescer.pending() == 0);
    REQUIRE(coalescer.drain().empty());
}
