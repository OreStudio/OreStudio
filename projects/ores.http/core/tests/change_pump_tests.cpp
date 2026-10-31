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
#include "ores.http.core/stream/change_pump.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using namespace ores::http::stream;
using namespace ores::logging;

TEST_CASE("a_frame_names_its_entity_and_carries_its_identifiers", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the event-stream message a frame becomes";

    const coalesced_frame frame{
        .tenant = "acme",
        .party = "ops",
        .entity = "book",
        .ids = {"b1", "b2"},
    };

    REQUIRE(frame_text(frame) ==
            "event: book\n"
            "data: {\"tenant\":\"acme\",\"party\":\"ops\",\"entity\":\"book\","
            "\"ids\":[\"b1\",\"b2\"]}\n\n");
}

TEST_CASE("a_pump_gives_every_open_stream_the_window", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a pumped window reaches every stream";

    change_coalescer coalescer;
    change_hub hub;
    auto first = hub.subscribe();
    auto second = hub.subscribe();
    change_pump pump(coalescer, hub);

    coalescer.record({.tenant = "acme", .party = "ops", .entity = "book", .id = "b1"});
    coalescer.record({.tenant = "acme", .party = "ops", .entity = "book", .id = "b2"});

    REQUIRE(pump.pump() == 1);

    REQUIRE(first->size() == 1);
    REQUIRE(second->size() == 1);
    REQUIRE(first->try_pop() == second->try_pop());
}

TEST_CASE("a_window_with_nothing_in_it_publishes_nothing", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an empty window is silent";

    change_coalescer coalescer;
    change_hub hub;
    auto stream = hub.subscribe();
    change_pump pump(coalescer, hub);

    REQUIRE(pump.pump() == 0);
    REQUIRE(stream->size() == 0);
}

TEST_CASE("a_stream_that_ended_is_not_given_the_next_window", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a ended stream is forgotten by the pump";

    change_coalescer coalescer;
    change_hub hub;
    auto ended = hub.subscribe();
    auto open = hub.subscribe();
    change_pump pump(coalescer, hub);

    ended->close();
    coalescer.record({.tenant = "acme", .party = "ops", .entity = "book", .id = "b1"});

    REQUIRE(pump.pump() == 1);
    REQUIRE(ended->size() == 0);
    REQUIRE(open->size() == 1);
    REQUIRE(hub.open_streams() == 1);
}

TEST_CASE("a_frame_with_no_identifiers_still_builds_its_message", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing an empty identifier list";

    const coalesced_frame frame{
        .tenant = "acme", .party = "ops", .entity = "book", .ids = {}};

    REQUIRE(frame_text(frame).find("\"ids\":[]") != std::string::npos);
}
