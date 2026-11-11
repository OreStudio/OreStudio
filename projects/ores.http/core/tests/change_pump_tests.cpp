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
#include <chrono>
#include <optional>
#include <string>

using namespace ores::http::stream;
using namespace ores::logging;

namespace {

/// The tenant every stream hears beside its own.
const std::string pump_system_tenant = "ffffffff-ffff-ffff-ffff-ffffffffffff";

}

TEST_CASE("a_frame_names_its_collection_and_carries_its_tenancy", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the event-stream message a frame becomes";

    const coalesced_frame frame{
        .tenant = "acme",
        .party = "ops",
        .component = "iam",
        .entity = "book",
    };

    REQUIRE(frame_text(frame, "2026-11-11T04:30:00.123Z") ==
            "event: entity-changed\n"
            "data: {\"component\":\"iam\",\"entity\":\"book\","
            "\"at\":\"2026-11-11T04:30:00.123Z\",\"tenant\":\"acme\",\"party\":\"ops\"}\n\n");
}

TEST_CASE("a_pump_gives_every_open_stream_the_window", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a pumped window reaches every stream";

    change_coalescer coalescer;
    change_hub hub(pump_system_tenant);
    auto first = hub.subscribe({.tenant = "acme", .party = "ops"});
    auto second = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_pump pump(coalescer, hub);

    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "book"});

    REQUIRE(pump.pump() == 1);

    REQUIRE(first->size() == 1);
    REQUIRE(second->size() == 1);
    REQUIRE(first->try_pop() == second->try_pop());
}

TEST_CASE("a_window_with_nothing_in_it_publishes_nothing", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an empty window is silent";

    change_coalescer coalescer;
    change_hub hub(pump_system_tenant);
    auto stream = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_pump pump(coalescer, hub);

    REQUIRE(pump.pump() == 0);
    REQUIRE(stream->size() == 0);
}

TEST_CASE("a_stream_that_ended_is_not_given_the_next_window", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a ended stream is forgotten by the pump";

    change_coalescer coalescer;
    change_hub hub(pump_system_tenant);
    auto ended = hub.subscribe({.tenant = "acme", .party = "ops"});
    auto open = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_pump pump(coalescer, hub);

    ended->close();
    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "book"});

    REQUIRE(pump.pump() == 1);
    REQUIRE(ended->size() == 0);
    REQUIRE(open->size() == 1);
    REQUIRE(hub.open_streams() == 1);
}

TEST_CASE("a_change_with_no_party_builds_its_message", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a change that concerns the whole tenant";

    const coalesced_frame frame{
        .tenant = "acme",
        .party = "",
        .component = "iam",
        .entity = "book",
    };

    REQUIRE(frame_text(frame, "t").find("\"party\":\"\"") != std::string::npos);
}

TEST_CASE("two_windows_for_one_collection_leave_one_pending_frame", "[change_pump]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info)
        << "Testing that a stream that fell behind holds one frame per collection";

    change_coalescer coalescer;
    change_hub hub(pump_system_tenant);
    auto slow = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_pump pump(coalescer, hub);

    // The stream is never read, so both windows are still pending, and the second
    // window says the same thing as the first: the collection moved.
    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "book"});
    REQUIRE(pump.pump() == 1);
    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "book"});
    REQUIRE(pump.pump() == 1);

    REQUIRE(slow->size() == 1);
    const auto pending = slow->try_pop();
    REQUIRE(pending);
    REQUIRE(pending->starts_with("id: " + hub.id_of(2) + "\nevent: entity-changed\n"));
    REQUIRE(pending->find("\"component\":\"iam\",\"entity\":\"book\"") != std::string::npos);
}

TEST_CASE("an_instant_reads_as_the_browser_clock_writes_it", "[change_pump]") {
    using namespace std::chrono;
    const auto at = sys_days{year{2026} / 11 / 11} + hours{4} + minutes{30} + milliseconds{123};
    REQUIRE(instant_text(at) == "2026-11-11T04:30:00.123Z");
}
