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
#include <sstream>
#include <string_view>
#include <catch2/catch_test_macros.hpp>
#include "ores.mq/domain/queue_message_event.hpp"
#include "ores.mq/domain/queue_message_event_json_io.hpp"
#include "ores.mq/generators/queue_message_event_generator.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string tags("[domain][mq]");

}

using namespace ores::mq::domain;
using namespace ores::mq::generators;
using namespace ores::utility::generation;

TEST_CASE("queue_message_event default construction", tags) {
    queue_message_event ev;
    REQUIRE(ev.queue_name.empty());
    REQUIRE(ev.msg_id == 0);
    REQUIRE(ev.encoding == payload_encoding::json);
    REQUIRE(ev.payload.empty());
    REQUIRE(ev.tenant_id.empty());
}

TEST_CASE("queue_message_event event_traits name", tags) {
    using traits = ores::eventing::domain::event_traits<queue_message_event>;
    constexpr std::string_view expected = "ores.mq.queue_message_event";
    REQUIRE(traits::name == expected);
}

TEST_CASE("queue_message_event stream operator produces output", tags) {
    queue_message_event ev;
    ev.queue_name = "test.queue";
    ev.msg_id = 42;
    ev.tenant_id = "tenant-123";

    std::ostringstream oss;
    oss << ev;
    const auto str = oss.str();

    REQUIRE(str.find("test.queue") != std::string::npos);
    REQUIRE(str.find("42") != std::string::npos);
    REQUIRE(str.find("tenant-123") != std::string::npos);
}

TEST_CASE("generate_synthetic_queue_message_event produces valid event", tags) {
    generation_context ctx(42);
    auto ev = generate_synthetic_queue_message_event("my.queue", ctx);

    REQUIRE(ev.queue_name == "my.queue");
    REQUIRE(ev.msg_id > 0);
    REQUIRE(ev.encoding == payload_encoding::json);
    REQUIRE_FALSE(ev.payload.empty());
    REQUIRE_FALSE(ev.tenant_id.empty());
}

TEST_CASE("generate_synthetic_queue_message_event uses generated queue name when empty", tags) {
    generation_context ctx(99);
    auto ev = generate_synthetic_queue_message_event("", ctx);

    REQUIRE_FALSE(ev.queue_name.empty());
}

TEST_CASE("generate_synthetic_json_queue_message_event payload is valid JSON fragment", tags) {
    generation_context ctx(7);
    auto ev = generate_synthetic_json_queue_message_event("events.queue", ctx);

    REQUIRE_FALSE(ev.payload.empty());
    REQUIRE(ev.encoding == payload_encoding::json);

    // Payload should start with '{' (JSON object)
    REQUIRE(ev.payload.front() == static_cast<std::byte>('{'));
    REQUIRE(ev.payload.back() == static_cast<std::byte>('}'));
}
