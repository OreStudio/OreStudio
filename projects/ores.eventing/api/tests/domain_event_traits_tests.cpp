/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.iam.api/eventing/tenant_type_event.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace {

const std::string tags("[event_traits]");

/**
 * An in-process domain event that is not an entity change, such as a role
 * being assigned. It has no NATS subject; the traits name it on the bus.
 */
struct probe_event final {
    std::chrono::system_clock::time_point timestamp;
};

}

namespace ores::eventing::domain {

template <>
struct event_traits<probe_event> {
    static constexpr std::string_view name = "ores.test.probe";
};

}

using namespace ores::eventing::domain;

TEST_CASE("event_traits_name_an_in_process_event", tags) {
    REQUIRE(event_traits<probe_event>::name == "ores.test.probe");

    // Verify the concept works
    STATIC_REQUIRE(has_event_traits<probe_event>);
}

TEST_CASE("entity_event_traits_state_the_events_subject_prefix", tags) {
    // A canonical event states the collection's prefix; the action completes
    // the subject, so one payload is addressed by three subjects.
    using event_type = ores::iam::messaging::tenant_type_event;
    REQUIRE(entity_event_traits<event_type>::subject_prefix == "iam.v1.tenant_types_events");
    REQUIRE(event_subject<event_type>("created") == "iam.v1.tenant_types_events.created");
    REQUIRE(event_subject<event_type>("updated") == "iam.v1.tenant_types_events.updated");
    REQUIRE(event_subject<event_type>("deleted") == "iam.v1.tenant_types_events.deleted");
}

TEST_CASE("event_bus_with_domain_events", "[event_traits][event_bus]") {
    ores::eventing::service::event_bus bus;

    bool probe_received = false;
    bool other_received = false;
    std::chrono::system_clock::time_point received_timestamp;

    auto sub1 = bus.subscribe<probe_event>([&](const probe_event& e) {
        probe_received = true;
        received_timestamp = e.timestamp;
    });

    auto sub2 = bus.subscribe<ores::iam::messaging::tenant_type_event>(
        [&](const ores::iam::messaging::tenant_type_event&) { other_received = true; });

    auto now = std::chrono::system_clock::now();
    bus.publish(probe_event{now});

    REQUIRE(probe_received);
    REQUIRE_FALSE(other_received);
    REQUIRE(received_timestamp == now);
}
