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
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/service/client.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

const std::string tags("[service][entity_event_publisher]");

}

TEST_CASE("publish_entity_event_rethrows_on_nats_failure", tags) {
    // A client that was never connected fails every publish with
    // NATS_NOT_CONNECTED. The publisher must surface that failure, not
    // swallow it: the event_bus handler-error path is what makes it visible.
    ores::nats::service::client nats(ores::nats::config::nats_options{});

    const ores::eventing::domain::entity_change_event ev{
        "ores.test.entity",
        std::chrono::system_clock::now(),
        {"id-1"},
        "test-tenant"};

    REQUIRE_THROWS_AS(
        ores::eventing::service::publish_entity_event(nats, "ores.test.v1.events", ev),
        std::runtime_error);
}
