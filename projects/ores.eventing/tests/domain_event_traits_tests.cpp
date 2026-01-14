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
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.refdata/eventing/currency_changed_event.hpp"
#include "ores.iam/eventing/account_changed_event.hpp"

#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[event_traits]");

}

using namespace ores::eventing::domain;
using ores::refdata::eventing::currency_changed_event;
using ores::iam::eventing::account_changed_event;

TEST_CASE("event_traits_currency_changed_event", tags) {
    REQUIRE(event_traits<currency_changed_event>::name == "ores.risk.currency_changed");

    // Verify the concept works
    STATIC_REQUIRE(has_event_traits<currency_changed_event>);
}

TEST_CASE("event_traits_account_changed_event", tags) {
    REQUIRE(event_traits<account_changed_event>::name == "ores.iam.account_changed");

    // Verify the concept works
    STATIC_REQUIRE(has_event_traits<account_changed_event>);
}

TEST_CASE("event_bus_with_domain_events", "[event_traits][event_bus]") {
    ores::eventing::service::event_bus bus;

    bool currency_received = false;
    bool account_received = false;
    std::chrono::system_clock::time_point received_timestamp;

    auto sub1 = bus.subscribe<currency_changed_event>([&](const currency_changed_event& e) {
        currency_received = true;
        received_timestamp = e.timestamp;
    });

    auto sub2 = bus.subscribe<account_changed_event>([&](const account_changed_event& e) {
        account_received = true;
    });

    auto now = std::chrono::system_clock::now();
    bus.publish(currency_changed_event{now});

    REQUIRE(currency_received);
    REQUIRE_FALSE(account_received);
    REQUIRE(received_timestamp == now);
}
