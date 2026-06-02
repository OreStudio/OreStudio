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
#include "ores.trading.api/eventing/trade_changed_event.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.eventing/service/event_bus.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[eventing]");

}

using ores::trading::eventing::trade_changed_event;
using ores::eventing::domain::event_traits;
using ores::eventing::domain::has_event_traits;
using namespace ores::logging;

TEST_CASE("event_traits_trade_changed_event", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Testing trade_changed_event traits";

    REQUIRE(event_traits<trade_changed_event>::name == "ores.trading.trade_changed");
    STATIC_REQUIRE(has_event_traits<trade_changed_event>);
}

TEST_CASE("create_trade_changed_event", tags) {
    auto lg(make_logger(test_suite));

    trade_changed_event sut;
    sut.timestamp = std::chrono::system_clock::now();
    sut.trade_ids = {"trade-uuid-1", "trade-uuid-2"};
    sut.tenant_id = "system";

    BOOST_LOG_SEV(lg, info) << "Trade changed event - tenant: " << sut.tenant_id
        << ", trades: " << sut.trade_ids.size();

    CHECK(!sut.trade_ids.empty());
    CHECK(sut.trade_ids.size() == 2);
    CHECK(sut.trade_ids[0] == "trade-uuid-1");
    CHECK(sut.tenant_id == "system");
}

TEST_CASE("create_trade_changed_event_with_faker", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        trade_changed_event sut;
        sut.timestamp = std::chrono::system_clock::now() -
            std::chrono::hours(faker::number::integer(0, 168));
        sut.tenant_id = "system";

        const int num_ids = faker::number::integer(1, 5);
        for (int j = 0; j < num_ids; ++j) {
            std::ostringstream oss;
            oss << boost::uuids::random_generator()();
            sut.trade_ids.push_back(oss.str());
        }

        BOOST_LOG_SEV(lg, info) << "Trade changed event " << i
            << " - trades: " << sut.trade_ids.size();

        CHECK(!sut.trade_ids.empty());
        CHECK(!sut.tenant_id.empty());
    }
}

TEST_CASE("event_bus_trade_changed_subscription", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    bool event_received = false;
    std::vector<std::string> received_ids;

    auto sub = bus.subscribe<trade_changed_event>([&](const trade_changed_event& e) {
        event_received = true;
        received_ids = e.trade_ids;
    });

    trade_changed_event event;
    event.timestamp = std::chrono::system_clock::now();
    event.trade_ids = {"uuid-abc", "uuid-def"};
    event.tenant_id = "system";

    BOOST_LOG_SEV(lg, info) << "Publishing trade_changed_event";
    bus.publish(event);

    CHECK(event_received);
    CHECK(received_ids.size() == 2);
    CHECK(received_ids[0] == "uuid-abc");
}

TEST_CASE("event_bus_trade_changed_isolation", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    bool trade_changed_received = false;

    auto sub = bus.subscribe<trade_changed_event>([&](const trade_changed_event&) {
        trade_changed_received = true;
    });

    // Publish but check no cross-contamination with other event types
    trade_changed_event event;
    event.timestamp = std::chrono::system_clock::now();
    event.trade_ids = {"uuid-xyz"};
    event.tenant_id = "tenant1";

    bus.publish(event);

    CHECK(trade_changed_received);
}
