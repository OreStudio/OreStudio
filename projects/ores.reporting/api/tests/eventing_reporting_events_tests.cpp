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
#include "ores.reporting.api/eventing/concurrency_policy_changed_event.hpp"
#include "ores.reporting.api/eventing/report_definition_changed_event.hpp"
#include "ores.reporting.api/eventing/report_instance_changed_event.hpp"
#include "ores.reporting.api/eventing/report_type_changed_event.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.eventing/service/event_bus.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[eventing]");

}

using ores::reporting::eventing::concurrency_policy_changed_event;
using ores::reporting::eventing::report_definition_changed_event;
using ores::reporting::eventing::report_instance_changed_event;
using ores::reporting::eventing::report_type_changed_event;
using ores::eventing::domain::event_traits;
using ores::eventing::domain::has_event_traits;
using namespace ores::logging;

TEST_CASE("event_traits_concurrency_policy_changed_event", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Testing concurrency_policy_changed_event traits";

    REQUIRE(event_traits<concurrency_policy_changed_event>::name ==
        "ores.reporting.concurrency_policy_changed");
    STATIC_REQUIRE(has_event_traits<concurrency_policy_changed_event>);
}

TEST_CASE("event_traits_report_definition_changed_event", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Testing report_definition_changed_event traits";

    REQUIRE(event_traits<report_definition_changed_event>::name ==
        "ores.reporting.report_definition_changed");
    STATIC_REQUIRE(has_event_traits<report_definition_changed_event>);
}

TEST_CASE("event_traits_report_instance_changed_event", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Testing report_instance_changed_event traits";

    REQUIRE(event_traits<report_instance_changed_event>::name ==
        "ores.reporting.report_instance_changed");
    STATIC_REQUIRE(has_event_traits<report_instance_changed_event>);
}

TEST_CASE("event_traits_report_type_changed_event", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Testing report_type_changed_event traits";

    REQUIRE(event_traits<report_type_changed_event>::name ==
        "ores.reporting.report_type_changed");
    STATIC_REQUIRE(has_event_traits<report_type_changed_event>);
}

TEST_CASE("create_concurrency_policy_changed_event", tags) {
    auto lg(make_logger(test_suite));

    concurrency_policy_changed_event sut;
    sut.timestamp = std::chrono::system_clock::now();
    sut.codes = {"skip", "queue"};
    sut.tenant_id = "system";

    BOOST_LOG_SEV(lg, info) << "Concurrency policy changed event - codes: "
        << sut.codes.size();

    CHECK(sut.codes.size() == 2);
    CHECK(sut.codes[0] == "skip");
    CHECK(sut.tenant_id == "system");
}

TEST_CASE("create_report_definition_changed_event", tags) {
    auto lg(make_logger(test_suite));

    report_definition_changed_event sut;
    sut.timestamp = std::chrono::system_clock::now();
    sut.ids = {"uuid-def-1", "uuid-def-2"};
    sut.tenant_id = "tenant1";

    BOOST_LOG_SEV(lg, info) << "Report definition changed event - ids: "
        << sut.ids.size();

    CHECK(sut.ids.size() == 2);
    CHECK(sut.tenant_id == "tenant1");
}

TEST_CASE("create_report_instance_changed_event", tags) {
    auto lg(make_logger(test_suite));

    report_instance_changed_event sut;
    sut.timestamp = std::chrono::system_clock::now();
    sut.ids = {"uuid-inst-1"};
    sut.tenant_id = "tenant1";

    BOOST_LOG_SEV(lg, info) << "Report instance changed event - ids: "
        << sut.ids.size();

    CHECK(sut.ids.size() == 1);
    CHECK(!sut.tenant_id.empty());
}

TEST_CASE("create_report_type_changed_event", tags) {
    auto lg(make_logger(test_suite));

    report_type_changed_event sut;
    sut.timestamp = std::chrono::system_clock::now();
    sut.codes = {"risk", "grid"};
    sut.tenant_id = "system";

    BOOST_LOG_SEV(lg, info) << "Report type changed event - codes: "
        << sut.codes.size();

    CHECK(sut.codes.size() == 2);
    CHECK(sut.tenant_id == "system");
}

TEST_CASE("event_bus_report_definition_changed_subscription", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    bool event_received = false;
    std::vector<std::string> received_ids;

    auto sub = bus.subscribe<report_definition_changed_event>(
        [&](const report_definition_changed_event& e) {
            event_received = true;
            received_ids = e.ids;
        });

    report_definition_changed_event event;
    event.timestamp = std::chrono::system_clock::now();
    event.ids = {"uuid-abc", "uuid-def"};
    event.tenant_id = "tenant1";

    BOOST_LOG_SEV(lg, info) << "Publishing report_definition_changed_event";
    bus.publish(event);

    CHECK(event_received);
    CHECK(received_ids.size() == 2);
}

TEST_CASE("event_bus_reporting_events_isolation", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    bool definition_received = false;
    bool instance_received = false;
    bool type_received = false;
    bool policy_received = false;

    auto sub1 = bus.subscribe<report_definition_changed_event>(
        [&](const report_definition_changed_event&) { definition_received = true; });
    auto sub2 = bus.subscribe<report_instance_changed_event>(
        [&](const report_instance_changed_event&) { instance_received = true; });
    auto sub3 = bus.subscribe<report_type_changed_event>(
        [&](const report_type_changed_event&) { type_received = true; });
    auto sub4 = bus.subscribe<concurrency_policy_changed_event>(
        [&](const concurrency_policy_changed_event&) { policy_received = true; });

    report_definition_changed_event event;
    event.timestamp = std::chrono::system_clock::now();
    event.ids = {"uuid-only-def"};
    event.tenant_id = "system";

    BOOST_LOG_SEV(lg, info) << "Publishing only report_definition_changed_event";
    bus.publish(event);

    CHECK(definition_received);
    CHECK_FALSE(instance_received);
    CHECK_FALSE(type_received);
    CHECK_FALSE(policy_received);
}
