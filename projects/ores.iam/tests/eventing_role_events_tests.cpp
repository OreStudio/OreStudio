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
#include "ores.iam/eventing/role_assigned_event.hpp"
#include "ores.iam/eventing/role_revoked_event.hpp"
#include "ores.iam/eventing/permissions_changed_event.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.eventing/service/event_bus.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"
#include "ores.iam/domain/permission.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[eventing]");

}

using ores::iam::eventing::role_assigned_event;
using ores::iam::eventing::role_revoked_event;
using ores::iam::eventing::permissions_changed_event;
using ores::eventing::domain::event_traits;
using ores::eventing::domain::has_event_traits;
using namespace ores::iam::domain::permissions;
using namespace ores::utility::log;

TEST_CASE("event_traits_role_assigned_event", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Testing role_assigned_event traits";

    REQUIRE(event_traits<role_assigned_event>::name == "ores.iam.role_assigned");
    STATIC_REQUIRE(has_event_traits<role_assigned_event>);
}

TEST_CASE("event_traits_role_revoked_event", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Testing role_revoked_event traits";

    REQUIRE(event_traits<role_revoked_event>::name == "ores.iam.role_revoked");
    STATIC_REQUIRE(has_event_traits<role_revoked_event>);
}

TEST_CASE("event_traits_permissions_changed_event", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Testing permissions_changed_event traits";

    REQUIRE(event_traits<permissions_changed_event>::name == "ores.iam.permissions_changed");
    STATIC_REQUIRE(has_event_traits<permissions_changed_event>);
}

TEST_CASE("create_role_assigned_event", tags) {
    auto lg(make_logger(test_suite));

    role_assigned_event sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.role_id = boost::uuids::random_generator()();
    sut.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Role assigned event - account_id: "
        << sut.account_id << ", role_id: " << sut.role_id;

    CHECK(!sut.account_id.is_nil());
    CHECK(!sut.role_id.is_nil());
}

TEST_CASE("create_role_revoked_event", tags) {
    auto lg(make_logger(test_suite));

    role_revoked_event sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.role_id = boost::uuids::random_generator()();
    sut.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Role revoked event - account_id: "
        << sut.account_id << ", role_id: " << sut.role_id;

    CHECK(!sut.account_id.is_nil());
    CHECK(!sut.role_id.is_nil());
}

TEST_CASE("create_permissions_changed_event", tags) {
    auto lg(make_logger(test_suite));

    permissions_changed_event sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.permission_codes = {accounts_read, currencies_read, flags_read};
    sut.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Permissions changed event - account_id: "
        << sut.account_id << ", permission_codes count: " << sut.permission_codes.size();

    CHECK(!sut.account_id.is_nil());
    CHECK(sut.permission_codes.size() == 3);
}

TEST_CASE("permissions_changed_event_with_wildcard", tags) {
    auto lg(make_logger(test_suite));

    permissions_changed_event sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.permission_codes = {all};
    sut.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Permissions changed event with wildcard - account_id: "
        << sut.account_id;

    CHECK(!sut.account_id.is_nil());
    CHECK(sut.permission_codes.size() == 1);
    CHECK(sut.permission_codes[0] == "*");
}

TEST_CASE("permissions_changed_event_empty_permissions", tags) {
    auto lg(make_logger(test_suite));

    permissions_changed_event sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Permissions changed event with no permissions - account_id: "
        << sut.account_id;

    CHECK(!sut.account_id.is_nil());
    CHECK(sut.permission_codes.empty());
}

TEST_CASE("event_bus_role_assigned_subscription", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    bool event_received = false;
    boost::uuids::uuid received_account_id;
    boost::uuids::uuid received_role_id;

    auto sub = bus.subscribe<role_assigned_event>([&](const role_assigned_event& e) {
        event_received = true;
        received_account_id = e.account_id;
        received_role_id = e.role_id;
    });

    role_assigned_event event;
    event.account_id = boost::uuids::random_generator()();
    event.role_id = boost::uuids::random_generator()();
    event.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Publishing role_assigned_event";
    bus.publish(event);

    CHECK(event_received);
    CHECK(received_account_id == event.account_id);
    CHECK(received_role_id == event.role_id);
}

TEST_CASE("event_bus_role_revoked_subscription", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    bool event_received = false;
    boost::uuids::uuid received_account_id;
    boost::uuids::uuid received_role_id;

    auto sub = bus.subscribe<role_revoked_event>([&](const role_revoked_event& e) {
        event_received = true;
        received_account_id = e.account_id;
        received_role_id = e.role_id;
    });

    role_revoked_event event;
    event.account_id = boost::uuids::random_generator()();
    event.role_id = boost::uuids::random_generator()();
    event.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Publishing role_revoked_event";
    bus.publish(event);

    CHECK(event_received);
    CHECK(received_account_id == event.account_id);
    CHECK(received_role_id == event.role_id);
}

TEST_CASE("event_bus_permissions_changed_subscription", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    bool event_received = false;
    boost::uuids::uuid received_account_id;
    std::vector<std::string> received_permission_codes;

    auto sub = bus.subscribe<permissions_changed_event>([&](const permissions_changed_event& e) {
        event_received = true;
        received_account_id = e.account_id;
        received_permission_codes = e.permission_codes;
    });

    permissions_changed_event event;
    event.account_id = boost::uuids::random_generator()();
    event.permission_codes = {accounts_read, accounts_update};
    event.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Publishing permissions_changed_event";
    bus.publish(event);

    CHECK(event_received);
    CHECK(received_account_id == event.account_id);
    CHECK(received_permission_codes.size() == 2);
}

TEST_CASE("event_bus_role_events_isolation", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    bool assigned_received = false;
    bool revoked_received = false;
    bool permissions_changed_received = false;

    auto sub1 = bus.subscribe<role_assigned_event>([&](const role_assigned_event&) {
        assigned_received = true;
    });

    auto sub2 = bus.subscribe<role_revoked_event>([&](const role_revoked_event&) {
        revoked_received = true;
    });

    auto sub3 = bus.subscribe<permissions_changed_event>([&](const permissions_changed_event&) {
        permissions_changed_received = true;
    });

    role_assigned_event event;
    event.account_id = boost::uuids::random_generator()();
    event.role_id = boost::uuids::random_generator()();
    event.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Publishing role_assigned_event only";
    bus.publish(event);

    CHECK(assigned_received);
    CHECK_FALSE(revoked_received);
    CHECK_FALSE(permissions_changed_received);
}

TEST_CASE("create_role_assigned_event_with_faker", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        role_assigned_event sut;
        sut.account_id = boost::uuids::random_generator()();
        sut.role_id = boost::uuids::random_generator()();
        sut.timestamp = std::chrono::system_clock::now() -
            std::chrono::hours(faker::number::integer(0, 168));

        BOOST_LOG_SEV(lg, info) << "Role assigned event " << i << " - account_id: "
            << sut.account_id << ", role_id: " << sut.role_id;

        CHECK(!sut.account_id.is_nil());
        CHECK(!sut.role_id.is_nil());
    }
}

TEST_CASE("create_permissions_changed_event_with_faker", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> available_permissions = {
        accounts_read, accounts_create, accounts_update, accounts_delete,
        currencies_read, currencies_create, currencies_update, currencies_delete,
        flags_read, flags_create, flags_update, flags_delete
    };

    for (int i = 0; i < 3; ++i) {
        permissions_changed_event sut;
        sut.account_id = boost::uuids::random_generator()();
        sut.timestamp = std::chrono::system_clock::now();

        const int num_permissions = faker::number::integer(1, 5);
        for (int j = 0; j < num_permissions; ++j) {
            const auto idx = faker::number::integer(0,
                static_cast<int>(available_permissions.size() - 1));
            sut.permission_codes.push_back(available_permissions[idx]);
        }

        BOOST_LOG_SEV(lg, info) << "Permissions changed event " << i
            << " - account_id: " << sut.account_id
            << ", permissions count: " << sut.permission_codes.size();

        CHECK(!sut.account_id.is_nil());
        CHECK(!sut.permission_codes.empty());
    }
}
