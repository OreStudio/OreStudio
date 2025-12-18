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
#include <chrono>
#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.comms/net/client_session.hpp"

namespace {

const std::string tags("[net][client_session][notification]");

using namespace ores::comms::net;

// ============================================================================
// pending_notification structure tests
// ============================================================================

TEST_CASE("pending_notification stores event_type and timestamp", tags) {
    pending_notification notif;
    notif.event_type = "ores.risk.currency_changed";
    notif.timestamp = std::chrono::system_clock::now();

    REQUIRE(notif.event_type == "ores.risk.currency_changed");
    REQUIRE(notif.timestamp.time_since_epoch().count() > 0);
}

// ============================================================================
// client_session notification queue tests (disconnected state)
// ============================================================================

TEST_CASE("client_session has no pending notifications initially", tags) {
    client_session session;

    REQUIRE_FALSE(session.has_pending_notifications());
    REQUIRE(session.take_pending_notifications().empty());
}

TEST_CASE("client_session is_subscribed returns false when not connected", tags) {
    client_session session;

    REQUIRE_FALSE(session.is_subscribed("any.event"));
}

TEST_CASE("client_session get_subscriptions returns empty when not connected", tags) {
    client_session session;

    REQUIRE(session.get_subscriptions().empty());
}

TEST_CASE("client_session subscribe returns false when not connected", tags) {
    client_session session;

    REQUIRE_FALSE(session.subscribe("any.event"));
}

TEST_CASE("client_session unsubscribe returns false when not connected", tags) {
    client_session session;

    REQUIRE_FALSE(session.unsubscribe("any.event"));
}

}
