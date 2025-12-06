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
#include "ores.notification/domain/notification.hpp"
#include "ores.notification/domain/notification_json_io.hpp"
#include "ores.notification/domain/notification_table_io.hpp"
#include "ores.notification/generators/notification_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include <sstream>

namespace {

const std::string test_suite("ores.notification.tests");
const std::string tags("[domain]");

}

using namespace ores::notification::domain;
using namespace ores::notification::generators;

TEST_CASE("notification_json_serialization", tags) {
    auto n = notification_generator::generate();
    std::stringstream ss;
    ss << n;
    REQUIRE_FALSE(ss.str().empty());
    // Further checks could involve deserializing and comparing, or checking for specific fields
    // For now, just ensuring it doesn't crash and produces output is sufficient for a stub.
}

TEST_CASE("notification_table_serialization", tags) {
    auto notifications = notification_generator::generate_set(3);
    std::stringstream ss;
    ss << notifications;
    REQUIRE_FALSE(ss.str().empty());
    // Basic check for table headers and some content
    REQUIRE(ss.str().find("Entity") != std::string::npos);
    REQUIRE(ss.str().find("Timestamp") != std::string::npos);
}

TEST_CASE("notification_generator", tags) {
    auto n = notification_generator::generate();
    REQUIRE_FALSE(n.entity.empty());
    REQUIRE(n.timestamp.time_since_epoch().count() != 0);

    auto notifications = notification_generator::generate_set(5);
    REQUIRE(notifications.size() == 5);
    for (const auto& notif : notifications) {
        REQUIRE_FALSE(notif.entity.empty());
        REQUIRE(notif.timestamp.time_since_epoch().count() != 0);
    }
}