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
#include "ores.eventing/domain/entity_change_event.hpp"
#include "ores.eventing/domain/entity_change_event_json_io.hpp"
#include "ores.eventing/domain/entity_change_event_table_io.hpp"
#include "ores.eventing/generators/entity_change_event_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include <sstream>

namespace {

const std::string test_suite("ores.eventing.tests");
const std::string tags("[domain]");

}

using namespace ores::eventing::domain;
using namespace ores::eventing::generators;

TEST_CASE("entity_change_event_json_serialization", tags) {
    auto n = entity_change_event_generator::generate();
    std::stringstream ss;
    ss << n;
    REQUIRE_FALSE(ss.str().empty());
}

TEST_CASE("entity_change_event_table_serialization", tags) {
    auto notifications = entity_change_event_generator::generate_set(3);
    std::stringstream ss;
    ss << notifications;
    REQUIRE_FALSE(ss.str().empty());
    REQUIRE(ss.str().find("Entity") != std::string::npos);
    // REQUIRE(ss.str().find("Timestamp") != std::string::npos); // FIXME
}

TEST_CASE("entity_change_event_generator", tags) {
    auto n = entity_change_event_generator::generate();
    REQUIRE_FALSE(n.entity.empty());
    REQUIRE_FALSE(n.tenant_id.empty());
    // REQUIRE(n.timestamp.time_since_epoch().count() != 0);  // FIXME

    auto events = entity_change_event_generator::generate_set(5);
    REQUIRE(events.size() == 5);
    for (const auto& e : events) {
        REQUIRE_FALSE(e.entity.empty());
        REQUIRE_FALSE(e.tenant_id.empty());
        // REQUIRE(e.timestamp.time_since_epoch().count() != 0); // FIXME
    }
}
