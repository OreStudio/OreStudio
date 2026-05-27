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
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
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
    REQUIRE(ss.str().find("Timestamp") != std::string::npos);
}

TEST_CASE("entity_change_event_generator", tags) {
    auto n = entity_change_event_generator::generate();
    REQUIRE_FALSE(n.entity.empty());
    REQUIRE_FALSE(n.tenant_id.empty());
    REQUIRE(n.timestamp.time_since_epoch().count() != 0);

    auto events = entity_change_event_generator::generate_set(5);
    REQUIRE(events.size() == 5);
    for (const auto& e : events) {
        REQUIRE_FALSE(e.entity.empty());
        REQUIRE_FALSE(e.tenant_id.empty());
        REQUIRE(e.timestamp.time_since_epoch().count() != 0);
    }
}

// Parses a literal payload in the exact format emitted by the DB notify trigger
// (ISO 8601 UTC, T separator, Z suffix). If this test fails after a SQL change,
// the trigger timestamp format has regressed.
TEST_CASE("entity_change_event_db_payload_roundtrip", tags) {
    const std::string payload =
        R"({"entity":"ores.refdata.currency","timestamp":"2026-05-27T12:34:56Z",)"
        R"("entity_ids":["USD"],"tenant_id":"ffffffff-ffff-ffff-ffff-ffffffffffff"})";

    auto result = rfl::json::read<entity_change_event>(payload);
    REQUIRE(result.has_value());
    CHECK(result->entity == "ores.refdata.currency");
    CHECK(result->timestamp.time_since_epoch().count() != 0);
    CHECK(result->entity_ids.size() == 1);
    CHECK(result->entity_ids[0] == "USD");
    CHECK(result->tenant_id == "ffffffff-ffff-ffff-ffff-ffffffffffff");
}

// The broken format (space separator, no UTC designator) must NOT parse cleanly.
// This is a regression guard: if rfl ever accepts it silently we need to know.
TEST_CASE("entity_change_event_broken_timestamp_rejected", tags) {
    const std::string payload =
        R"({"entity":"ores.refdata.currency","timestamp":"2026-05-27 12:34:56",)"
        R"("entity_ids":["USD"],"tenant_id":"ffffffff-ffff-ffff-ffff-ffffffffffff"})";

    auto result = rfl::json::read<entity_change_event>(payload);
    CHECK_FALSE(result.has_value());
}
