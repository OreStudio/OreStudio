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
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>


TEST_CASE("a raw query's setting holding a quote reaches the session unchanged",
          "[session_settings]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto ctx = h.context().with_tenant(h.tenant_id(), "o'brien");
    const auto rows = ores::database::query_builder::execute_raw_multi_column_query(
        ctx, "select ores_iam_current_actor_fn()", lg, "actor");
    REQUIRE(rows.size() == 1);
    CHECK(rows[0][0] == std::optional<std::string>("o'brien"));
}

TEST_CASE("an_agent_s_call_names_the_agent_and_a_person_s_names_none", "[session_settings]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto person = h.context().with_tenant(h.tenant_id(), "alice");
    const auto agent = person.with_delegated_actor(std::string("agent:1234"));
    const auto service = person.with_delegated_actor(std::string("ReportingService"));

    const auto read = [&](const ores::database::context& ctx) {
        const auto rows = ores::database::query_builder::execute_raw_multi_column_query(
            ctx, "select ores_iam_current_agent_fn()", lg, "agent");
        REQUIRE(rows.size() == 1);
        return rows[0][0];
    };
    CHECK(read(agent) == std::optional<std::string>("agent:1234"));
    CHECK_FALSE(read(person).has_value());
    // A run token's actor is a service, which is not an agent.
    CHECK_FALSE(read(service).has_value());
}
