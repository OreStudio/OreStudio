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
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.core/repository/fsm_state_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string_view>

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using ores::dq::repository::fsm_state_repository;
using ores::testing::database_helper;

// The states are seeded into the system tenant, and the test context is the
// test tenant, so every read below switches context first. Without the switch
// row-level security hides the rows and the read looks like a missing seed.

TEST_CASE("read_latest_by_machine_name_returns_the_named_machines_states", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(h.context());
    fsm_state_repository repo;

    const auto instance_states = repo.read_latest_by_machine_name(sys_ctx, "workflow_instance");
    const auto step_states = repo.read_latest_by_machine_name(sys_ctx, "workflow_step");

    // The two machines share most of their state names, so a read that ignored
    // the machine would still return a full-looking set for each. What tells
    // them apart is that each machine owns a name the other does not.
    REQUIRE_FALSE(instance_states.empty());
    REQUIRE_FALSE(step_states.empty());

    const auto has = [](const auto& states, std::string_view name) {
        return std::any_of(
            states.begin(), states.end(), [name](const auto& s) { return s.name == name; });
    };

    CHECK(has(instance_states, "compensating"));
    CHECK_FALSE(has(instance_states, "completed_with_warnings"));
    CHECK(has(step_states, "completed_with_warnings"));
    CHECK_FALSE(has(step_states, "compensating"));

    // Every row the read returned belongs to the machine it was asked for, and
    // to no other: the machine id is what a name-only read could not give.
    const auto instance_machine_id = instance_states.front().machine_id;
    for (const auto& s : instance_states)
        CHECK(s.machine_id == instance_machine_id);
    for (const auto& s : step_states)
        CHECK(s.machine_id != instance_machine_id);
}

TEST_CASE("read_latest_by_machine_name_returns_nothing_for_an_unknown_machine", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(h.context());
    fsm_state_repository repo;

    // A machine named in a typo, or one whose seed was dropped, is an empty
    // list rather than an exception: the caller decides what a missing
    // machine means for it.
    BOOST_LOG_SEV(lg, debug) << "Reading a machine that does not exist.";
    const auto states = repo.read_latest_by_machine_name(sys_ctx, "no_such_machine");

    CHECK(states.empty());
}
