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
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/messaging/report_run_health_handler.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include "report_instance_test_fixture.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>
#include <vector>

// The decision these pin: the shell's one-run verb answers the same read the
// web asks, so the word and the state it came from are the service's, and a run
// the session cannot see is reported as missing rather than as a run in a state
// with no word.

using ores::reporting::messaging::get_report_instance_health_request;
using ores::reporting::messaging::project_run_health;
using ores::reporting::service::report_instance_service;
using ores::testing::scoped_database_helper;

TEST_CASE("get_run_health answers the word and the state of one run", "[service][database]") {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);
    const auto states = ores::reporting::tests::lifecycle_states(
        ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()), lg);
    REQUIRE(states.contains("failed"));

    const auto run_id =
        ores::reporting::tests::write_run_in_state(ctx, h.db_user(), gen, states.at("failed"));

    report_instance_service service(ctx);
    get_report_instance_health_request read;
    read.instance_ids = {run_id};
    const auto answer = project_run_health(service.get_report_instance_health(read));

    CHECK(answer.result.outcome == ores::utility::domain::outcome::ok);
    CHECK(answer.state_name == "failed");
    CHECK(answer.health == "failed");
}

TEST_CASE("get_run_health answers missing for a run the session cannot read",
          "[service][database]") {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);
    const auto unknown_id = boost::uuids::random_generator()();

    report_instance_service service(ctx);
    get_report_instance_health_request read;
    read.instance_ids = {unknown_id};
    const auto answer = project_run_health(service.get_report_instance_health(read));

    CHECK(answer.result.outcome == ores::utility::domain::outcome::missing);
    CHECK(answer.result.code == "not_found");
    CHECK(answer.result.message == "The report instance does not exist.");
    CHECK(answer.health.empty());
    CHECK(answer.state_name.empty());
}
