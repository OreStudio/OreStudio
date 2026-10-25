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
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/messaging/report_run_pricing_handler.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "report_instance_test_fixture.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>

// The decision these pin: the date a run priced at is recorded on the run, in
// the day the packaging step used, and a run the session cannot see answers
// missing rather than being written.

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[service][database]");

}

using ores::reporting::messaging::apply_run_pricing;
using ores::reporting::messaging::record_run_pricing_request;
using ores::reporting::service::report_instance_service;
using ores::testing::scoped_database_helper;

TEST_CASE("record_run_pricing stores the date and the note on the run", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);
    const auto states = ores::reporting::tests::lifecycle_states(
        ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()), lg);
    const auto run_id =
        ores::reporting::tests::write_run_in_state(ctx, h.db_user(), gen, states.at("pending"));

    report_instance_service service(ctx);
    record_run_pricing_request request;
    request.report_instance_id = run_id;
    request.priced_at = "2026-05-05";
    request.price_note =
        "The run asked for 2026-05-06 and the gathered market data ends at 2026-05-05, so the "
        "run priced at the data's date.";

    const auto answer = apply_run_pricing(service, request);
    CHECK(answer.result.outcome == ores::utility::domain::outcome::ok);

    const auto stored = service.get_instance(run_id);
    REQUIRE(stored.has_value());
    CHECK(stored->priced_at.has_value());
    CHECK(ores::platform::time::datetime::to_db_string(*stored->priced_at) == "2026-05-05 00:00:00.000000");
    CHECK(stored->price_note == request.price_note);
}

TEST_CASE("record_run_pricing answers missing for a run the session cannot read", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);

    report_instance_service service(ctx);
    record_run_pricing_request request;
    request.report_instance_id = boost::uuids::random_generator()();
    request.priced_at = "2026-05-05";

    const auto answer = apply_run_pricing(service, request);
    CHECK(answer.result.outcome == ores::utility::domain::outcome::missing);
    CHECK(answer.result.code == "not_found");
    CHECK(answer.result.message == "The report instance does not exist.");
}

TEST_CASE("record_run_pricing refuses a date that is not one", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);
    const auto states = ores::reporting::tests::lifecycle_states(
        ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()), lg);
    const auto run_id =
        ores::reporting::tests::write_run_in_state(ctx, h.db_user(), gen, states.at("pending"));

    report_instance_service service(ctx);
    record_run_pricing_request request;
    request.report_instance_id = run_id;
    request.priced_at = "the fifth of May";

    const auto answer = apply_run_pricing(service, request);
    CHECK(answer.result.outcome == ores::utility::domain::outcome::invalid);
    CHECK(answer.result.code == "as_of_invalid");
    CHECK(!service.get_instance(run_id)->priced_at.has_value());
}
