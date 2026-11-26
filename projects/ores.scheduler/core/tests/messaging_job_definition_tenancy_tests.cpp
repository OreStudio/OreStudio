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
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.scheduler.api/domain/cron_expression.hpp"
#include "ores.scheduler.api/messaging/job_definition_protocol.hpp"
#include "ores.scheduler.core/messaging/job_definition_handler.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <utility>
#include <vector>

// A service that registers a job for a tenant does not name the tenant on the
// wire: it enters the tenant, and the job lands in the session's tenant. This
// test states that landing where it can be checked.

namespace {

// The handler answers on a subject of the test's own, so the running scheduler,
// which subscribes the protocol's subject, never answers this request.
const std::string tenancy_subjects("ores.test.scheduler.job_definition_tenancy.");

struct job_definition_tenancy_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    std::vector<ores::nats::service::subscription> subs;

    job_definition_tenancy_fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        subs.push_back(nats.queue_subscribe(
            tenancy_subjects + "put_job_definition", "ores.test", [this](ores::nats::message m) {
                ores::scheduler::messaging::job_definition_handler(
                    nats, db.context(), std::optional(keys.verifier()))
                    .put_job_definition(std::move(m));
            }));
        subs.push_back(nats.queue_subscribe(
            tenancy_subjects + "list_job_definitions", "ores.test", [this](ores::nats::message m) {
                ores::scheduler::messaging::job_definition_handler(
                    nats, db.context(), std::optional(keys.verifier()))
                    .list_job_definitions(std::move(m));
            }));
    }
};

ores::scheduler::messaging::put_job_definition_request make_request(const std::string& job_name) {
    const auto cron = ores::scheduler::domain::cron_expression::from_string("0 0 * * *");
    REQUIRE(cron);
    ores::scheduler::messaging::job_definition_change change;
    change.write.id = boost::uuids::random_generator()();
    change.write.job_name = job_name;
    change.write.description = "Registered for a tenant by another tenant's session";
    change.write.command = "";
    change.write.schedule_expression = *cron;
    change.write.action_type = "nats_publish";
    change.write.action_payload = "{}";
    change.write.is_active = true;
    change.precondition.kind = ores::utility::domain::precondition_kind::any;
    return {
        .change = std::move(change),
        .intent = {.reason_code = std::string(ores::service::messaging::change_reasons::new_record),
                   .commentary = "Tenancy test"}};
}

}

TEST_CASE("a_put_from_a_session_of_another_tenant_writes_the_job_into_that_tenant") {
    job_definition_tenancy_fixture f;
    const auto job_name = "tenancy." + boost::uuids::to_string(boost::uuids::random_generator()());

    // The token names the test tenant, which is not the system tenant the
    // scheduler serves the platform's jobs from.
    const auto token = f.keys.token_without_party(
        f.db.tenant_id().to_string(),
        {"scheduler::job_definitions:write", "scheduler::job_definitions:read"});
    const auto req = make_request(job_name);

    const auto reply =
        ores::testing::request(f.nats, tenancy_subjects + "put_job_definition", token, req);
    const auto response =
        ores::testing::decode_reply<ores::scheduler::messaging::put_job_definition_response>(reply);
    REQUIRE(response);
    REQUIRE(response->result.outcome == ores::utility::domain::outcome::ok);

    // The tenant's own read returns the job, which is what a tenant's screen
    // does, and the installation sees nothing, which is what keeps the
    // platform's job list its own.
    ores::scheduler::messaging::list_job_definitions_request list_req;
    list_req.limit = 1000;
    const auto list_reply =
        ores::testing::request(f.nats, tenancy_subjects + "list_job_definitions", token, list_req);
    const auto listed =
        ores::testing::decode_reply<ores::scheduler::messaging::list_job_definitions_response>(
            list_reply);
    REQUIRE(listed);
    REQUIRE(listed->result.outcome == ores::utility::domain::outcome::ok);
    const auto tenant = f.db.tenant_id();
    CHECK(std::ranges::any_of(listed->definitions, [&job_name, &tenant](const auto& j) {
        return j.job_name == job_name && j.tenant_id == tenant;
    }));

    // The installation's own read does not carry the job: it is the tenant's
    // row, not a platform job.
    const auto system_token = f.keys.token_without_party(
        ores::utility::uuid::tenant_id::system().to_string(), {"scheduler::job_definitions:read"});
    const auto system_reply = ores::testing::request(
        f.nats, tenancy_subjects + "list_job_definitions", system_token, list_req);
    const auto system_listed =
        ores::testing::decode_reply<ores::scheduler::messaging::list_job_definitions_response>(
            system_reply);
    REQUIRE(system_listed);
    CHECK(std::ranges::none_of(system_listed->definitions,
                               [&job_name](const auto& j) { return j.job_name == job_name; }));
}
