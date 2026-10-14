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
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include "ores.reporting.core/repository/report_trigger_event_repository.hpp"
#include "ores.reporting.core/service/report_run_admission_service.hpp"
#include "ores.scheduler.api/generators/job_definition_generator.hpp"
#include "ores.scheduler.core/repository/job_definition_repository.hpp"
#include "ores.scheduler.core/repository/job_instance_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <rfl/msgpack.hpp>
#include <stdexcept>

// The decision these pin: report_instance.trigger_id is a real foreign key, so
// an instance is admitted only by a trigger that exists, and the one entry
// point both an on-demand and a scheduled trigger call writes both rows. The
// tests read the rows back through the repositories, so what is asserted is
// what the store holds rather than what the service returned.

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[admission][service][integration]");

ores::database::context
write_test_party_and_scope_context(ores::testing::scoped_database_helper& h,
                                   ores::utility::generation::generation_context& ctx) {
    using ores::refdata::repository::party_repository;
    party_repository party_repo;
    auto party = ores::refdata::generators::generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    auto existing = party_repo.read_latest(h.context());
    for (const auto& e : existing) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    party_repo.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

// The body the scheduler's nats_publish action publishes on each firing.
//
// The scheduler keeps its own copy, in
// ores.scheduler.core/src/service/nats_publish_action_handler.cpp, because it
// cannot depend on the reporting API. The codec is positional, so that copy and
// the reporting request type must match field for field: a field the handler
// declares and the scheduler does not send fails the whole decode, not just
// that field. That is the state this test was written to catch -- the scheduler
// sent three fields while the request type had four, so the reporting service
// answered bad_request and no scheduled run was ever admitted.
//
// Stating the shape here rather than importing it is deliberate and is what
// makes the seam visible: no compiler sees both ends of this contract, so the
// next field added to either side fails here, at the receiving end, where the
// silent rejection used to be.
struct scheduler_trigger_body {
    std::string report_definition_id;
    std::string tenant_id;
    std::int64_t job_instance_id = 0;
    std::string kind;
};

ores::reporting::messaging::trigger_report_instance_request
decode_the_scheduler_trigger(const scheduler_trigger_body& body) {
    const auto decoded =
        rfl::msgpack::read<ores::reporting::messaging::trigger_report_instance_request>(
            rfl::msgpack::write(body));
    REQUIRE(decoded.has_value());
    return *decoded;
}

}

using namespace ores::logging;
using ores::reporting::repository::report_instance_repository;
using ores::reporting::repository::report_trigger_event_repository;
using ores::reporting::service::report_run_admission_service;
using ores::testing::scoped_database_helper;
using ores::workflow::service::load_fsm_states;

TEST_CASE("an on-demand run is admitted by the trigger it records", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto party_ctx = write_test_party_and_scope_context(h, ctx);

    // An instance names an FSM state of the report instance machine. The
    // machines and their states are system-tenant reference data, so they are
    // read there whatever tenant the run is admitted in, exactly as the live
    // registrar reads them.
    const auto states = load_fsm_states(
        party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()),
        "report_instance_lifecycle");

    report_run_admission_service::plan plan;
    plan.definition_id = ctx.generate_uuid();
    plan.tenant_id = party_ctx.tenant_id();
    plan.party_id = *party_ctx.party_id();
    plan.name = "an on demand run";
    plan.trigger_type = "on_demand";
    plan.kind = "production";
    plan.fsm_state_id = states.require("pending");

    report_run_admission_service admission(party_ctx);
    const auto receipt = admission.admit(plan);

    // The trigger the database holds is the one the instance names, and it is
    // the trigger every field of the plan says it is. An on-demand trigger
    // names no scheduler job and no firing.
    const auto triggers = report_trigger_event_repository().read_latest(
        party_ctx, boost::uuids::to_string(receipt.trigger_id));
    REQUIRE(triggers.size() == 1);
    const auto& trigger = triggers.front();
    CHECK(trigger.trigger_type == "on_demand");
    CHECK(trigger.report_definition_id == plan.definition_id);
    CHECK(trigger.party_id == plan.party_id);
    CHECK(trigger.tenant_id == plan.tenant_id);
    CHECK_FALSE(trigger.scheduler_job_id.has_value());
    CHECK_FALSE(trigger.schedule_job_run_id.has_value());

    const auto instances = report_instance_repository().read_latest(
        party_ctx, boost::uuids::to_string(receipt.instance_id));
    REQUIRE(instances.size() == 1);
    CHECK(instances.front().trigger_id == receipt.trigger_id);
    CHECK(instances.front().definition_id == plan.definition_id);
    CHECK(instances.front().name == plan.name);
}

TEST_CASE("a plan that is not one of the two trigger types is refused", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto party_ctx = write_test_party_and_scope_context(h, ctx);

    report_run_admission_service::plan plan;
    plan.definition_id = ctx.generate_uuid();
    plan.tenant_id = party_ctx.tenant_id();
    plan.party_id = *party_ctx.party_id();
    plan.name = "a run nobody asked for";
    plan.trigger_type = "whenever";
    plan.kind = "production";

    report_run_admission_service admission(party_ctx);
    CHECK_THROWS_AS(admission.admit(plan), std::invalid_argument);

    // Nothing was written, so the next admission with the same name is the
    // first one the store sees.
    auto triggers = report_trigger_event_repository().read_latest(party_ctx);
    for (const auto& trigger : triggers)
        CHECK(trigger.report_definition_id != plan.definition_id);
}

TEST_CASE("the scheduler's own firing reaches the reporting handler as a scheduled trigger", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    const auto definition_id = ctx.generate_uuid();
    const auto job_instance_id = std::int64_t{918273645};
    const auto body =
        scheduler_trigger_body{.report_definition_id = boost::uuids::to_string(definition_id),
                               .tenant_id = h.tenant_id().to_string(),
                               .job_instance_id = job_instance_id,
                               .kind = "production"};

    const auto req = decode_the_scheduler_trigger(body);

    // The firing is what makes the handler take the scheduled branch, and it
    // only survives if the scheduler puts it on the wire: the handler reads a
    // zero here as a trigger sent by hand.
    CHECK(req.job_instance_id == job_instance_id);
    CHECK(req.report_definition_id == definition_id);
    CHECK(req.tenant_id == h.tenant_id().to_uuid());
    CHECK(req.kind == "production");
}

TEST_CASE("a scheduled run is admitted by the trigger that names its firing", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto party_ctx = write_test_party_and_scope_context(h, ctx);

    const auto states = load_fsm_states(
        party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()),
        "report_instance_lifecycle");

    // The trigger table refuses a scheduler job or a firing that does not
    // exist, so the run is admitted against the rows a real firing leaves: the
    // job the definition was scheduled with, and the instance that firing
    // wrote. Naming them is the whole difference between the two trigger types.
    auto job = ores::scheduler::generators::generate_synthetic_job_definition(ctx);
    job.tenant_id = party_ctx.tenant_id();
    job.party_id = *party_ctx.party_id();
    ores::scheduler::repository::job_definition_repository().write(party_ctx, job);

    ores::scheduler::domain::job_instance firing_instance;
    firing_instance.tenant_id = party_ctx.tenant_id().to_uuid();
    firing_instance.party_id = *party_ctx.party_id();
    firing_instance.job_definition_id = job.id;
    firing_instance.action_type = job.action_type;
    firing_instance.status = ores::scheduler::domain::job_status::starting;
    firing_instance.triggered_at = std::chrono::system_clock::now();
    firing_instance.started_at = firing_instance.triggered_at;
    const auto firing = ores::scheduler::repository::job_instance_repository().write_started(
        party_ctx, firing_instance);
    REQUIRE(firing != 0);

    report_run_admission_service::plan plan;
    plan.definition_id = ctx.generate_uuid();
    plan.tenant_id = party_ctx.tenant_id();
    plan.party_id = *party_ctx.party_id();
    plan.name = "a scheduled run";
    plan.trigger_type = "scheduled";
    plan.scheduler_job_id = job.id;
    plan.schedule_job_run_id = firing;
    plan.kind = "production";
    plan.fsm_state_id = states.require("pending");

    report_run_admission_service admission(party_ctx);
    const auto receipt = admission.admit(plan);

    // The trigger the schema holds is a scheduled one, and it carries both the
    // scheduler job and the firing that produced it. A scheduled trigger that
    // names no job is refused by the table itself, so reaching this read at all
    // says the job was named.
    const auto triggers = report_trigger_event_repository().read_latest(
        party_ctx, boost::uuids::to_string(receipt.trigger_id));
    REQUIRE(triggers.size() == 1);
    const auto& trigger = triggers.front();
    CHECK(trigger.trigger_type == "scheduled");
    REQUIRE(trigger.scheduler_job_id.has_value());
    CHECK(*trigger.scheduler_job_id == job.id);
    REQUIRE(trigger.schedule_job_run_id.has_value());
    CHECK(*trigger.schedule_job_run_id == firing);
    CHECK(trigger.report_definition_id == plan.definition_id);
    CHECK(trigger.party_id == plan.party_id);
    CHECK(trigger.tenant_id == plan.tenant_id);

    const auto instances = report_instance_repository().read_latest(
        party_ctx, boost::uuids::to_string(receipt.instance_id));
    REQUIRE(instances.size() == 1);
    CHECK(instances.front().trigger_id == receipt.trigger_id);
    CHECK(instances.front().definition_id == plan.definition_id);
    CHECK(instances.front().name == plan.name);
}
