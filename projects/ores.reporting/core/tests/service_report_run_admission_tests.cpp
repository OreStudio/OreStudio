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
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include "test_suite.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

// The decision these pin: report_instance.trigger_id is a real foreign key, so
// an instance is admitted only by a trigger that exists, and the one entry
// point both an on-demand and a scheduled trigger call writes both rows. The
// tests read the rows back through the repositories, so what is asserted is
// what the store holds rather than what the service returned.

namespace {

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

}

using namespace ores::logging;
using ores::reporting::repository::report_instance_repository;
using ores::reporting::repository::report_trigger_event_repository;
using ores::reporting::service::report_run_admission_service;
using ores::testing::scoped_database_helper;
using ores::workflow::service::load_fsm_states;

TEST_CASE("an on-demand run is admitted by the trigger it records", "[admission][service][integration]") {
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

TEST_CASE("a plan that is not one of the two trigger types is refused", "[admission][service][integration]") {
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

// The scheduler's own firing enters as a scheduled trigger, not as one sent by
// hand: the handler reads its job instance from the request, and admits the run
// with the definition's own scheduler job named. The body the scheduler
// publishes is guarded in service_scheduler_trigger_contract_tests.cpp, which
// decodes the bytes the scheduler's handler actually sends.
TEST_CASE("a scheduled run is admitted by the trigger that names its firing", "[admission][service][integration]") {
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
