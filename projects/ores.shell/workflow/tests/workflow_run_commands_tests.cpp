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
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/workflow/workflow_run_commands.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cli/cli.h>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::commands::workflow_run_commands;
using ores::workflow::messaging::get_step_result_response;
using ores::workflow::messaging::get_workflow_steps_response;
using ores::workflow::messaging::workflow_step_summary;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.workflow.tests");
const std::string tags("[commands]");

/// A step that finished, at @p index.
workflow_step_summary finished_step(int index, std::string name) {
    return workflow_step_summary{.id = "step-" + std::to_string(index),
                                 .name = std::move(name),
                                 .status = "completed",
                                 .step_index = index};
}

/// A step result the server holds, decoded from @p json.
get_step_result_response result_holding(std::string json) {
    return get_step_result_response{
        .found = true, .result_json = std::move(json), .success = true};
}

const std::string handoff_result =
    R"({"kind":"start_tenant_setup","tenant_id":"0d2b1c44-6f6a-4f0e-9d33-71d4bb0c1f21",)"
    R"("child_instance_id":"3b891255-54ad-4eb0-9b3a-79f1010e61ab"})";

/// The same shape, for a step that hands nothing on.
const std::string no_handoff_result =
    R"({"kind":"system_provision","tenant_id":"","child_instance_id":"","bundles":["system_core"]})";

}

TEST_CASE("workflow_run_commands_registers_the_wait_verb", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    workflow_run_commands::register_commands(root_menu, session);

    // A publish that declines to block tells the operator to follow progress
    // with this verb, so a wait that is no longer registered turns a shipped
    // instruction into an unknown command.
    const auto completions = root_menu.GetCompletions("workflow ");
    CHECK(std::find(completions.begin(), completions.end(), std::string{"workflow wait"}) !=
          completions.end());
}

TEST_CASE("workflow_run_commands_start_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    workflow_run_commands::process_start(
        out, session, {"identity_workflow", R"({"steps":[{"name":"one"}]})"});

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("workflow_run_commands_start_usage_names_the_instance_id_flag", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    workflow_run_commands::process_start(out, session, {"identity_workflow"});

    BOOST_LOG_SEV(lg, debug) << "Usage output: " << out.str();
    CHECK(out.str().find("--instance-id") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("workflow_run_commands_start_refuses_an_instance_id_that_is_not_a_uuid", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    // Refused before the session is consulted: a value that is not a UUID can
    // only address a run that will never exist, whatever the caller is signed
    // in as.
    command_feedback::reset();
    workflow_run_commands::process_start(
        out,
        session,
        {"identity_workflow", R"({"steps":[{"name":"one"}]})", "--instance-id", "one"});

    BOOST_LOG_SEV(lg, debug) << "Output for a bad instance id: " << out.str();
    CHECK(out.str().find("must be a UUID") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("workflow_run_commands_start_refuses_the_nil_instance_id", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    // The nil UUID parses, so it would pass a check that only asked whether the
    // text is a UUID. It is refused because every nil run would be the same run:
    // a placeholder id would collapse unrelated workflows into one.
    command_feedback::reset();
    workflow_run_commands::process_start(out,
                                         session,
                                         {"identity_workflow",
                                          R"({"steps":[{"name":"one"}]})",
                                          "--instance-id",
                                          "00000000-0000-0000-0000-000000000000"});

    BOOST_LOG_SEV(lg, debug) << "Output for the nil instance id: " << out.str();
    CHECK(out.str().find("nil UUID") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("workflow_run_commands_reads_the_child_the_last_step_hands_on", tags) {
    auto lg(make_logger(test_suite));

    const auto handoff = workflow_run_commands::handoff_of_result(result_holding(handoff_result));

    REQUIRE(handoff.has_value());
    BOOST_LOG_SEV(lg, debug) << "Handoff: child " << handoff->instance_id << " in tenant "
                             << handoff->tenant_id;
    CHECK(handoff->instance_id == "3b891255-54ad-4eb0-9b3a-79f1010e61ab");
    CHECK(handoff->tenant_id == "0d2b1c44-6f6a-4f0e-9d33-71d4bb0c1f21");
}

TEST_CASE("workflow_run_commands_treats_a_step_that_names_no_child_as_nothing_handed_on", tags) {
    auto lg(make_logger(test_suite));

    // A result that names an empty child and one that records nothing at all
    // both mean the run is the whole job. Many steps finish without a result,
    // so calling either of them a failure would fail runs that did their work.
    for (const auto& json : {no_handoff_result, std::string{}}) {
        const auto handoff = workflow_run_commands::handoff_of_result(result_holding(json));

        REQUIRE(handoff.has_value());
        BOOST_LOG_SEV(lg, debug) << "Result '" << json << "' names child '"
                                 << handoff->instance_id << "'";
        CHECK(handoff->instance_id.empty());
        CHECK(handoff->tenant_id.empty());
    }
}

TEST_CASE("workflow_run_commands_fails_on_a_step_result_it_cannot_read", tags) {
    auto lg(make_logger(test_suite));

    // The read said the step has no result, so whether the run handed work on
    // is unknown. Reporting success there is how a half-provisioned tenant gets
    // called ready.
    auto missing = get_step_result_response{.found = false};
    const auto unreadable = workflow_run_commands::handoff_of_result(missing);

    BOOST_LOG_SEV(lg, debug) << "Error for a result the server does not hold: "
                             << unreadable.error();
    CHECK(!unreadable.has_value());
    CHECK(!unreadable.error().empty());
}

TEST_CASE("workflow_run_commands_reads_only_the_last_completed_step", tags) {
    auto lg(make_logger(test_suite));

    auto steps = std::vector<workflow_step_summary>{finished_step(0, "system_provision"),
                                                   finished_step(1, "start_tenant_setup")};
    steps[1].status = "completed_with_warnings";

    const auto* last = workflow_run_commands::last_completed_step(steps);

    REQUIRE(last != nullptr);
    BOOST_LOG_SEV(lg, debug) << "Last completed step: " << last->name << " at " << last->step_index;
    CHECK(last->step_index == 1);
    CHECK(last->name == "start_tenant_setup");
}

TEST_CASE("workflow_run_commands_finds_no_handoff_step_in_a_run_that_completed_none", tags) {
    auto lg(make_logger(test_suite));

    auto steps = std::vector<workflow_step_summary>{finished_step(0, "one"), finished_step(1, "two")};
    steps[0].status = "in_progress";
    steps[1].status = "failed";

    const auto* last = workflow_run_commands::last_completed_step(steps);

    BOOST_LOG_SEV(lg, debug) << "Last completed step: " << (last ? last->name : "none");
    CHECK(last == nullptr);
}

TEST_CASE("workflow_run_commands_waits_for_every_step_the_run_planned", tags) {
    auto lg(make_logger(test_suite));

    // The plan says two steps, but only the first exists yet. Calling that run
    // finished is what reports a tenant ready before the step that starts its
    // setup has run at all.
    get_workflow_steps_response reply{.success = true, .step_count = 2};
    reply.steps.push_back(finished_step(0, "system_provision"));

    BOOST_LOG_SEV(lg, debug) << "Complete with " << reply.steps.size() << " of "
                             << reply.step_count << " step(s): "
                             << workflow_run_commands::run_completed(reply, 0);
    CHECK(!workflow_run_commands::run_completed(reply, 0));
    CHECK(workflow_run_commands::steps_expected(reply, 0) == 2);

    reply.steps.push_back(finished_step(1, "start_tenant_setup"));
    CHECK(workflow_run_commands::run_completed(reply, 0));
}

TEST_CASE("workflow_run_commands_counts_a_step_that_warned_as_complete", tags) {
    auto lg(make_logger(test_suite));

    auto step = finished_step(0, "import_lei_hierarchy");
    step.status = "completed_with_warnings";

    BOOST_LOG_SEV(lg, debug) << "Status " << step.status << " counts as complete: "
                             << workflow_run_commands::is_step_complete(step);
    CHECK(workflow_run_commands::is_step_complete(step));
}

TEST_CASE("workflow_run_commands_follows_a_child_only_in_a_tenant_it_can_read", tags) {
    auto lg(make_logger(test_suite));

    const std::string acme("3a8907d7-ba47-4b70-993c-459a942cab45");
    const std::string new_tenant("0d2b1c44-6f6a-4f0e-9d33-71d4bb0c1f21");
    const std::string system(ores::utility::uuid::max_uuid_str);

    BOOST_LOG_SEV(lg, debug) << "acme may read its own run: "
                             << workflow_run_commands::can_read_run_of(acme, acme);
    CHECK(workflow_run_commands::can_read_run_of(acme, acme));
    CHECK(workflow_run_commands::can_read_run_of(system, new_tenant));
    CHECK(!workflow_run_commands::can_read_run_of(acme, new_tenant));
}

TEST_CASE("workflow_run_commands_fails_when_it_cannot_read_the_run", tags) {
    auto lg(make_logger(test_suite));

    // A session with no connection stands in for any read the session cannot
    // make. The command must report the failure rather than call the run
    // finished, because the handoff it never read may still be running.
    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    const auto result = workflow_run_commands::follow_child_instance(
        out,
        session,
        "3b891255-54ad-4eb0-9b3a-79f1010e61ab",
        std::chrono::seconds(1));

    BOOST_LOG_SEV(lg, debug) << "Output for an unreadable run: " << out.str();
    CHECK(result == workflow_run_commands::follow_result::failed);
    CHECK(command_feedback::failed());
    CHECK(out.str().find("Could not read the steps") != std::string::npos);
}

TEST_CASE("workflow_run_commands_suppresses_only_the_first_refused_poll", tags) {
    auto lg(make_logger(test_suite));

    // The engine answers the start before it creates the instance row, so the
    // first refusal is expected and the budget absorbs it. Every later failure
    // is printed, and a transport error is never that race.
    BOOST_LOG_SEV(lg, debug) << "First refusal reported: "
                             << workflow_run_commands::report_poll_failure(1, true);
    CHECK(!workflow_run_commands::report_poll_failure(1, true));
    CHECK(workflow_run_commands::report_poll_failure(2, true));
    CHECK(workflow_run_commands::report_poll_failure(5, true));
    CHECK(workflow_run_commands::report_poll_failure(1, false));
}
