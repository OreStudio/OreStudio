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
#include "test_suite.hpp"
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

/// A step that finished, at @p index.
workflow_step_summary finished_step(int index, std::string name) {
    return workflow_step_summary{.id = "step-" + std::to_string(index),
                                 .name = std::move(name),
                                 .status = "completed",
                                 .step_index = index};
}

/// A step result the server holds, decoded from @p json.
get_step_result_response result_holding(std::string json) {
    return get_step_result_response{.found = true, .result_json = std::move(json), .success = true};
}

const std::string handoff_result =
    R"({"kind":"start_tenant_setup","tenant_id":"0d2b1c44-6f6a-4f0e-9d33-71d4bb0c1f21",)"
    R"("child_instance_id":"3b891255-54ad-4eb0-9b3a-79f1010e61ab"})";

/// The same shape, for a step that hands nothing on.
const std::string no_handoff_result =
    R"({"kind":"system_provision","tenant_id":"","child_instance_id":"","bundles":["system_core"]})";

}

TEST_CASE("workflow_run_commands_registers_the_wait_verb", "[commands]") {
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

TEST_CASE("workflow_run_commands_start_requires_a_session", "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    workflow_run_commands::process_start(
        out, session, {"identity_workflow", R"({"steps":[{"name":"one"}]})"});

    BOOST_LOG_SEV(lg, info) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("workflow_run_commands_start_usage_names_the_instance_id_flag", "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    workflow_run_commands::process_start(out, session, {"identity_workflow"});

    BOOST_LOG_SEV(lg, info) << "Usage output: " << out.str();
    CHECK(out.str().find("--instance-id") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("workflow_run_commands_start_refuses_an_instance_id_that_is_not_a_uuid", "[commands]") {
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

    BOOST_LOG_SEV(lg, info) << "Output for a bad instance id: " << out.str();
    CHECK(out.str().find("must be a UUID") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("workflow_run_commands_start_refuses_the_nil_instance_id", "[commands]") {
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

    BOOST_LOG_SEV(lg, info) << "Output for the nil instance id: " << out.str();
    CHECK(out.str().find("nil UUID") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("workflow_run_commands_reads_the_child_the_last_step_hands_on", "[commands]") {
    auto lg(make_logger(test_suite));

    const auto handoff = workflow_run_commands::handoff_of_result(result_holding(handoff_result));

    REQUIRE(handoff.has_value());
    BOOST_LOG_SEV(lg, info) << "Handoff: child " << handoff->instance_id << " in tenant "
                            << handoff->tenant_id;
    CHECK(handoff->instance_id == "3b891255-54ad-4eb0-9b3a-79f1010e61ab");
    CHECK(handoff->tenant_id == "0d2b1c44-6f6a-4f0e-9d33-71d4bb0c1f21");
}

TEST_CASE("workflow_run_commands_treats_a_step_that_names_no_child_as_nothing_handed_on",
          "[commands]") {
    auto lg(make_logger(test_suite));

    // A result that names an empty child and one that records nothing at all
    // both mean the run is the whole job. Many steps finish without a result,
    // so calling either of them a failure would fail runs that did their work.
    for (const auto& json : {no_handoff_result, std::string{}}) {
        const auto handoff = workflow_run_commands::handoff_of_result(result_holding(json));

        REQUIRE(handoff.has_value());
        BOOST_LOG_SEV(lg, info) << "Result '" << json << "' names child '" << handoff->instance_id
                                << "'";
        CHECK(handoff->instance_id.empty());
        CHECK(handoff->tenant_id.empty());
    }
}

TEST_CASE("workflow_run_commands_fails_on_a_step_result_it_cannot_read", "[commands]") {
    auto lg(make_logger(test_suite));

    // The read said the step has no result, so whether the run handed work on
    // is unknown. Reporting success there is how a half-provisioned tenant gets
    // called ready.
    auto missing = get_step_result_response{.found = false};
    const auto unreadable = workflow_run_commands::handoff_of_result(missing);

    BOOST_LOG_SEV(lg, info) << "Error for a result the server does not hold: "
                            << unreadable.error();
    CHECK(!unreadable.has_value());
    CHECK(!unreadable.error().empty());
}

TEST_CASE("workflow_run_commands_reads_only_the_last_completed_step", "[commands]") {
    auto lg(make_logger(test_suite));

    auto steps = std::vector<workflow_step_summary>{finished_step(0, "system_provision"),
                                                    finished_step(1, "start_tenant_setup")};
    steps[1].status = "completed_with_warnings";

    const auto* last = workflow_run_commands::last_completed_step(steps);

    REQUIRE(last != nullptr);
    BOOST_LOG_SEV(lg, info) << "Last completed step: " << last->name << " at " << last->step_index;
    CHECK(last->step_index == 1);
    CHECK(last->name == "start_tenant_setup");
}

TEST_CASE("workflow_run_commands_finds_no_handoff_step_in_a_run_that_completed_none",
          "[commands]") {
    auto lg(make_logger(test_suite));

    auto steps =
        std::vector<workflow_step_summary>{finished_step(0, "one"), finished_step(1, "two")};
    steps[0].status = "in_progress";
    steps[1].status = "failed";

    const auto* last = workflow_run_commands::last_completed_step(steps);

    BOOST_LOG_SEV(lg, info) << "Last completed step: " << (last ? last->name : "none");
    CHECK(last == nullptr);
}

TEST_CASE("workflow_run_commands_waits_for_every_step_the_run_planned", "[commands]") {
    auto lg(make_logger(test_suite));

    // The plan says two steps, but only the first exists yet. Calling that run
    // finished is what reports a tenant ready before the step that starts its
    // setup has run at all.
    get_workflow_steps_response reply{.success = true, .step_count = 2};
    reply.steps.push_back(finished_step(0, "system_provision"));

    BOOST_LOG_SEV(lg, info) << "Complete with " << reply.steps.size() << " of " << reply.step_count
                            << " step(s): " << workflow_run_commands::run_completed(reply, 0);
    CHECK(!workflow_run_commands::run_completed(reply, 0));
    CHECK(workflow_run_commands::steps_expected(reply, 0) == 2);

    reply.steps.push_back(finished_step(1, "start_tenant_setup"));
    CHECK(workflow_run_commands::run_completed(reply, 0));
}

TEST_CASE("workflow_run_commands_counts_a_step_that_warned_as_complete", "[commands]") {
    auto lg(make_logger(test_suite));

    auto step = finished_step(0, "import_lei_hierarchy");
    step.status = "completed_with_warnings";

    BOOST_LOG_SEV(lg, info) << "Status " << step.status << " counts as complete: "
                            << workflow_run_commands::is_step_complete(step);
    CHECK(workflow_run_commands::is_step_complete(step));
}

TEST_CASE("workflow_run_commands_completes_a_run_whose_status_says_completed", "[commands]") {
    auto lg(make_logger(test_suite));

    // A run the caller may not see comes back as a success with an empty step
    // list. Counting rows would spin to the timeout and report a failure that
    // never happened, so the instance's own status settles the question.
    get_workflow_steps_response reply{.success = true, .status = "completed", .step_count = 2};

    BOOST_LOG_SEV(lg, info) << "A completed run with " << reply.steps.size()
                            << " visible step(s) counts as complete: "
                            << workflow_run_commands::run_completed(reply, 0);
    CHECK(workflow_run_commands::run_completed(reply, 0));
}

TEST_CASE("workflow_run_commands_does_not_complete_a_running_run_with_no_steps", "[commands]") {
    auto lg(make_logger(test_suite));

    get_workflow_steps_response reply{.success = true, .status = "in_progress", .step_count = 9};

    BOOST_LOG_SEV(lg, info) << "A running run with " << reply.steps.size()
                            << " visible step(s) counts as complete: "
                            << workflow_run_commands::run_completed(reply, 0);
    CHECK(!workflow_run_commands::run_completed(reply, 0));
}

TEST_CASE("workflow_run_commands_follows_a_child_only_in_a_tenant_it_can_read", "[commands]") {
    auto lg(make_logger(test_suite));

    const std::string acme("3a8907d7-ba47-4b70-993c-459a942cab45");
    const std::string new_tenant("0d2b1c44-6f6a-4f0e-9d33-71d4bb0c1f21");
    const std::string system(ores::utility::uuid::max_uuid_str);

    BOOST_LOG_SEV(lg, info) << "acme may read its own run: "
                            << workflow_run_commands::can_read_run_of(acme, acme);
    CHECK(workflow_run_commands::can_read_run_of(acme, acme));
    CHECK(workflow_run_commands::can_read_run_of(system, new_tenant));
    CHECK(!workflow_run_commands::can_read_run_of(acme, new_tenant));
}

TEST_CASE("workflow_run_commands_refuses_a_run_whose_tenant_is_unknown", "[commands]") {
    auto lg(make_logger(test_suite));

    // A tenant the handoff does not name, or names as something that is not a
    // tenant id, is unknown. An unknown tenant must not grant access, and the
    // system tenant is no exception: it reads across tenants, not into a field
    // that says nothing.
    const std::string acme("3a8907d7-ba47-4b70-993c-459a942cab45");
    const std::string system(ores::utility::uuid::max_uuid_str);

    BOOST_LOG_SEV(lg, info) << "acme may read an empty tenant: "
                            << workflow_run_commands::can_read_run_of(acme, "");
    CHECK(!workflow_run_commands::can_read_run_of(acme, ""));
    CHECK(!workflow_run_commands::can_read_run_of(system, ""));
    CHECK(!workflow_run_commands::can_read_run_of(acme, "not-a-tenant-id"));
    CHECK(!workflow_run_commands::can_read_run_of(system, "not-a-tenant-id"));
}

TEST_CASE("workflow_run_commands_follows_a_handoff_this_session_can_read", "[commands]") {
    auto lg(make_logger(test_suite));

    const std::string acme("3a8907d7-ba47-4b70-993c-459a942cab45");
    const std::string new_tenant("0d2b1c44-6f6a-4f0e-9d33-71d4bb0c1f21");
    const std::string system(ores::utility::uuid::max_uuid_str);

    workflow_run_commands::handoff work{.instance_id = "3b891255-54ad-4eb0-9b3a-79f1010e61ab",
                                        .tenant_id = new_tenant};

    // The system tenant follows the child wherever it runs; a tenant
    // administrator follows only a child in its own tenant.
    BOOST_LOG_SEV(lg, info) << "The system session follows a child in " << work.tenant_id << ": "
                            << workflow_run_commands::is_handoff_followed(system, work);
    CHECK(workflow_run_commands::is_handoff_followed(system, work));
    CHECK(!workflow_run_commands::is_handoff_followed(acme, work));

    work.tenant_id = acme;
    CHECK(workflow_run_commands::is_handoff_followed(acme, work));

    work.tenant_id.clear();
    CHECK(!workflow_run_commands::is_handoff_followed(system, work));

    work.tenant_id = "not-a-tenant-id";
    CHECK(!workflow_run_commands::is_handoff_followed(system, work));

    work.tenant_id = acme;
    work.instance_id.clear();
    CHECK(!workflow_run_commands::is_handoff_followed(acme, work));
}

TEST_CASE("workflow_run_commands_hands_the_child_only_what_the_first_run_left", "[commands]") {
    auto lg(make_logger(test_suite));

    // One budget covers the command, so the time the first run took is taken
    // from what the child may spend, and never turns negative.
    BOOST_LOG_SEV(lg, info) << "Budget left after 120s of 3600s: "
                            << workflow_run_commands::remaining_budget(std::chrono::seconds(3600),
                                                                       std::chrono::seconds(120))
                                   .count()
                            << "s";
    CHECK(workflow_run_commands::remaining_budget(
              std::chrono::seconds(3600), std::chrono::seconds(120)) == std::chrono::seconds(3480));
    CHECK(workflow_run_commands::remaining_budget(
              std::chrono::seconds(3600), std::chrono::seconds(3600)) == std::chrono::seconds(0));
    CHECK(workflow_run_commands::remaining_budget(
              std::chrono::seconds(3600), std::chrono::seconds(7200)) == std::chrono::seconds(0));
}

TEST_CASE("workflow_run_commands_fails_when_it_cannot_read_the_run", "[commands]") {
    auto lg(make_logger(test_suite));

    // A session with no connection stands in for any read the session cannot
    // make. The command must report the failure rather than call the run
    // finished, because the handoff it never read may still be running.
    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    const auto result = workflow_run_commands::follow_child_instance(
        out, session, "3b891255-54ad-4eb0-9b3a-79f1010e61ab", std::chrono::seconds(1));

    BOOST_LOG_SEV(lg, info) << "Output for an unreadable run: " << out.str();
    CHECK(result == workflow_run_commands::follow_result::failed);
    CHECK(command_feedback::failed());
    CHECK(out.str().find("Could not read the steps") != std::string::npos);
}

TEST_CASE("workflow_run_commands_suppresses_only_the_first_refused_poll", "[commands]") {
    auto lg(make_logger(test_suite));

    // The engine answers the start before it creates the instance row, so the
    // first refusal is expected and the budget absorbs it. Every later failure
    // is printed, and a transport error is never that race.
    BOOST_LOG_SEV(lg, info) << "First refusal reported: "
                            << workflow_run_commands::report_poll_failure(1, true);
    CHECK(!workflow_run_commands::report_poll_failure(1, true));
    CHECK(workflow_run_commands::report_poll_failure(2, true));
    CHECK(workflow_run_commands::report_poll_failure(5, true));
    CHECK(workflow_run_commands::report_poll_failure(1, false));
}
