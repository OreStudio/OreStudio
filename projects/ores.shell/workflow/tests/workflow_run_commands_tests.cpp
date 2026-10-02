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
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::commands::workflow_run_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.workflow.tests");
const std::string tags("[commands]");

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
