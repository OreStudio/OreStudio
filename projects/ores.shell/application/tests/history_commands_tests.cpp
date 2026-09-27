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
#include "ores.shell/app/commands/history_commands.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::commands::history_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.application.tests");
const std::string tags("[commands][history]");

const std::string workflow_instance("ores.workflow.workflow_instance");
const std::string instance_id("1a529bd2-3345-41f8-b8ff-ab9334509c1e");

/**
 * @brief Every case below refuses before the transport is touched.
 *
 * None of these reaches a server: each states an argument or a session the
 * command rejects first, which is what makes them runnable in the unit suite.
 * The read itself is exercised against a live fleet, because that is the only
 * place history exists to be read.
 */
} // anonymous namespace

TEST_CASE("history_commands registers the get verb", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    history_commands::register_commands(root_menu, session);

    // The command is the only way into the generic history protocol from the
    // shell, so a get that is no longer registered leaves the family unreachable.
    const auto completions = root_menu.GetCompletions("history ");
    CHECK(std::find(completions.begin(), completions.end(), std::string{"history get"}) !=
          completions.end());
}

TEST_CASE("history_commands_get_names_the_arguments_it_needs", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    history_commands::process_get(out, session, {workflow_instance});

    BOOST_LOG_SEV(lg, debug) << "Usage output: " << out.str();
    CHECK(out.str().find("<entity_type> <entity_id>") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("history_commands_get_refuses_a_version_it_cannot_read", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    history_commands::process_get(
        out, session, {workflow_instance, instance_id, "--diff", "--version", "two"});

    BOOST_LOG_SEV(lg, debug) << "Output for a bad version: " << out.str();
    CHECK(out.str().find("Invalid --version value") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("history_commands_get_refuses_a_version_without_a_diff", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    // The listing has no version to pick: only the diff takes one. Saying so is
    // better than ignoring the flag and returning the newest version anyway.
    command_feedback::reset();
    history_commands::process_get(out, session, {workflow_instance, instance_id, "--version", "2"});

    BOOST_LOG_SEV(lg, debug) << "Output for a version without a diff: " << out.str();
    CHECK(out.str().find("only supported together with --diff") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("history_commands_get_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    history_commands::process_get(out, session, {workflow_instance, instance_id});

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}
