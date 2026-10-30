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
#include "ores.shell/app/commands/inbox/notification_operations_commands.hpp"
#include "test_suite.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::commands::notification_operations_commands;
using namespace ores::logging;

namespace {

/** A session that has signed in, so the command reaches the transport. */
nats_client signed_in_session() {
    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    return session;
}

}

TEST_CASE("notification_operations_commands_registers_raise_notification", "[commands]") {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    notification_operations_commands::register_commands(root_menu, session);

    const auto completions = root_menu.GetCompletions("notification_operations ");
    CHECK(std::find(completions.begin(),
                    completions.end(),
                    "notification_operations raise-notification") != completions.end());

    BOOST_LOG_SEV(lg, debug) << "Registered 1 command(s).";
}

TEST_CASE("notification_operations_commands_raise_notification_requires_a_session",
          "[commands]") {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    notification_operations_commands::process_raise_notification(
        out,
        session,
        std::vector<std::string>{"scenario.to_run", "/development/tests/1", "__none__"});

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("notification_operations_commands_raise_notification_reports_the_expected_count",
          "[commands]") {
    auto lg(make_logger(test_suite));

    auto session = signed_in_session();
    std::ostringstream out;

    command_feedback::reset();
    notification_operations_commands::process_raise_notification(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 3 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("notification_operations_commands_raise_notification_reads_named_values",
          "[commands]") {
    auto lg(make_logger(test_suite));

    auto session = signed_in_session();
    std::ostringstream out;

    command_feedback::reset();
    notification_operations_commands::process_raise_notification(
        out,
        session,
        std::vector<std::string>{"scenario.to_run",
                                 "/development/tests/1",
                                 "8e940c9e-8aea-4c61-9e99-ec33c4abfe2c",
                                 "--argument",
                                 "scenario=a scenario with = in its title"});

    BOOST_LOG_SEV(lg, debug) << "Output for named values: " << out.str();
    // The token was read as one named value, so the command got as far as the
    // transport rather than refusing the argument.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("notification_operations_commands_raise_notification_refuses_a_bare_name",
          "[commands]") {
    auto lg(make_logger(test_suite));

    auto session = signed_in_session();
    std::ostringstream out;

    command_feedback::reset();
    notification_operations_commands::process_raise_notification(
        out,
        session,
        std::vector<std::string>{
            "scenario.to_run", "/development/tests/1", "__none__", "--argument", "scenario"});

    BOOST_LOG_SEV(lg, debug) << "Output for a token with no value: " << out.str();
    CHECK(out.str().find("Expected name=value, got 'scenario'.") != std::string::npos);
    CHECK(command_feedback::failed());
}
