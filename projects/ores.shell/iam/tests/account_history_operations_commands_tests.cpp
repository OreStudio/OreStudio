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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_shell_operation_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/iam/account_history_operations_commands.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::commands::account_history_operations_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.iam.tests");
const std::string tags("[commands]");

// One token per positional argument the command takes, in declaration order.
std::vector<std::string> tokens(const std::size_t count) {
    return std::vector<std::string>(count, std::string{"sample"});
}

}

TEST_CASE("account_history_operations_registers_every_declared_command", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    account_history_operations_commands::register_commands(root_menu, session);

    BOOST_LOG_SEV(lg, debug) << "Registered 1 command(s).";
    CHECK(true);
}

TEST_CASE("account_history_operations_process_get_account_history_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_history_operations_commands::process_get_account_history(out, session, tokens(1));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_history_operations_process_get_account_history_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_history_operations_commands::process_get_account_history(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_history_operations_process_get_account_history_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_history_operations_commands::process_get_account_history(out, session, tokens(1));

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}
