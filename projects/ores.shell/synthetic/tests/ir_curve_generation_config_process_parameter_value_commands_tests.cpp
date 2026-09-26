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
 * Template: cpp_shell_command_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/synthetic/ir_curve_generation_config_process_parameter_value_commands.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::commands::ir_curve_generation_config_process_parameter_value_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.synthetic.tests");
const std::string tags("[commands]");

const std::string valid_party_id("3f2504e0-4f89-11d3-9a0c-0305e82c3301");

void log_in(nats_client& session) {
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    info.default_party_id = valid_party_id;
    session.set_auth(std::move(info));
}

// One token per positional the command takes. A value the command parses is
// not a value the guard reaches, so any token does to prove the guard.
std::vector<std::string> tokens(const std::size_t count) {
    return std::vector<std::string>(count, std::string{"sample"});
}

}

TEST_CASE(
    "ir_curve_generation_config_process_parameter_value_commands_registers_every_derived_verb",
    tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    ir_curve_generation_config_process_parameter_value_commands::register_commands(root_menu,
                                                                                   session);

    // The menu's completion list is the only public view of its children, so
    // a verb that is missing from it was never registered.
    const auto completions =
        root_menu.GetCompletions("ir_curve_generation_config_process_parameter_values ");
    for (const auto& verb : {
             std::string{"ir_curve_generation_config_process_parameter_values list"},
             std::string{"ir_curve_generation_config_process_parameter_values get"},
             std::string{"ir_curve_generation_config_process_parameter_values get-many"},
             std::string{"ir_curve_generation_config_process_parameter_values add"},
             std::string{"ir_curve_generation_config_process_parameter_values set"},
             std::string{"ir_curve_generation_config_process_parameter_values put-many"},
             std::string{"ir_curve_generation_config_process_parameter_values delete"},
             std::string{"ir_curve_generation_config_process_parameter_values delete-many"},
             std::string{"ir_curve_generation_config_process_parameter_values versions"},
             std::string{"ir_curve_generation_config_process_parameter_values version"},
         })
        CHECK(std::find(completions.begin(), completions.end(), verb) != completions.end());

    BOOST_LOG_SEV(lg, debug) << "Registered 10 command(s).";
}

TEST_CASE(
    "ir_curve_generation_config_process_parameter_value_commands_process_list_requires_a_session",
    tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_list(
        out, session, tokens(0));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE(
    "ir_curve_generation_config_process_parameter_value_commands_process_get_requires_a_session",
    tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_get(
        out, session, tokens(1));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_get_reports_the_"
          "expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_get(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    // The arity guard names both the count it expects and the count it
    // received. The expected count is shape-specific in the command (a write
    // also reads its intent), so the case pins the wording and the received
    // count rather than restating that arithmetic.
    CHECK(out.str().find("Expected ") != std::string::npos);
    CHECK(out.str().find("got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_get_many_requires_a_"
          "session",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_get_many(
        out, session, tokens(1));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE(
    "ir_curve_generation_config_process_parameter_value_commands_process_add_requires_a_session",
    tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_add(
        out, session, tokens(4));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_add_reports_the_"
          "expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_add(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    // The arity guard names both the count it expects and the count it
    // received. The expected count is shape-specific in the command (a write
    // also reads its intent), so the case pins the wording and the received
    // count rather than restating that arithmetic.
    CHECK(out.str().find("Expected ") != std::string::npos);
    CHECK(out.str().find("got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE(
    "ir_curve_generation_config_process_parameter_value_commands_process_set_requires_a_session",
    tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_set(
        out, session, tokens(4));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_set_reports_the_"
          "expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_set(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    // The arity guard names both the count it expects and the count it
    // received. The expected count is shape-specific in the command (a write
    // also reads its intent), so the case pins the wording and the received
    // count rather than restating that arithmetic.
    CHECK(out.str().find("Expected ") != std::string::npos);
    CHECK(out.str().find("got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_put_many_requires_a_"
          "session",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_put_many(
        out, session, tokens(4));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE(
    "ir_curve_generation_config_process_parameter_value_commands_process_delete_requires_a_session",
    tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_delete(
        out, session, tokens(1));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_delete_reports_the_"
          "expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_delete(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    // The arity guard names both the count it expects and the count it
    // received. The expected count is shape-specific in the command (a write
    // also reads its intent), so the case pins the wording and the received
    // count rather than restating that arithmetic.
    CHECK(out.str().find("Expected ") != std::string::npos);
    CHECK(out.str().find("got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_delete_many_"
          "requires_a_session",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_delete_many(
        out, session, tokens(1));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_versions_requires_a_"
          "session",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_versions(
        out, session, tokens(1));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_versions_reports_"
          "the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_versions(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    // The arity guard names both the count it expects and the count it
    // received. The expected count is shape-specific in the command (a write
    // also reads its intent), so the case pins the wording and the received
    // count rather than restating that arithmetic.
    CHECK(out.str().find("Expected ") != std::string::npos);
    CHECK(out.str().find("got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_version_requires_a_"
          "session",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_version(
        out, session, tokens(1));

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("ir_curve_generation_config_process_parameter_value_commands_process_version_reports_the_"
          "expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session);
    std::ostringstream out;

    command_feedback::reset();
    ir_curve_generation_config_process_parameter_value_commands::process_version(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    // The arity guard names both the count it expects and the count it
    // received. The expected count is shape-specific in the command (a write
    // also reads its intent), so the case pins the wording and the received
    // count rather than restating that arithmetic.
    CHECK(out.str().find("Expected ") != std::string::npos);
    CHECK(out.str().find("got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}
