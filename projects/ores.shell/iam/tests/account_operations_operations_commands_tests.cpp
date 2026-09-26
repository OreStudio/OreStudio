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
#include "ores.shell/app/commands/iam/account_operations_operations_commands.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::commands::account_operations_operations_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.iam.tests");
const std::string tags("[commands]");

// One token per positional argument the command takes, in declaration order,
// spelled for the type the command parses it with.

}

TEST_CASE("account_operations_operations_registers_every_declared_command", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    account_operations_operations_commands::register_commands(root_menu, session);

    // The menu's completion list is the only public view of its children, so a
    // command that is missing from it was never registered.
    const auto completions = root_menu.GetCompletions("account_operations ");
    for (const auto& verb : {
             std::string{"account_operations save-account"},
             std::string{"account_operations update-account"},
             std::string{"account_operations delete-account"},
             std::string{"account_operations lock-account"},
             std::string{"account_operations unlock-account"},
             std::string{"account_operations reset-password"},
             std::string{"account_operations update-my-email"},
             std::string{"account_operations set-my-default-party"},
             std::string{"account_operations select-party"},
             std::string{"account_operations switch-party"},
             std::string{"account_operations change-password"},
         })
        CHECK(std::find(completions.begin(), completions.end(), verb) != completions.end());

    BOOST_LOG_SEV(lg, debug) << "Registered 11 command(s).";
}

TEST_CASE("account_operations_operations_process_save_account_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_save_account(out,
                                                                 session,
                                                                 std::vector<std::string>{
                                                                     "sample",
                                                                     "sample",
                                                                     "sample",
                                                                     "sample",
                                                                     "sample",
                                                                 });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_save_account_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_save_account(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 5 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_save_account_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_save_account(out,
                                                                 session,
                                                                 std::vector<std::string>{
                                                                     "sample",
                                                                     "sample",
                                                                     "sample",
                                                                     "sample",
                                                                     "sample",
                                                                 });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_update_account_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_update_account(out,
                                                                   session,
                                                                   std::vector<std::string>{
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                   });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_update_account_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_update_account(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 9 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_update_account_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_update_account(out,
                                                                   session,
                                                                   std::vector<std::string>{
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                   });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_delete_account_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_delete_account(out,
                                                                   session,
                                                                   std::vector<std::string>{
                                                                       "sample",
                                                                   });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_delete_account_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_delete_account(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_delete_account_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_delete_account(out,
                                                                   session,
                                                                   std::vector<std::string>{
                                                                       "sample",
                                                                   });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_lock_account_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_lock_account(out,
                                                                 session,
                                                                 std::vector<std::string>{
                                                                     "sample",
                                                                 });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_lock_account_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_lock_account(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_lock_account_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_lock_account(out,
                                                                 session,
                                                                 std::vector<std::string>{
                                                                     "sample",
                                                                 });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_unlock_account_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_unlock_account(out,
                                                                   session,
                                                                   std::vector<std::string>{
                                                                       "sample",
                                                                   });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_unlock_account_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_unlock_account(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_unlock_account_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_unlock_account(out,
                                                                   session,
                                                                   std::vector<std::string>{
                                                                       "sample",
                                                                   });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_reset_password_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_reset_password(out,
                                                                   session,
                                                                   std::vector<std::string>{
                                                                       "sample",
                                                                       "sample",
                                                                   });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_reset_password_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_reset_password(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 2 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_reset_password_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_reset_password(out,
                                                                   session,
                                                                   std::vector<std::string>{
                                                                       "sample",
                                                                       "sample",
                                                                   });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_update_my_email_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_update_my_email(out,
                                                                    session,
                                                                    std::vector<std::string>{
                                                                        "sample",
                                                                    });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_update_my_email_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_update_my_email(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_update_my_email_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_update_my_email(out,
                                                                    session,
                                                                    std::vector<std::string>{
                                                                        "sample",
                                                                    });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_set_my_default_party_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_set_my_default_party(out,
                                                                         session,
                                                                         std::vector<std::string>{
                                                                             "sample",
                                                                         });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_set_my_default_party_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_set_my_default_party(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_set_my_default_party_reaches_the_transport",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_set_my_default_party(out,
                                                                         session,
                                                                         std::vector<std::string>{
                                                                             "sample",
                                                                         });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_select_party_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_select_party(out,
                                                                 session,
                                                                 std::vector<std::string>{
                                                                     "sample",
                                                                 });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_select_party_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_select_party(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_select_party_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_select_party(out,
                                                                 session,
                                                                 std::vector<std::string>{
                                                                     "sample",
                                                                 });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_switch_party_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_switch_party(out,
                                                                 session,
                                                                 std::vector<std::string>{
                                                                     "sample",
                                                                 });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_switch_party_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_switch_party(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_switch_party_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_switch_party(out,
                                                                 session,
                                                                 std::vector<std::string>{
                                                                     "sample",
                                                                 });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_change_password_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_change_password(out,
                                                                    session,
                                                                    std::vector<std::string>{
                                                                        "sample",
                                                                        "sample",
                                                                    });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_change_password_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_change_password(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 2 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("account_operations_operations_process_change_password_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    account_operations_operations_commands::process_change_password(out,
                                                                    session,
                                                                    std::vector<std::string>{
                                                                        "sample",
                                                                        "sample",
                                                                    });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}
