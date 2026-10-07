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
#include "ores.shell/app/commands/reporting/report_operations_operations_commands.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::commands::report_operations_operations_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.reporting.tests");
const std::string tags("[commands]");

// One token per positional argument the command takes, in declaration order,
// spelled for the type the command parses it with.

}

TEST_CASE("report_operations_operations_registers_every_declared_command", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    report_operations_operations_commands::register_commands(root_menu, session);

    // The menu's completion list is the only public view of its children, so a
    // command that is missing from it was never registered.
    const auto completions = root_menu.GetCompletions("report_operations ");
    for (const auto& verb : {
             std::string{"report_operations trigger-report-instance"},
             std::string{"report_operations schedule-report-definitions"},
             std::string{"report_operations unschedule-report-definitions"},
             std::string{"report_operations gather-trades"},
             std::string{"report_operations gather-market-data"},
             std::string{"report_operations assemble-bundle"},
             std::string{"report_operations prepare-ore-package"},
             std::string{"report_operations submit-compute"},
             std::string{"report_operations collect-compute-results"},
             std::string{"report_operations finalise-report"},
             std::string{"report_operations fail-report"},
             std::string{"report_operations resolve-prepared-input"},
             std::string{"report_operations ignore-compute-results"},
         })
        CHECK(std::find(completions.begin(), completions.end(), verb) != completions.end());

    BOOST_LOG_SEV(lg, debug) << "Registered 13 command(s).";
}

TEST_CASE("report_operations_operations_process_trigger_report_instance_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_trigger_report_instance(
        out,
        session,
        std::vector<std::string>{
            "00000000-0000-0000-0000-000000000001",
            "00000000-0000-0000-0000-000000000001",
        });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_trigger_report_instance_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_trigger_report_instance(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 2 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_trigger_report_instance_reaches_the_transport",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_trigger_report_instance(
        out,
        session,
        std::vector<std::string>{
            "00000000-0000-0000-0000-000000000001",
            "00000000-0000-0000-0000-000000000001",
        });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_schedule_report_definitions_requires_a_session",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_schedule_report_definitions(
        out,
        session,
        std::vector<std::string>{
            "sample",
        });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE(
    "report_operations_operations_process_schedule_report_definitions_reports_the_expected_count",
    tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_schedule_report_definitions(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_schedule_report_definitions_reaches_the_transport",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_schedule_report_definitions(
        out,
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

TEST_CASE("report_operations_operations_process_unschedule_report_definitions_requires_a_session",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_unschedule_report_definitions(
        out,
        session,
        std::vector<std::string>{
            "sample",
        });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE(
    "report_operations_operations_process_unschedule_report_definitions_reports_the_expected_count",
    tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_unschedule_report_definitions(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 1 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE(
    "report_operations_operations_process_unschedule_report_definitions_reaches_the_transport",
    tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_unschedule_report_definitions(
        out,
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

TEST_CASE("report_operations_operations_process_gather_trades_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_gather_trades(out,
                                                                 session,
                                                                 std::vector<std::string>{
                                                                     "sample",
                                                                     "sample",
                                                                     "sample",
                                                                     "sample",
                                                                 });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_gather_trades_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_gather_trades(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 4 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_gather_trades_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_gather_trades(out,
                                                                 session,
                                                                 std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_gather_market_data_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_gather_market_data(out,
                                                                      session,
                                                                      std::vector<std::string>{
                                                                          "sample",
                                                                          "sample",
                                                                          "sample",
                                                                          "sample",
                                                                      });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_gather_market_data_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_gather_market_data(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 4 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_gather_market_data_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_gather_market_data(out,
                                                                      session,
                                                                      std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_assemble_bundle_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_assemble_bundle(out,
                                                                   session,
                                                                   std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_assemble_bundle_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_assemble_bundle(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 7 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_assemble_bundle_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_assemble_bundle(out,
                                                                   session,
                                                                   std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_prepare_ore_package_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_prepare_ore_package(out,
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
                                                                           "sample",
                                                                       });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_prepare_ore_package_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_prepare_ore_package(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 10 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_prepare_ore_package_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_prepare_ore_package(out,
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
                                                                           "sample",
                                                                       });

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    // Whether the command carries a token or not, everything ahead of the
    // transport is satisfied, which is what the absence of a connection proves.
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_submit_compute_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_submit_compute(out,
                                                                  session,
                                                                  std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_submit_compute_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_submit_compute(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 6 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_submit_compute_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_submit_compute(out,
                                                                  session,
                                                                  std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_collect_compute_results_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_collect_compute_results(out,
                                                                           session,
                                                                           std::vector<std::string>{
                                                                               "sample",
                                                                               "sample",
                                                                               "sample",
                                                                               "sample",
                                                                           });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_collect_compute_results_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_collect_compute_results(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 4 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_collect_compute_results_reaches_the_transport",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_collect_compute_results(out,
                                                                           session,
                                                                           std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_finalise_report_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_finalise_report(out,
                                                                   session,
                                                                   std::vector<std::string>{
                                                                       "sample",
                                                                       "sample",
                                                                       "sample",
                                                                   });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_finalise_report_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_finalise_report(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 3 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_finalise_report_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_finalise_report(out,
                                                                   session,
                                                                   std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_fail_report_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_fail_report(out,
                                                               session,
                                                               std::vector<std::string>{
                                                                   "sample",
                                                                   "sample",
                                                                   "sample",
                                                                   "sample",
                                                               });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_fail_report_reports_the_expected_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_fail_report(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 4 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_fail_report_reaches_the_transport", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_fail_report(out,
                                                               session,
                                                               std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_resolve_prepared_input_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_resolve_prepared_input(out,
                                                                          session,
                                                                          std::vector<std::string>{
                                                                              "sample",
                                                                              "sample",
                                                                              "sample",
                                                                              "sample",
                                                                          });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_resolve_prepared_input_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_resolve_prepared_input(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 4 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_resolve_prepared_input_reaches_the_transport",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_resolve_prepared_input(out,
                                                                          session,
                                                                          std::vector<std::string>{
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

TEST_CASE("report_operations_operations_process_ignore_compute_results_requires_a_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_ignore_compute_results(out,
                                                                          session,
                                                                          std::vector<std::string>{
                                                                              "sample",
                                                                              "sample",
                                                                              "sample",
                                                                              "sample",
                                                                          });

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_ignore_compute_results_reports_the_expected_count",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_ignore_compute_results(out, session, {});

    BOOST_LOG_SEV(lg, debug) << "Output for an empty argument list: " << out.str();
    CHECK(out.str().find("Expected 4 arguments, got 0.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("report_operations_operations_process_ignore_compute_results_reaches_the_transport",
          tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    session.set_auth(std::move(info));
    std::ostringstream out;

    command_feedback::reset();
    report_operations_operations_commands::process_ignore_compute_results(out,
                                                                          session,
                                                                          std::vector<std::string>{
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
