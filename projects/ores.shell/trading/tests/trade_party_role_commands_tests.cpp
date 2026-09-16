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
#include "ores.shell/app/commands/trading/trade_party_role_commands.hpp"
#include "ores.shell/app/pagination_context.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::command_feedback;
using ores::shell::app::pagination_context;
using ores::shell::app::commands::trade_party_role_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.trading.tests");
const std::string tags("[commands]");

const std::string valid_party_id("3f2504e0-4f89-11d3-9a0c-0305e82c3301");

void log_in(nats_client& session, const std::string& party_id) {
    nats_client::login_info info;
    info.username = "tester";
    info.jwt = "token";
    info.default_party_id = party_id;
    session.set_auth(std::move(info));
}

// The positionals the add verb reads: one per user-supplied column, then the
// change reason and the change commentary.
std::vector<std::string> add_tokens() {
    return {"3f2504e0-4f89-11d3-9a0c-0305e82c3301",
            "3f2504e0-4f89-11d3-9a0c-0305e82c3301",
            "sample",
            "reason",
            "commentary"};
}

}

TEST_CASE("trade_party_role_commands_register_commands_registers_the_pagination_callback", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;
    pagination_context pagination;

    trade_party_role_commands::register_commands(root_menu, session, pagination);

    BOOST_LOG_SEV(lg, debug) << "Looking up the pagination callback for trade_party_roles.";
    CHECK(pagination.get_list_callback("trade_party_roles") != nullptr);
}

TEST_CASE("trade_party_role_commands_process_add_requires_a_logged_in_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    trade_party_role_commands::process_add_trade_party_role(out, session, add_tokens());

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out session: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("trade_party_role_commands_process_add_reports_the_expected_argument_count", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session, valid_party_id);
    std::ostringstream out;

    command_feedback::reset();
    trade_party_role_commands::process_add_trade_party_role(out, session, {"sample"});

    BOOST_LOG_SEV(lg, debug) << "Output for a short argument list: " << out.str();
    CHECK(out.str().find("Expected 5 arguments, got 1.") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("trade_party_role_commands_process_add_names_the_field_of_a_malformed_token", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session, valid_party_id);
    auto tokens = add_tokens();
    tokens[0] = "not-a-value";
    std::ostringstream out;

    command_feedback::reset();
    trade_party_role_commands::process_add_trade_party_role(out, session, tokens);

    BOOST_LOG_SEV(lg, debug) << "Output for a malformed trade_id: " << out.str();
    CHECK(out.str().find("Invalid value for trade_id") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("trade_party_role_commands_process_add_sends_a_valid_token_vector_to_the_server", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    log_in(session, valid_party_id);
    std::ostringstream out;

    command_feedback::reset();
    trade_party_role_commands::process_add_trade_party_role(out, session, add_tokens());

    BOOST_LOG_SEV(lg, debug) << "Output for a valid token vector: " << out.str();
    CHECK(out.str().find("Not connected to NATS") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("trade_party_role_commands_process_delete_requires_a_logged_in_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    trade_party_role_commands::process_delete_trade_party_role(out, session, "missing-key");

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out delete: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("trade_party_role_commands_process_history_requires_a_logged_in_session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    trade_party_role_commands::process_get_trade_party_role_history(out, session, {"missing-key"});

    BOOST_LOG_SEV(lg, debug) << "Output for a signed-out history request: " << out.str();
    CHECK(out.str().find("You must be logged in") != std::string::npos);
    CHECK(command_feedback::failed());
}

TEST_CASE("trade_party_role_commands_process_history_rejects_a_version_without_diff", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    command_feedback::reset();
    trade_party_role_commands::process_get_trade_party_role_history(
        out, session, {"missing-key", "--version", "2"});

    BOOST_LOG_SEV(lg, debug) << "Output for a version with no diff: " << out.str();
    CHECK(out.str().find("--version is only supported together with --diff.") != std::string::npos);
    CHECK(command_feedback::failed());
}
