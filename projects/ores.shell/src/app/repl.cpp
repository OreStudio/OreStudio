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
#include "ores.shell/app/repl.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/account_parties_commands.hpp"
#include "ores.shell/app/commands/accounts_commands.hpp"
#include "ores.shell/app/commands/bundles_commands.hpp"
#include "ores.shell/app/commands/change_reason_categories_commands.hpp"
#include "ores.shell/app/commands/change_reasons_commands.hpp"
#include "ores.shell/app/commands/connection_commands.hpp"
#include "ores.shell/app/commands/countries_commands.hpp"
#include "ores.shell/app/commands/currencies_commands.hpp"
#include "ores.shell/app/commands/lei_commands.hpp"
#include "ores.shell/app/commands/marketdata_commands.hpp"
#include "ores.shell/app/commands/navigation_commands.hpp"
#include "ores.shell/app/commands/orgmode_commands.hpp"
#include "ores.shell/app/commands/parties_commands.hpp"
#include "ores.shell/app/commands/provision_commands.hpp"
#include "ores.shell/app/commands/rbac_commands.hpp"
#include "ores.shell/app/commands/reports_commands.hpp"
#include "ores.shell/app/commands/script_commands.hpp"
#include "ores.shell/app/commands/subscription_commands.hpp"
#include "ores.shell/app/commands/synthetic_commands.hpp"
#include "ores.shell/app/commands/tenants_commands.hpp"
#include "ores.shell/app/commands/variability_commands.hpp"
#include "ores.shell/app/commands/workflow_commands.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <cli/cli.h>
#include <cli/clifilesession.h>
#include <iostream>
#include <rfl/json.hpp>

namespace ores::shell::app {

using namespace ores::logging;

repl::repl(ores::nats::service::nats_client& session,
           nats::config::nats_options connection_template)
    : session_(session)
    , connection_template_(std::move(connection_template)) {
    BOOST_LOG_SEV(lg(), info) << "REPL created.";
}

void repl::run() {
    run(std::cin, std::cout);
}

void repl::run(std::istream& in, std::ostream& out) {
    BOOST_LOG_SEV(lg(), info) << "REPL session started.";
    display_welcome(out);
    auto cli_instance = setup_menus();
    cli::CliFileSession session(*cli_instance, in, out);
    active_session_ = &session;
    session.Start();
    active_session_ = nullptr;
    cleanup();
    BOOST_LOG_SEV(lg(), info) << "REPL session ended.";
}

std::unique_ptr<cli::Cli> repl::setup_menus() {
    auto root = std::make_unique<cli::Menu>("ores-shell");

    using namespace commands;
    change_reason_categories_commands::register_commands(*root, session_);
    change_reasons_commands::register_commands(*root, session_, pagination_);
    connection_commands::register_commands(*root, session_, connection_template_);
    countries_commands::register_commands(*root, session_, pagination_);
    currencies_commands::register_commands(*root, session_, pagination_);
    accounts_commands::register_commands(*root, session_, pagination_);
    variability_commands::register_commands(*root, session_);
    subscription_commands::register_commands(*root, session_);
    rbac_commands::register_commands(*root, session_, pagination_);
    tenants_commands::register_commands(*root, session_, pagination_);
    navigation_commands::register_commands(*root, pagination_);
    orgmode_commands::register_commands(*root);
    script_commands::register_commands(*root, active_session_);
    bundles_commands::register_commands(*root, session_);
    workflow_commands::register_commands(*root, session_);
    lei_commands::register_commands(*root, session_);
    marketdata_commands::register_commands(*root, session_);
    synthetic_commands::register_commands(*root, session_);
    parties_commands::register_commands(*root, session_);
    account_parties_commands::register_commands(*root, session_);
    reports_commands::register_commands(*root, session_);
    provision_commands::register_commands(*root, session_);

    auto cli_instance = std::make_unique<cli::Cli>(std::move(root));
    cli_instance->ExitAction([](auto& out) { out << "Bye!" << std::endl; });

    // Route cli-level errors through command_feedback so scripts abort
    // on unknown commands and uncaught exceptions too.
    cli_instance->WrongCommandHandler([](std::ostream& out, const std::string& cmd) {
        fail(out) << "Wrong command: " << cmd << std::endl;
    });
    cli_instance->StdExceptionHandler(
        [](std::ostream& out, const std::string& cmd, const std::exception& e) {
            fail(out) << "Command failed: " << cmd << " (" << e.what() << ")" << std::endl;
        });
    return cli_instance;
}

void repl::display_welcome(std::ostream& out) const {
    out << "ORE Studio Shell REPL v" << ORES_VERSION << std::endl;
    out << "Type 'help' for available commands, 'exit' to quit." << std::endl;
    out << std::endl;
}

void repl::cleanup() {
    if (!session_.is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Not connected, skipping cleanup.";
        return;
    }
    if (session_.is_logged_in()) {
        BOOST_LOG_SEV(lg(), debug) << "Sending logout request before exit.";
        try {
            std::ignore = session_.authenticated_request(
                "iam.v1.auth.logout", rfl::json::write(iam::messaging::logout_request{}));
            BOOST_LOG_SEV(lg(), info) << "Logged out successfully.";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Logout request failed during cleanup: " << e.what();
        }
        session_.clear_auth();
    }
    BOOST_LOG_SEV(lg(), debug) << "Disconnecting from server.";
    session_.disconnect();
}

}
