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
#include "ores.comms.shell/app/repl.hpp"

#include <iostream>
#include <rfl/json.hpp>
#include <cli/cli.h>
#include <cli/clifilesession.h>
#include "ores.utility/version/version.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.comms.shell/app/commands/change_reason_categories_commands.hpp"
#include "ores.comms.shell/app/commands/change_reasons_commands.hpp"
#include "ores.comms.shell/app/commands/countries_commands.hpp"
#include "ores.comms.shell/app/commands/currencies_commands.hpp"
#include "ores.comms.shell/app/commands/connection_commands.hpp"
#include "ores.comms.shell/app/commands/accounts_commands.hpp"
#include "ores.comms.shell/app/commands/variability_commands.hpp"
#include "ores.comms.shell/app/commands/subscription_commands.hpp"
#include "ores.comms.shell/app/commands/rbac_commands.hpp"
#include "ores.comms.shell/app/commands/tenants_commands.hpp"
#include "ores.comms.shell/app/commands/navigation_commands.hpp"
#include "ores.comms.shell/app/commands/script_commands.hpp"

namespace ores::comms::shell::app {

using namespace ores::logging;

repl::repl(service::nats_session& session)
    : session_(session) {
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
    connection_commands::register_commands(*root, session_);
    countries_commands::register_commands(*root, session_, pagination_);
    currencies_commands::register_commands(*root, session_, pagination_);
    accounts_commands::register_commands(*root, session_, pagination_);
    variability_commands::register_commands(*root, session_);
    subscription_commands::register_commands(*root, session_);
    rbac_commands::register_commands(*root, session_, pagination_);
    tenants_commands::register_commands(*root, session_, pagination_);
    navigation_commands::register_commands(*root, pagination_);
    script_commands::register_commands(*root, active_session_);

    auto cli_instance = std::make_unique<cli::Cli>(std::move(root));
    cli_instance->ExitAction([](auto& out) { out << "Bye!" << std::endl; });
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
            std::ignore = session_.authenticated_request("iam.v1.auth.logout",
                rfl::json::write(iam::messaging::logout_request{}));
            BOOST_LOG_SEV(lg(), info) << "Logged out successfully.";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Logout request failed during cleanup: "
                                      << e.what();
        }
        session_.clear_auth();
    }
    BOOST_LOG_SEV(lg(), debug) << "Disconnecting from server.";
    session_.disconnect();
}

}
