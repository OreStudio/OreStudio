/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <cli/cli.h>
#include <cli/clifilesession.h>
#include "ores.utility/version/version.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.comms/messaging/message_type.hpp"
#include "ores.iam/messaging/protocol.hpp"
#include "ores.comms.shell/app/commands/change_reason_categories_commands.hpp"
#include "ores.comms.shell/app/commands/change_reasons_commands.hpp"
#include "ores.comms.shell/app/commands/countries_commands.hpp"
#include "ores.comms.shell/app/commands/currencies_commands.hpp"
#include "ores.comms.shell/app/commands/connection_commands.hpp"
#include "ores.comms.shell/app/commands/accounts_commands.hpp"
#include "ores.comms.shell/app/commands/variability_commands.hpp"
#include "ores.comms.shell/app/commands/compression_commands.hpp"
#include "ores.comms.shell/app/commands/subscription_commands.hpp"
#include "ores.comms.shell/app/commands/rbac_commands.hpp"
#include "ores.comms.shell/app/commands/tenants_commands.hpp"

namespace ores::comms::shell::app {

using namespace ores::logging;

repl::repl(comms::net::client_session& session)
    : session_(session) {
    BOOST_LOG_SEV(lg(), info) << "REPL created.";
}

void repl::run() {
    BOOST_LOG_SEV(lg(), info) << "REPL session started.";

    display_welcome();
    auto cli_instance = setup_menus();
    cli::CliFileSession session(*cli_instance, std::cin, std::cout);
    session.Start();

    // Clean up before exiting
    cleanup();

    BOOST_LOG_SEV(lg(), info) << "REPL session ended.";
}

std::unique_ptr<cli::Cli> repl::setup_menus() {
    auto root =
        std::make_unique<cli::Menu>("ores-shell");

    using namespace commands;
    change_reason_categories_commands::register_commands(*root, session_);
    change_reasons_commands::register_commands(*root, session_);
    connection_commands::register_commands(*root, session_);
    countries_commands::register_commands(*root, session_);
    currencies_commands::register_commands(*root, session_);
    accounts_commands::register_commands(*root, session_);
    variability_commands::register_commands(*root, session_);
    compression_commands::register_commands(*root);
    subscription_commands::register_commands(*root, session_);
    rbac_commands::register_commands(*root, session_);
    tenants_commands::register_commands(*root, session_);

    auto cli_instance =
        std::make_unique<cli::Cli>(std::move(root));
    cli_instance->ExitAction([](auto& out) {
        out << "Bye!" << std::endl;
    });

    return cli_instance;
}

void repl::display_welcome() const {
    std::cout << "ORE Studio Shell REPL v" << ORES_VERSION << std::endl;
    std::cout << "Type 'help' for available commands, 'exit' to quit." << std::endl;
    std::cout << std::endl;
}

void repl::cleanup() {
    // Only proceed if connected
    if (!session_.is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Not connected, skipping cleanup.";
        return;
    }

    // Send logout if logged in
    if (session_.is_logged_in()) {
        BOOST_LOG_SEV(lg(), debug) << "Sending logout request before exit.";

        using iam::messaging::logout_request;
        using iam::messaging::logout_response;
        using comms::messaging::message_type;

        auto result = session_.process_authenticated_request<logout_request,
                                                             logout_response,
                                                             message_type::logout_request>
            (logout_request{});

        if (result && result->success) {
            BOOST_LOG_SEV(lg(), info) << "Logged out successfully.";
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Logout request failed during cleanup.";
        }

        session_.clear_session_info();
    }

    // Disconnect cleanly
    BOOST_LOG_SEV(lg(), debug) << "Disconnecting from server.";
    session_.disconnect();
}

}
