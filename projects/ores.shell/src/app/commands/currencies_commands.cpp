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
#include "ores.shell/app/commands/currencies_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.risk/messaging/currency_protocol.hpp"
#include "ores.risk/domain/currency_table_io.hpp" // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace utility::log;
using comms::messaging::message_type;
using comms::net::client_session;

void currencies_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    auto currencies_menu =
        std::make_unique<cli::Menu>("currencies");

    currencies_menu->Insert("get", [&session](std::ostream& out) {
        process_get_currencies(std::ref(out), std::ref(session));
    }, "Retrieve all currencies from the server");

    root_menu.Insert(std::move(currencies_menu));
}

void currencies_commands::
process_get_currencies(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get currencies request.";

    using risk::messaging::get_currencies_request;
    using risk::messaging::get_currencies_response;
    auto result = session.process_request<get_currencies_request,
                                          get_currencies_response,
                                          message_type::get_currencies_request>
        (get_currencies_request{});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->currencies.size() << " currencies.";
    out << result->currencies << std::endl;
}

}
