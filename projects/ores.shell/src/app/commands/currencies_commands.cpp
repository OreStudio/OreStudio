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

    currencies_menu->Insert("add", [&session](std::ostream& out,
            std::string iso_code, std::string name,
            std::string numeric_code, std::string symbol,
            std::string fractions_per_unit) {
        process_add_currency(std::ref(out), std::ref(session),
            std::move(iso_code), std::move(name),
            std::move(numeric_code), std::move(symbol),
            std::move(fractions_per_unit));
    }, "Add a currency (iso_code name [numeric_code] [symbol] [fractions_per_unit])");

    root_menu.Insert(std::move(currencies_menu));
}

void currencies_commands::
process_get_currencies(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get currencies request.";

    using risk::messaging::get_currencies_request;
    auto result = session.process_request(get_currencies_request{});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->currencies.size() << " currencies.";
    out << result->currencies << std::endl;
}

void currencies_commands::
process_add_currency(std::ostream& out, client_session& session,
    std::string iso_code, std::string name,
    std::string numeric_code, std::string symbol,
    std::string fractions_per_unit) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add currency request for: "
                               << iso_code;

    // Get recorded_by from logged-in user
    const auto& session_info = session.session_info();
    if (!session_info) {
        out << "✗ You must be logged in to add a currency." << std::endl;
        return;
    }
    const auto& recorded_by = session_info->username;

    // Parse fractions_per_unit with default
    int fractions = 100;
    if (!fractions_per_unit.empty()) {
        try {
            fractions = std::stoi(fractions_per_unit);
        } catch (...) {
            out << "✗ Invalid fractions_per_unit value: " << fractions_per_unit
                << std::endl;
            return;
        }
    }

    using risk::messaging::save_currency_request;
    using risk::messaging::save_currency_response;
    auto result = session.process_authenticated_request<save_currency_request,
                                                        save_currency_response,
                                                        message_type::save_currency_request>
        (save_currency_request{
            .currency = risk::domain::currency{
                .version = 0,
                .iso_code = std::move(iso_code),
                .name = std::move(name),
                .numeric_code = std::move(numeric_code),
                .symbol = std::move(symbol),
                .fraction_symbol = "",
                .fractions_per_unit = fractions,
                .rounding_type = "Nearest",
                .rounding_precision = 2,
                .format = "",
                .currency_type = "fiat",
                .recorded_by = recorded_by,
                .recorded_at = ""
            }
        });

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added currency.";
        out << "✓ Currency added successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to add currency: "
                                  << response.message;
        out << "✗ Failed to add currency: " << response.message << std::endl;
    }
}

}
