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
#include "ores.comms.shell/app/commands/currencies_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.refdata/messaging/currency_protocol.hpp"
#include "ores.refdata/messaging/currency_history_protocol.hpp"
#include "ores.refdata/domain/currency_table_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/currency_version_table_io.hpp" // IWYU pragma: keep.

namespace ores::comms::shell::app::commands {

using namespace logging;
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
            std::string fractions_per_unit, std::string change_reason_code,
            std::string change_commentary) {
        process_add_currency(std::ref(out), std::ref(session),
            std::move(iso_code), std::move(name),
            std::move(numeric_code), std::move(symbol),
            std::move(fractions_per_unit), std::move(change_reason_code),
            std::move(change_commentary));
    }, "Add a currency (iso_code name numeric_code symbol fractions reason_code \"commentary\")");

    currencies_menu->Insert("delete", [&session](std::ostream& out,
            std::string iso_code) {
        process_delete_currency(std::ref(out), std::ref(session),
            std::move(iso_code));
    }, "Delete a currency by ISO code");

    currencies_menu->Insert("history", [&session](std::ostream& out,
            std::string iso_code) {
        process_get_currency_history(std::ref(out), std::ref(session),
            std::move(iso_code));
    }, "Get version history for a currency by ISO code");

    root_menu.Insert(std::move(currencies_menu));
}

void currencies_commands::
process_get_currencies(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get currencies request.";

    using refdata::messaging::get_currencies_request;
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
    std::string fractions_per_unit, std::string change_reason_code,
    std::string change_commentary) {
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

    using refdata::messaging::save_currency_request;
    using refdata::messaging::save_currency_response;
    auto result = session.process_authenticated_request<save_currency_request,
                                                        save_currency_response,
                                                        message_type::save_currency_request>
        (save_currency_request{
            .currency = refdata::domain::currency{
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
                .change_reason_code = std::move(change_reason_code),
                .change_commentary = std::move(change_commentary),
                .recorded_at = std::chrono::system_clock::now()
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

void currencies_commands::
process_delete_currency(std::ostream& out, client_session& session,
    std::string iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete currency request for: "
                               << iso_code;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to delete a currency." << std::endl;
        return;
    }

    using refdata::messaging::delete_currency_request;
    using refdata::messaging::delete_currency_response;
    auto result = session.process_authenticated_request<delete_currency_request,
                                                        delete_currency_response,
                                                        message_type::delete_currency_request>
        (delete_currency_request{.iso_codes = {std::move(iso_code)}});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.results.empty()) {
        out << "✗ No results returned from server." << std::endl;
        return;
    }

    const auto& delete_result = response.results[0];
    if (delete_result.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted currency: "
                                  << delete_result.iso_code;
        out << "✓ Currency " << delete_result.iso_code
            << " deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete currency: "
                                  << delete_result.message;
        out << "✗ Failed to delete currency: " << delete_result.message
            << std::endl;
    }
}

void currencies_commands::
process_get_currency_history(std::ostream& out, client_session& session,
    std::string iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get currency history for: "
                               << iso_code;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to get currency history." << std::endl;
        return;
    }

    using refdata::messaging::get_currency_history_request;
    using refdata::messaging::get_currency_history_response;
    auto result = session.process_authenticated_request<get_currency_history_request,
                                                        get_currency_history_response,
                                                        message_type::get_currency_history_request>
        (get_currency_history_request{.iso_code = std::move(iso_code)});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get currency history: "
                                  << response.message;
        out << "✗ " << response.message << std::endl;
        return;
    }

    if (response.history.versions.empty()) {
        out << "No history found for this currency." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << response.history.versions.size()
                              << " history records.";
    out << response.history.versions << std::endl;
}

}
