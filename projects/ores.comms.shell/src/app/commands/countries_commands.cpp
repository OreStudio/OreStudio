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
#include "ores.comms.shell/app/commands/countries_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.refdata/messaging/country_protocol.hpp"
#include "ores.refdata/domain/country_table_io.hpp" // IWYU pragma: keep.

namespace ores::comms::shell::app::commands {

using namespace logging;
using comms::messaging::message_type;
using comms::net::client_session;

void countries_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    auto countries_menu =
        std::make_unique<cli::Menu>("countries");

    countries_menu->Insert("get", [&session](std::ostream& out) {
        process_get_countries(std::ref(out), std::ref(session));
    }, "Retrieve all countries from the server");

    countries_menu->Insert("add", [&session](std::ostream& out,
            std::string alpha2_code, std::string alpha3_code,
            std::string numeric_code, std::string name,
            std::string official_name, std::string change_reason_code,
            std::string change_commentary) {
        process_add_country(std::ref(out), std::ref(session),
            std::move(alpha2_code), std::move(alpha3_code),
            std::move(numeric_code), std::move(name),
            std::move(official_name), std::move(change_reason_code),
            std::move(change_commentary));
    }, "Add a country (alpha2 alpha3 numeric name official_name reason_code \"commentary\")");

    countries_menu->Insert("delete", [&session](std::ostream& out,
            std::string alpha2_code) {
        process_delete_country(std::ref(out), std::ref(session),
            std::move(alpha2_code));
    }, "Delete a country by alpha-2 code");

    countries_menu->Insert("history", [&session](std::ostream& out,
            std::string alpha2_code) {
        process_get_country_history(std::ref(out), std::ref(session),
            std::move(alpha2_code));
    }, "Get version history for a country by alpha-2 code");

    root_menu.Insert(std::move(countries_menu));
}

void countries_commands::
process_get_countries(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get countries request.";

    using refdata::messaging::get_countries_request;
    auto result = session.process_request(get_countries_request{});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->countries.size() << " countries.";
    out << result->countries << std::endl;
}

void countries_commands::
process_add_country(std::ostream& out, client_session& session,
    std::string alpha2_code, std::string alpha3_code,
    std::string numeric_code, std::string name,
    std::string official_name, std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add country request for: "
                               << alpha2_code;

    // Get recorded_by from logged-in user
    const auto& session_info = session.session_info();
    if (!session_info) {
        out << "✗ You must be logged in to add a country." << std::endl;
        return;
    }
    const auto& recorded_by = session_info->username;

    using refdata::messaging::save_country_request;
    using refdata::messaging::save_country_response;
    auto result = session.process_authenticated_request<save_country_request,
                                                        save_country_response,
                                                        message_type::save_country_request>
        (save_country_request{
            .country = refdata::domain::country{
                .version = 0,
                .alpha2_code = std::move(alpha2_code),
                .alpha3_code = std::move(alpha3_code),
                .numeric_code = std::move(numeric_code),
                .name = std::move(name),
                .official_name = std::move(official_name),
                .image_id = std::nullopt,
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
        BOOST_LOG_SEV(lg(), info) << "Successfully added country.";
        out << "✓ Country added successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to add country: "
                                  << response.message;
        out << "✗ Failed to add country: " << response.message << std::endl;
    }
}

void countries_commands::
process_delete_country(std::ostream& out, client_session& session,
    std::string alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete country request for: "
                               << alpha2_code;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to delete a country." << std::endl;
        return;
    }

    using refdata::messaging::delete_country_request;
    using refdata::messaging::delete_country_response;
    auto result = session.process_authenticated_request<delete_country_request,
                                                        delete_country_response,
                                                        message_type::delete_country_request>
        (delete_country_request{.alpha2_codes = {std::move(alpha2_code)}});

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
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted country: "
                                  << delete_result.alpha2_code;
        out << "✓ Country " << delete_result.alpha2_code
            << " deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete country: "
                                  << delete_result.message;
        out << "✗ Failed to delete country: " << delete_result.message
            << std::endl;
    }
}

void countries_commands::
process_get_country_history(std::ostream& out, client_session& session,
    std::string alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get country history for: "
                               << alpha2_code;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to get country history." << std::endl;
        return;
    }

    using refdata::messaging::get_country_history_request;
    using refdata::messaging::get_country_history_response;
    auto result = session.process_authenticated_request<get_country_history_request,
                                                        get_country_history_response,
                                                        message_type::get_country_history_request>
        (get_country_history_request{.alpha2_code = std::move(alpha2_code)});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get country history: "
                                  << response.message;
        out << "✗ " << response.message << std::endl;
        return;
    }

    if (response.history.empty()) {
        out << "No history found for this country." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << response.history.size() << " history records.";
    out << response.history << std::endl;
}

}
