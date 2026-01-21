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
#include "ores.comms.shell/app/commands/change_reasons_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.dq/domain/change_reason_table_io.hpp" // IWYU pragma: keep.

namespace ores::comms::shell::app::commands {

using namespace logging;
using comms::messaging::message_type;
using comms::net::client_session;

void change_reasons_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    auto menu = std::make_unique<cli::Menu>("change-reasons");

    menu->Insert("get", [&session](std::ostream& out) {
        process_get_change_reasons(std::ref(out), std::ref(session));
    }, "Retrieve all change reasons from the server");

    menu->Insert("add", [&session](std::ostream& out,
            std::string code, std::string description,
            std::string category_code, std::string change_commentary) {
        process_add_change_reason(std::ref(out), std::ref(session),
            std::move(code), std::move(description),
            std::move(category_code), std::move(change_commentary));
    }, "Add a change reason (code description category_code \"commentary\")");

    menu->Insert("delete", [&session](std::ostream& out, std::string code) {
        process_delete_change_reason(std::ref(out), std::ref(session),
            std::move(code));
    }, "Delete a change reason by code");

    menu->Insert("history", [&session](std::ostream& out, std::string code) {
        process_get_change_reason_history(std::ref(out), std::ref(session),
            std::move(code));
    }, "Get version history for a change reason by code");

    root_menu.Insert(std::move(menu));
}

void change_reasons_commands::
process_get_change_reasons(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get change reasons request.";

    using dq::messaging::get_change_reasons_request;
    auto result = session.process_request(get_change_reasons_request{});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->reasons.size() << " change reasons.";
    out << result->reasons << std::endl;
}

void change_reasons_commands::
process_add_change_reason(std::ostream& out, client_session& session,
    std::string code, std::string description,
    std::string category_code, std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add change reason request for: "
                               << code;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to add a change reason." << std::endl;
        return;
    }
    const auto& recorded_by = session.session_info()->username;

    using dq::messaging::save_change_reason_request;
    using dq::messaging::save_change_reason_response;
    auto result = session.process_authenticated_request<save_change_reason_request,
                                                        save_change_reason_response,
                                                        message_type::save_change_reason_request>
        (save_change_reason_request{
            .reason = dq::domain::change_reason{
                .version = 0,
                .code = std::move(code),
                .description = std::move(description),
                .category_code = std::move(category_code),
                .applies_to_amend = true,
                .applies_to_delete = true,
                .requires_commentary = false,
                .display_order = 0,
                .recorded_by = recorded_by,
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
        BOOST_LOG_SEV(lg(), info) << "Successfully added change reason.";
        out << "✓ Change reason added successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to add change reason: "
                                  << response.message;
        out << "✗ Failed to add change reason: " << response.message
            << std::endl;
    }
}

void change_reasons_commands::
process_delete_change_reason(std::ostream& out, client_session& session,
    std::string code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete change reason request for: "
                               << code;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to delete a change reason." << std::endl;
        return;
    }

    using dq::messaging::delete_change_reason_request;
    using dq::messaging::delete_change_reason_response;
    auto result = session.process_authenticated_request<delete_change_reason_request,
                                                        delete_change_reason_response,
                                                        message_type::delete_change_reason_request>
        (delete_change_reason_request{.codes = {std::move(code)}});

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
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted change reason: "
                                  << delete_result.code;
        out << "✓ Change reason " << delete_result.code
            << " deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete change reason: "
                                  << delete_result.message;
        out << "✗ Failed to delete change reason: " << delete_result.message
            << std::endl;
    }
}

void change_reasons_commands::
process_get_change_reason_history(std::ostream& out, client_session& session,
    std::string code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get change reason history for: "
                               << code;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to get change reason history."
            << std::endl;
        return;
    }

    using dq::messaging::get_change_reason_history_request;
    using dq::messaging::get_change_reason_history_response;
    auto result = session.process_authenticated_request<get_change_reason_history_request,
                                                        get_change_reason_history_response,
                                                        message_type::get_change_reason_history_request>
        (get_change_reason_history_request{.code = std::move(code)});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get change reason history: "
                                  << response.message;
        out << "✗ " << response.message << std::endl;
        return;
    }

    if (response.versions.empty()) {
        out << "No history found for this change reason." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << response.versions.size() << " history records.";
    out << response.versions << std::endl;
}

}
