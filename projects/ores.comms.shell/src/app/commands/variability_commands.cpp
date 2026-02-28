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
#include "ores.comms.shell/app/commands/variability_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"
#include "ores.variability/domain/feature_flags_table_io.hpp"  // IWYU pragma: keep.

namespace ores::comms::shell::app::commands {

using namespace ores::logging;
using comms::messaging::message_type;
using comms::net::client_session;

void variability_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    auto variability_menu =
        std::make_unique<cli::Menu>("variability");

    variability_menu->Insert("list-flags", [&session](std::ostream& out) {
        process_list_feature_flags(std::ref(out), std::ref(session));
    }, "Retrieve all feature flags from the server");

    variability_menu->Insert("add-flag", [&session](std::ostream& out,
            std::string name, std::string enabled, std::string description,
            std::string change_reason_code, std::string change_commentary) {
        process_add_feature_flag(std::ref(out), std::ref(session),
            std::move(name), std::move(enabled), std::move(description),
            std::move(change_reason_code), std::move(change_commentary));
    }, "Add a feature flag (name enabled description reason_code \"commentary\")");

    variability_menu->Insert("delete-flag", [&session](std::ostream& out,
            std::string name) {
        process_delete_feature_flag(std::ref(out), std::ref(session),
            std::move(name));
    }, "Delete a feature flag by name");

    variability_menu->Insert("flag-history", [&session](std::ostream& out,
            std::string name) {
        process_get_feature_flag_history(std::ref(out), std::ref(session),
            std::move(name));
    }, "Get version history for a feature flag by name");

    root_menu.Insert(std::move(variability_menu));
}

void variability_commands::
process_list_feature_flags(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list feature flags request.";

    using variability::messaging::get_feature_flags_request;
    auto result = session.process_request(get_feature_flags_request{});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->feature_flags.size() << " feature flags.";
    out << result->feature_flags << std::endl;
}

void variability_commands::
process_add_feature_flag(std::ostream& out, client_session& session,
    std::string name, std::string enabled, std::string description,
    std::string change_reason_code, std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add feature flag request for: "
                               << name;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to add a feature flag." << std::endl;
        return;
    }
    const auto& modified_by = session.session_info()->username;

    // Parse enabled flag
    bool is_enabled = (enabled == "true" || enabled == "1" || enabled == "yes");

    using variability::messaging::save_feature_flag_request;
    using variability::messaging::save_feature_flag_response;
    auto result = session.process_authenticated_request<save_feature_flag_request,
                                                        save_feature_flag_response,
                                                        message_type::save_feature_flag_request>
        (save_feature_flag_request::from(variability::domain::feature_flags{
            .version = 0,
            .enabled = is_enabled,
            .name = std::move(name),
            .description = std::move(description),
            .modified_by = modified_by,
            .change_reason_code = std::move(change_reason_code),
            .change_commentary = std::move(change_commentary),
            .recorded_at = std::chrono::system_clock::now()
        }));

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added feature flag.";
        out << "✓ Feature flag added successfully!" << std::endl;
    } else {
        const auto& msg = response.message.empty() ? "Unknown error" : response.message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add feature flag: " << msg;
        out << "✗ Failed to add feature flag: " << msg << std::endl;
    }
}

void variability_commands::
process_delete_feature_flag(std::ostream& out, client_session& session,
    std::string name) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete feature flag request for: "
                               << name;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to delete a feature flag." << std::endl;
        return;
    }

    using variability::messaging::delete_feature_flag_request;
    using variability::messaging::delete_feature_flag_response;
    auto result = session.process_authenticated_request<delete_feature_flag_request,
                                                        delete_feature_flag_response,
                                                        message_type::delete_feature_flag_request>
        (delete_feature_flag_request{.name = std::move(name)});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted feature flag.";
        out << "✓ Feature flag deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete feature flag: "
                                  << response.error_message;
        out << "✗ Failed to delete feature flag: " << response.error_message
            << std::endl;
    }
}

void variability_commands::
process_get_feature_flag_history(std::ostream& out, client_session& session,
    std::string name) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get feature flag history for: "
                               << name;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to get feature flag history."
            << std::endl;
        return;
    }

    using variability::messaging::get_feature_flag_history_request;
    using variability::messaging::get_feature_flag_history_response;
    auto result = session.process_authenticated_request<
        get_feature_flag_history_request,
        get_feature_flag_history_response,
        message_type::get_feature_flag_history_request>
        (get_feature_flag_history_request{.name = std::move(name)});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get feature flag history: "
                                  << response.message;
        out << "✗ " << response.message << std::endl;
        return;
    }

    if (response.history.empty()) {
        out << "No history found for this feature flag." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << response.history.size() << " history records.";
    out << response.history << std::endl;
}

}
