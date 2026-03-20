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
#include "ores.shell/app/commands/variability_commands.hpp"

#include <ostream>
#include <functional>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include "ores.variability/messaging/system_settings_protocol.hpp"
#include "ores.variability/domain/system_setting_table_io.hpp"  // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace ores::logging;
using service::nats_session;

namespace {

template<typename Response>
std::optional<Response> do_request(std::ostream& out, nats_session& session,
    const std::string& subject, const std::string& body) {
    try {
        auto reply = session.request(subject, body);
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            out << "✗ Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out << "✗ Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

template<typename Response>
std::optional<Response> do_auth_request(std::ostream& out, nats_session& session,
    const std::string& subject, const std::string& body) {
    try {
        auto reply = session.authenticated_request(subject, body);
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            out << "✗ Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out << "✗ Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

} // anonymous namespace

void variability_commands::
register_commands(cli::Menu& root_menu, nats_session& session) {
    auto variability_menu =
        std::make_unique<cli::Menu>("variability");

    variability_menu->Insert("list-settings", [&session](std::ostream& out) {
        process_list_settings(std::ref(out), std::ref(session));
    }, "Retrieve all system settings from the server");

    variability_menu->Insert("save-setting", [&session](std::ostream& out,
            std::string name, std::string value, std::string data_type,
            std::string description,
            std::string change_reason_code, std::string change_commentary) {
        process_save_setting(std::ref(out), std::ref(session),
            std::move(name), std::move(value), std::move(data_type),
            std::move(description),
            std::move(change_reason_code), std::move(change_commentary));
    }, "Save a system setting (name value data_type description reason_code \"commentary\")");

    variability_menu->Insert("delete-setting", [&session](std::ostream& out,
            std::string name) {
        process_delete_setting(std::ref(out), std::ref(session),
            std::move(name));
    }, "Delete a system setting by name");

    variability_menu->Insert("setting-history", [&session](std::ostream& out,
            std::string name) {
        process_get_setting_history(std::ref(out), std::ref(session),
            std::move(name));
    }, "Get version history for a system setting by name");

    root_menu.Insert(std::move(variability_menu));
}

void variability_commands::
process_list_settings(std::ostream& out, nats_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list settings request.";

    auto result = do_request<variability::messaging::list_settings_response>(
        out, session, "variability.v1.settings.list",
        rfl::json::write(variability::messaging::list_settings_request{}));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->settings.size() << " settings.";
    out << result->settings << std::endl;
}

void variability_commands::
process_save_setting(std::ostream& out, nats_session& session,
    std::string name, std::string value, std::string data_type,
    std::string description,
    std::string change_reason_code, std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating save setting request for: "
                               << name;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to save a setting." << std::endl;
        return;
    }
    const auto& modified_by = session.auth().username;

    auto req = variability::messaging::save_setting_request::from(
        variability::domain::system_setting{
            .version = 0,
            .name = std::move(name),
            .value = std::move(value),
            .data_type = std::move(data_type),
            .description = std::move(description),
            .modified_by = modified_by,
            .change_reason_code = std::move(change_reason_code),
            .change_commentary = std::move(change_commentary),
            .recorded_at = std::chrono::system_clock::now()
        });

    auto result = do_auth_request<variability::messaging::save_setting_response>(
        out, session, "variability.v1.settings.save", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully saved setting.";
        out << "✓ Setting saved successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to save setting: " << msg;
        out << "✗ Failed to save setting: " << msg << std::endl;
    }
}

void variability_commands::
process_delete_setting(std::ostream& out, nats_session& session,
    std::string name) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete setting request for: "
                               << name;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to delete a setting." << std::endl;
        return;
    }

    variability::messaging::delete_setting_request req;
    req.name = std::move(name);

    auto result = do_auth_request<variability::messaging::delete_setting_response>(
        out, session, "variability.v1.settings.delete", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted setting.";
        out << "✓ Setting deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete setting: "
                                  << result->error_message;
        out << "✗ Failed to delete setting: " << result->error_message
            << std::endl;
    }
}

void variability_commands::
process_get_setting_history(std::ostream& out, nats_session& session,
    std::string name) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get setting history for: "
                               << name;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to get setting history."
            << std::endl;
        return;
    }

    variability::messaging::get_setting_history_request req;
    req.name = std::move(name);

    auto result = do_auth_request<variability::messaging::get_setting_history_response>(
        out, session, "variability.v1.settings.history", rfl::json::write(req));
    if (!result) return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get setting history: "
                                  << result->message;
        out << "✗ " << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this setting." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->history.size() << " history records.";
    out << result->history << std::endl;
}

}
