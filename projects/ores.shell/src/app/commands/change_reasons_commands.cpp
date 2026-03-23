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
#include "ores.shell/app/commands/change_reasons_commands.hpp"

#include <ostream>
#include <functional>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include "ores.dq.api/messaging/change_management_protocol.hpp"
#include "ores.dq.api/domain/change_reason_table_io.hpp" // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace logging;
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

void change_reasons_commands::
register_commands(cli::Menu& root_menu, nats_session& session,
                  pagination_context& /*pagination*/) {
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
process_get_change_reasons(std::ostream& out, nats_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get change reasons request.";

    auto result = do_request<dq::messaging::get_change_reasons_response>(
        out, session, "dq.v1.change-reasons.list",
        rfl::json::write(dq::messaging::get_change_reasons_request{}));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->reasons.size() << " change reasons.";
    out << result->reasons << std::endl;
}

void change_reasons_commands::
process_add_change_reason(std::ostream& out, nats_session& session,
    std::string code, std::string description,
    std::string category_code, std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add change reason request for: "
                               << code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to add a change reason." << std::endl;
        return;
    }
    const auto& modified_by = session.auth().username;

    auto req = dq::messaging::save_change_reason_request::from(
        dq::domain::change_reason{
            .version = 0,
            .code = std::move(code),
            .description = std::move(description),
            .category_code = std::move(category_code),
            .applies_to_amend = true,
            .applies_to_delete = true,
            .requires_commentary = false,
            .display_order = 0,
            .modified_by = modified_by,
            .change_commentary = std::move(change_commentary),
            .recorded_at = std::chrono::system_clock::now()
        });

    auto result = do_auth_request<dq::messaging::save_change_reason_response>(
        out, session, "dq.v1.change-reasons.save", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added change reason.";
        out << "✓ Change reason added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add change reason: " << msg;
        out << "✗ Failed to add change reason: " << msg << std::endl;
    }
}

void change_reasons_commands::
process_delete_change_reason(std::ostream& out, nats_session& session,
    std::string code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete change reason request for: "
                               << code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to delete a change reason." << std::endl;
        return;
    }

    dq::messaging::delete_change_reason_request req;
    req.codes = {code};

    auto result = do_auth_request<dq::messaging::delete_change_reason_response>(
        out, session, "dq.v1.change-reasons.delete", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted change reason: " << code;
        out << "✓ Change reason " << code << " deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete change reason: " << result->message;
        out << "✗ Failed to delete change reason: " << result->message << std::endl;
    }
}

void change_reasons_commands::
process_get_change_reason_history(std::ostream& out, nats_session& session,
    std::string code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get change reason history for: "
                               << code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to get change reason history."
            << std::endl;
        return;
    }

    dq::messaging::get_change_reason_history_request req;
    req.code = std::move(code);

    auto result = do_auth_request<dq::messaging::get_change_reason_history_response>(
        out, session, "dq.v1.change-reasons.history", rfl::json::write(req));
    if (!result) return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get change reason history: "
                                  << result->message;
        out << "✗ " << result->message << std::endl;
        return;
    }

    if (result->versions.empty()) {
        out << "No history found for this change reason." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->versions.size() << " history records.";
    out << result->versions << std::endl;
}

}
