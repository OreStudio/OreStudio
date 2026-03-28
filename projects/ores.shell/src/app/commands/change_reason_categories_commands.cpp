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
#include "ores.shell/app/commands/change_reason_categories_commands.hpp"

#include <ostream>
#include <functional>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include "ores.dq.api/messaging/change_management_protocol.hpp"
#include "ores.dq.api/domain/change_reason_category_table_io.hpp" // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

template<typename Response>
std::optional<Response> do_request(std::ostream& out, nats_client& session,
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
std::optional<Response> do_auth_request(std::ostream& out, nats_client& session,
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

void change_reason_categories_commands::
register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("change-reason-categories");

    menu->Insert("get", [&session](std::ostream& out) {
        process_get_categories(std::ref(out), std::ref(session));
    }, "Retrieve all change reason categories from the server");

    menu->Insert("add", [&session](std::ostream& out,
            std::string code, std::string description,
            std::string change_commentary) {
        process_add_category(std::ref(out), std::ref(session),
            std::move(code), std::move(description),
            std::move(change_commentary));
    }, "Add a category (code description \"commentary\")");

    menu->Insert("delete", [&session](std::ostream& out, std::string code) {
        process_delete_category(std::ref(out), std::ref(session),
            std::move(code));
    }, "Delete a category by code");

    menu->Insert("history", [&session](std::ostream& out, std::string code) {
        process_get_category_history(std::ref(out), std::ref(session),
            std::move(code));
    }, "Get version history for a category by code");

    root_menu.Insert(std::move(menu));
}

void change_reason_categories_commands::
process_get_categories(std::ostream& out, nats_client& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get change reason categories request.";

    auto result = do_request<dq::messaging::get_change_reason_categories_response>(
        out, session, "dq.v1.change-reason-categories.list",
        rfl::json::write(dq::messaging::get_change_reason_categories_request{}));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->categories.size() << " categories.";
    out << result->categories << std::endl;
}

void change_reason_categories_commands::
process_add_category(std::ostream& out, nats_client& session,
    std::string code, std::string description, std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add change reason category for: "
                               << code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to add a category." << std::endl;
        return;
    }
    const auto& modified_by = session.auth().username;

    auto req = dq::messaging::save_change_reason_category_request::from(
        dq::domain::change_reason_category{
            .version = 0,
            .code = std::move(code),
            .description = std::move(description),
            .modified_by = modified_by,
            .change_commentary = std::move(change_commentary),
            .recorded_at = std::chrono::system_clock::now()
        });

    auto result = do_auth_request<dq::messaging::save_change_reason_category_response>(
        out, session, "dq.v1.change-reason-categories.save", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added category.";
        out << "✓ Category added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add category: " << msg;
        out << "✗ Failed to add category: " << msg << std::endl;
    }
}

void change_reason_categories_commands::
process_delete_category(std::ostream& out, nats_client& session,
    std::string code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete category request for: "
                               << code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to delete a category." << std::endl;
        return;
    }

    dq::messaging::delete_change_reason_category_request req;
    req.codes = {code};

    auto result = do_auth_request<dq::messaging::delete_change_reason_category_response>(
        out, session, "dq.v1.change-reason-categories.delete", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted category: " << code;
        out << "✓ Category " << code << " deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete category: " << result->message;
        out << "✗ Failed to delete category: " << result->message << std::endl;
    }
}

void change_reason_categories_commands::
process_get_category_history(std::ostream& out, nats_client& session,
    std::string code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get category history for: "
                               << code;

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to get category history." << std::endl;
        return;
    }

    dq::messaging::get_change_reason_category_history_request req;
    req.code = std::move(code);

    auto result = do_auth_request<dq::messaging::get_change_reason_category_history_response>(
        out, session, "dq.v1.change-reason-categories.history", rfl::json::write(req));
    if (!result) return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get category history: "
                                  << result->message;
        out << "✗ " << result->message << std::endl;
        return;
    }

    if (result->versions.empty()) {
        out << "No history found for this category." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->versions.size() << " history records.";
    out << result->versions << std::endl;
}

}
