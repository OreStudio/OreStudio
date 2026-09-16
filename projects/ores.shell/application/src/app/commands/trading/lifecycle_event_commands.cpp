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
#include "ores.shell/app/commands/trading/lifecycle_event_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/lifecycle_event.hpp"
#include "ores.trading.api/domain/lifecycle_event_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/lifecycle_event_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include <functional>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace domain = ores::trading::domain;

void lifecycle_event_commands::register_commands(cli::Menu& root_menu,
                                                 nats_client& session,
                                                 pagination_context& pagination) {
    auto lifecycle_events_menu = std::make_unique<cli::Menu>("lifecycle_events");

    lifecycle_events_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_lifecycle_events(std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve lifecycle events from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("lifecycle_events",
                                      [&session, &pagination](std::ostream& out) {
                                          process_get_lifecycle_events(out, session, pagination);
                                      });

    lifecycle_events_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string code,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_lifecycle_event(std::ref(out),
                                        std::ref(session),
                                        std::move(code),
                                        std::move(description),
                                        std::move(change_reason_code),
                                        std::move(change_commentary));
        },
        "Add a lifecycle event (code description reason_code \"commentary\")");

    lifecycle_events_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string code) {
            process_delete_lifecycle_event(std::ref(out), std::ref(session), std::move(code));
        },
        "Delete a lifecycle event by code");

    lifecycle_events_menu->Insert(
        "history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_lifecycle_event_history(std::ref(out), std::ref(session), args);
        },
        "Show a lifecycle event's version history (--diff for a unified diff, --version <n> to "
        "pick one)",
        {"code [--diff] [--version <n>]"});

    root_menu.Insert(std::move(lifecycle_events_menu));
}

void lifecycle_event_commands::process_get_lifecycle_events(std::ostream& out,
                                                            nats_client& session,
                                                            pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get lifecycle events request.";

    auto& state = pagination.state_for("lifecycle_events");

    trading::messaging::get_lifecycle_events_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_lifecycle_events_response>(
        out, session, "trading.v1.lifecycle_events.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("lifecycle_events");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->events.size()
                              << " lifecycle events.";
    out << result->events << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->events.size() << " of "
        << state.total_count << " total)" << std::endl;
}

void lifecycle_event_commands::process_add_lifecycle_event(std::ostream& out,
                                                           nats_client& session,
                                                           std::string code,
                                                           std::string description,
                                                           std::string change_reason_code,
                                                           std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add lifecycle event request for: " << code;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add a lifecycle event." << std::endl;
        return;
    }

    auto req = trading::messaging::save_lifecycle_event_request::from(
        domain::lifecycle_event{.code = std::move(code),
                                .description = std::move(description),
                                .change_reason_code = std::move(change_reason_code),
                                .change_commentary = std::move(change_commentary),
                                .recorded_at = std::chrono::system_clock::now()});

    auto result = do_auth_request<trading::messaging::save_lifecycle_event_response>(
        out, session, "trading.v1.lifecycle_events.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added lifecycle event.";
        out << "✓ Lifecycle event added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add lifecycle event: " << msg;
        fail(out) << "Failed to add lifecycle event: " << msg << std::endl;
    }
}

void lifecycle_event_commands::process_delete_lifecycle_event(std::ostream& out,
                                                              nats_client& session,
                                                              std::string code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete lifecycle event request for: " << code;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete a lifecycle event." << std::endl;
        return;
    }

    trading::messaging::delete_lifecycle_event_request req;
    req.codes = {std::move(code)};

    auto result = do_auth_request<trading::messaging::delete_lifecycle_event_response>(
        out, session, "trading.v1.lifecycle_events.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted lifecycle event.";
        out << "✓ Lifecycle event deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete lifecycle event: " << result->message;
        fail(out) << "Failed to delete lifecycle event: " << result->message << std::endl;
    }
}

void lifecycle_event_commands::process_get_lifecycle_event_history(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "diff", .requires_value = false, .default_value = "false"},
                              {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: lifecycle_events history code [--diff] [--version <n>]" << std::endl;
        return;
    }
    auto code = parsed->positionals.front();

    std::optional<int> version;
    if (const auto& v = parsed->flag("version"); !v.empty()) {
        const auto parsed_version = parse_uint32(v);
        if (!parsed_version) {
            fail(out) << "Invalid --version value: " << v << std::endl;
            return;
        }
        version = static_cast<int>(*parsed_version);
    }

    if (parsed->flag_set("diff")) {
        render_history_diff(out, session, "ores.trading.lifecycle_event", std::move(code), version);
        return;
    }

    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Initiating get lifecycle event history for: " << code;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get lifecycle event history." << std::endl;
        return;
    }

    trading::messaging::get_lifecycle_event_history_request req;
    req.code = code;

    auto result = do_auth_request<trading::messaging::get_lifecycle_event_history_response>(
        out, session, "trading.v1.lifecycle_events.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get lifecycle event history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this lifecycle event." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
