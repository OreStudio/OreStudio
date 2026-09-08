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
#include "ores.shell/app/commands/trading/trade_id_type_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/trade_id_type.hpp"
#include "ores.trading.api/domain/trade_id_type_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/trade_id_type_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include <functional>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace domain = ores::trading::domain;

void trade_id_type_commands::register_commands(cli::Menu& root_menu,
                                               nats_client& session,
                                               pagination_context& pagination) {
    auto trade_id_types_menu = std::make_unique<cli::Menu>("trade_id_types");

    trade_id_types_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_trade_id_types(std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve trade id types from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("trade_id_types", [&session, &pagination](std::ostream& out) {
        process_get_trade_id_types(out, session, pagination);
    });

    trade_id_types_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string code,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_trade_id_type(std::ref(out),
                                      std::ref(session),
                                      std::move(code),
                                      std::move(description),
                                      std::move(change_reason_code),
                                      std::move(change_commentary));
        },
        "Add a trade id type (code description reason_code \"commentary\")");

    trade_id_types_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string code) {
            process_delete_trade_id_type(std::ref(out), std::ref(session), std::move(code));
        },
        "Delete a trade id type by code");

    trade_id_types_menu->Insert(
        "history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_trade_id_type_history(std::ref(out), std::ref(session), args);
        },
        "Show a trade id type's version history (--diff for a unified diff, --version <n> to "
        "pick one)",
        {"code [--diff] [--version <n>]"});

    root_menu.Insert(std::move(trade_id_types_menu));
}

void trade_id_type_commands::process_get_trade_id_types(std::ostream& out,
                                                        nats_client& session,
                                                        pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get trade id types request.";

    auto& state = pagination.state_for("trade_id_types");

    trading::messaging::get_trade_id_types_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_trade_id_types_response>(
        out, session, "trading.v1.trade_id_types.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("trade_id_types");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->id_types.size()
                              << " trade id types.";
    out << result->id_types << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->id_types.size() << " of "
        << state.total_count << " total)" << std::endl;
}

void trade_id_type_commands::process_add_trade_id_type(std::ostream& out,
                                                       nats_client& session,
                                                       std::string code,
                                                       std::string description,
                                                       std::string change_reason_code,
                                                       std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add trade id type request for: " << code;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add a trade id type." << std::endl;
        return;
    }

    auto req = trading::messaging::save_trade_id_type_request::from(
        domain::trade_id_type{.code = std::move(code),
                              .description = std::move(description),
                              .change_reason_code = std::move(change_reason_code),
                              .change_commentary = std::move(change_commentary),
                              .recorded_at = std::chrono::system_clock::now()});

    auto result = do_auth_request<trading::messaging::save_trade_id_type_response>(
        out, session, "trading.v1.trade_id_types.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added trade id type.";
        out << "✓ Trade id type added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add trade id type: " << msg;
        fail(out) << "Failed to add trade id type: " << msg << std::endl;
    }
}

void trade_id_type_commands::process_delete_trade_id_type(std::ostream& out,
                                                          nats_client& session,
                                                          std::string code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete trade id type request for: " << code;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete a trade id type." << std::endl;
        return;
    }

    trading::messaging::delete_trade_id_type_request req;
    req.codes = {std::move(code)};

    auto result = do_auth_request<trading::messaging::delete_trade_id_type_response>(
        out, session, "trading.v1.trade_id_types.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted trade id type.";
        out << "✓ Trade id type deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete trade id type: " << result->message;
        fail(out) << "Failed to delete trade id type: " << result->message << std::endl;
    }
}

void trade_id_type_commands::process_get_trade_id_type_history(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "diff", .requires_value = false, .default_value = "false"},
                              {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: trade_id_types history code [--diff] [--version <n>]" << std::endl;
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
        render_history_diff(out, session, "ores.trading.trade_id_type", std::move(code), version);
        return;
    }

    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Initiating get trade id type history for: " << code;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get trade id type history." << std::endl;
        return;
    }

    trading::messaging::get_trade_id_type_history_request req;
    req.code = code;

    auto result = do_auth_request<trading::messaging::get_trade_id_type_history_response>(
        out, session, "trading.v1.trade_id_types.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get trade id type history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this trade id type." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
