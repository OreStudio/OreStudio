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
#include "ores.shell/app/commands/synthetic/yield_curve_process_type_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_type.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_type_table_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.api/messaging/yield_curve_process_type_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include <chrono>
#include <functional>
#include <optional>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace domain = ores::synthetic::domain;

namespace {

bool parse_flag(const std::string& value, bool& out) {
    if (value.empty()) {
        out = false;
        return true;
    }
    if (value == "true") {
        out = true;
        return true;
    }
    if (value == "false") {
        out = false;
        return true;
    }
    return false;
}

} // namespace

void yield_curve_process_type_commands::register_commands(cli::Menu& root_menu,
                                        nats_client& session,
                                        pagination_context& pagination) {
    auto yield_curve_process_types_menu = std::make_unique<cli::Menu>("yield_curve_process_types");

    yield_curve_process_types_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_yield_curve_process_types(std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve yield_curve_process_types from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("yield_curve_process_types",
                                      [&session, &pagination](std::ostream& out) {
                                          process_get_yield_curve_process_types(out, session, pagination);
                                      });

    yield_curve_process_types_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string s_code,
                   std::string s_name,
                   std::string s_description,
                   std::string s_display_order,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_yield_curve_process_type(std::ref(out),
                               std::ref(session),
                               std::move(s_code),
                               std::move(s_name),
                               std::move(s_description),
                               std::move(s_display_order),
                               std::move(change_reason_code),
                               std::move(change_commentary));
        },
        "Add a yield_curve_process_type (<code> <name> <description> <display_order> <reason_code> \"commentary\")");

    yield_curve_process_types_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string code) {
            process_delete_yield_curve_process_type(std::ref(out), std::ref(session), std::move(code));
        },
        "Delete a yield_curve_process_type by code");

    yield_curve_process_types_menu->Insert(
        "history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_yield_curve_process_type_history(std::ref(out), std::ref(session), args);
        },
        "Show a yield_curve_process_type's version history (--diff for a unified diff, --version <n> to "
        "pick one)",
        {"code [--diff] [--version <n>]"});

    root_menu.Insert(std::move(yield_curve_process_types_menu));
}

void yield_curve_process_type_commands::process_get_yield_curve_process_types(std::ostream& out,
                                           nats_client& session,
                                           pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get yield_curve_process_types request.";

    auto& state = pagination.state_for("yield_curve_process_types");

    synthetic::messaging::get_yield_curve_process_types_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<synthetic::messaging::get_yield_curve_process_types_response>(
        out, session, "synthetic.v1.yield_curve_process_types.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("yield_curve_process_types");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->process_types.size()
                              << " yield_curve_process_types.";
    out << result->process_types << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->process_types.size()
        << " of " << state.total_count << " total)" << std::endl;
}

void yield_curve_process_type_commands::process_add_yield_curve_process_type(std::ostream& out,
                                         nats_client& session
                                         ,
                                   std::string s_code,
                                   std::string s_name,
                                   std::string s_description,
                                   std::string s_display_order,
                                   std::string change_reason_code,
                                   std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add yield_curve_process_type request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add a yield_curve_process_type." << std::endl;
        return;
    }

    domain::yield_curve_process_type v;
    // code (str).
    v.code = std::move(s_code);

    // name (str).
    v.name = std::move(s_name);

    // description (str).
    v.description = std::move(s_description);

    // display_order (int).
    const auto n_display_order = parse_uint32(s_display_order);
    if (!n_display_order) {
        fail(out) << "Invalid display_order: " << s_display_order << std::endl;
        return;
    }
    v.display_order = static_cast<int>(*n_display_order);

    v.change_reason_code = std::move(change_reason_code);
    v.change_commentary = std::move(change_commentary);
    v.recorded_at = std::chrono::system_clock::now();

    auto req = synthetic::messaging::save_yield_curve_process_type_request::from(std::move(v));

    auto result = do_auth_request<synthetic::messaging::save_yield_curve_process_type_response>(
        out, session, "synthetic.v1.yield_curve_process_types.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added yield_curve_process_type.";
        out << "✓ Yield Curve Process Type added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add yield_curve_process_type: " << msg;
        fail(out) << "Failed to add yield_curve_process_type: " << msg << std::endl;
    }
}

void yield_curve_process_type_commands::process_delete_yield_curve_process_type(std::ostream& out,
                                            nats_client& session,
                                            std::string code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete yield_curve_process_type request for: " << code;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete a yield_curve_process_type." << std::endl;
        return;
    }

    synthetic::messaging::delete_yield_curve_process_type_request req;
    req.codes = {std::move(code)};

    auto result = do_auth_request<synthetic::messaging::delete_yield_curve_process_type_response>(
        out, session, "synthetic.v1.yield_curve_process_types.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted yield_curve_process_type.";
        out << "✓ Yield Curve Process Type deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete yield_curve_process_type: " << result->message;
        fail(out) << "Failed to delete yield_curve_process_type: " << result->message << std::endl;
    }
}

void yield_curve_process_type_commands::process_get_yield_curve_process_type_history(std::ostream& out,
                                                 nats_client& session,
                                                 const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {{.name = "diff", .requires_value = false, .default_value = "false"},
                                   {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: yield_curve_process_types history code [--diff] [--version <n>]" << std::endl;
        return;
    }
    auto key = parsed->positionals.front();

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
        render_history_diff(out, session, "ores.synthetic.yield_curve_process_type", std::move(key), version);
        return;
    }

    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Initiating get yield_curve_process_type history for: " << key;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get yield_curve_process_type history." << std::endl;
        return;
    }

    synthetic::messaging::get_yield_curve_process_type_history_request req;
    req.code = key;

    auto result = do_auth_request<synthetic::messaging::get_yield_curve_process_type_history_response>(
        out, session, "synthetic.v1.yield_curve_process_types.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get yield_curve_process_type history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this yield_curve_process_type." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
