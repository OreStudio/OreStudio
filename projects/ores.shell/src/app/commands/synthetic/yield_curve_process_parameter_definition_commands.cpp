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
#include "ores.shell/app/commands/synthetic/yield_curve_process_parameter_definition_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition_table_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.api/messaging/yield_curve_process_parameter_definition_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
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

void yield_curve_process_parameter_definition_commands::register_commands(cli::Menu& root_menu,
                                        nats_client& session,
                                        pagination_context& pagination) {
    auto yield_curve_process_parameter_definitions_menu = std::make_unique<cli::Menu>("yield_curve_process_parameter_definitions");

    yield_curve_process_parameter_definitions_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_yield_curve_process_parameter_definitions(std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve yield_curve_process_parameter_definitions from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("yield_curve_process_parameter_definitions",
                                      [&session, &pagination](std::ostream& out) {
                                          process_get_yield_curve_process_parameter_definitions(out, session, pagination);
                                      });

    yield_curve_process_parameter_definitions_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string s_process_type_code,
                   std::string s_parameter_name,
                   std::string s_display_name,
                   std::string s_symbol,
                   std::string s_short_label,
                   std::string s_description,
                   std::string s_data_type,
                   std::string s_default_value,
                   std::string s_min_value,
                   std::string s_max_value,
                   std::string s_display_order,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_yield_curve_process_parameter_definition(std::ref(out),
                               std::ref(session),
                               std::move(s_process_type_code),
                               std::move(s_parameter_name),
                               std::move(s_display_name),
                               std::move(s_symbol),
                               std::move(s_short_label),
                               std::move(s_description),
                               std::move(s_data_type),
                               std::move(s_default_value),
                               std::move(s_min_value),
                               std::move(s_max_value),
                               std::move(s_display_order),
                               std::move(change_reason_code),
                               std::move(change_commentary));
        },
        "Add a yield_curve_process_parameter_definition (<process_type_code> <parameter_name> <display_name> <symbol> <short_label> <description> <data_type> <default_value> <min_value> <max_value> <display_order> <reason_code> \"commentary\")");

    yield_curve_process_parameter_definitions_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string id) {
            process_delete_yield_curve_process_parameter_definition(std::ref(out), std::ref(session), std::move(id));
        },
        "Delete a yield_curve_process_parameter_definition by id");

    yield_curve_process_parameter_definitions_menu->Insert(
        "history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_yield_curve_process_parameter_definition_history(std::ref(out), std::ref(session), args);
        },
        "Show a yield_curve_process_parameter_definition's version history (--diff for a unified diff, --version <n> to "
        "pick one)",
        {"id [--diff] [--version <n>]"});

    root_menu.Insert(std::move(yield_curve_process_parameter_definitions_menu));
}

void yield_curve_process_parameter_definition_commands::process_get_yield_curve_process_parameter_definitions(std::ostream& out,
                                           nats_client& session,
                                           pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get yield_curve_process_parameter_definitions request.";

    auto& state = pagination.state_for("yield_curve_process_parameter_definitions");

    synthetic::messaging::get_yield_curve_process_parameter_definitions_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<synthetic::messaging::get_yield_curve_process_parameter_definitions_response>(
        out, session, "synthetic.v1.yield_curve_process_parameter_definitions.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("yield_curve_process_parameter_definitions");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->parameter_definitions.size()
                              << " yield_curve_process_parameter_definitions.";
    out << result->parameter_definitions << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->parameter_definitions.size()
        << " of " << state.total_count << " total)" << std::endl;
}

void yield_curve_process_parameter_definition_commands::process_add_yield_curve_process_parameter_definition(std::ostream& out,
                                         nats_client& session
                                         ,
                                   std::string s_process_type_code,
                                   std::string s_parameter_name,
                                   std::string s_display_name,
                                   std::string s_symbol,
                                   std::string s_short_label,
                                   std::string s_description,
                                   std::string s_data_type,
                                   std::string s_default_value,
                                   std::string s_min_value,
                                   std::string s_max_value,
                                   std::string s_display_order,
                                   std::string change_reason_code,
                                   std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add yield_curve_process_parameter_definition request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add a yield_curve_process_parameter_definition." << std::endl;
        return;
    }

    domain::yield_curve_process_parameter_definition v;
    v.id = boost::uuids::random_generator{}();

    // process_type_code (str).
    v.process_type_code = std::move(s_process_type_code);

    // parameter_name (str).
    v.parameter_name = std::move(s_parameter_name);

    // display_name (str).
    v.display_name = std::move(s_display_name);

    // symbol (str_opt).
    if (!s_symbol.empty())
        v.symbol = std::move(s_symbol);

    // short_label (str).
    v.short_label = std::move(s_short_label);

    // description (str).
    v.description = std::move(s_description);

    // data_type (str).
    v.data_type = std::move(s_data_type);

    // default_value (double).
    double n_default_value = 0;
    try {
        n_default_value = std::stod(s_default_value);
    } catch (const std::exception&) {
        fail(out) << "Invalid default_value: " << s_default_value << std::endl;
        return;
    }
    v.default_value = n_default_value;

    // min_value (double_opt).
    if (!s_min_value.empty()) {
        double n_min_value = 0;
        try {
            n_min_value = std::stod(s_min_value);
        } catch (const std::exception&) {
            fail(out) << "Invalid min_value: " << s_min_value << std::endl;
            return;
        }
        v.min_value = n_min_value;
    }

    // max_value (double_opt).
    if (!s_max_value.empty()) {
        double n_max_value = 0;
        try {
            n_max_value = std::stod(s_max_value);
        } catch (const std::exception&) {
            fail(out) << "Invalid max_value: " << s_max_value << std::endl;
            return;
        }
        v.max_value = n_max_value;
    }

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

    auto req = synthetic::messaging::save_yield_curve_process_parameter_definition_request::from(std::move(v));

    auto result = do_auth_request<synthetic::messaging::save_yield_curve_process_parameter_definition_response>(
        out, session, "synthetic.v1.yield_curve_process_parameter_definitions.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added yield_curve_process_parameter_definition.";
        out << "✓ Yield Curve Process Parameter Definition added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add yield_curve_process_parameter_definition: " << msg;
        fail(out) << "Failed to add yield_curve_process_parameter_definition: " << msg << std::endl;
    }
}

void yield_curve_process_parameter_definition_commands::process_delete_yield_curve_process_parameter_definition(std::ostream& out,
                                            nats_client& session,
                                            std::string id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete yield_curve_process_parameter_definition request for: " << id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete a yield_curve_process_parameter_definition." << std::endl;
        return;
    }

    synthetic::messaging::delete_yield_curve_process_parameter_definition_request req;
    req.ids = {std::move(id)};

    auto result = do_auth_request<synthetic::messaging::delete_yield_curve_process_parameter_definition_response>(
        out, session, "synthetic.v1.yield_curve_process_parameter_definitions.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted yield_curve_process_parameter_definition.";
        out << "✓ Yield Curve Process Parameter Definition deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete yield_curve_process_parameter_definition: " << result->message;
        fail(out) << "Failed to delete yield_curve_process_parameter_definition: " << result->message << std::endl;
    }
}

void yield_curve_process_parameter_definition_commands::process_get_yield_curve_process_parameter_definition_history(std::ostream& out,
                                                 nats_client& session,
                                                 const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {{.name = "diff", .requires_value = false, .default_value = "false"},
                                   {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: yield_curve_process_parameter_definitions history id [--diff] [--version <n>]" << std::endl;
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
        render_history_diff(out, session, "ores.synthetic.yield_curve_process_parameter_definition", std::move(key), version);
        return;
    }

    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Initiating get yield_curve_process_parameter_definition history for: " << key;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get yield_curve_process_parameter_definition history." << std::endl;
        return;
    }

    synthetic::messaging::get_yield_curve_process_parameter_definition_history_request req;
    req.id = key;

    auto result = do_auth_request<synthetic::messaging::get_yield_curve_process_parameter_definition_history_response>(
        out, session, "synthetic.v1.yield_curve_process_parameter_definitions.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get yield_curve_process_parameter_definition history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this yield_curve_process_parameter_definition." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
