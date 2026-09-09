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
#include "ores.shell/app/commands/synthetic/ir_curve_generation_config_process_parameter_value_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value_table_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.api/messaging/ir_curve_generation_config_process_parameter_value_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <chrono>
#include <cli/cli.h>
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

std::optional<boost::uuids::uuid> parse_uuid(const std::string& value) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(value);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

} // namespace

void ir_curve_generation_config_process_parameter_value_commands::register_commands(
    cli::Menu& root_menu, nats_client& session, pagination_context& pagination) {
    auto ir_curve_generation_config_process_parameter_values_menu =
        std::make_unique<cli::Menu>("ir_curve_generation_config_process_parameter_values");

    ir_curve_generation_config_process_parameter_values_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_ir_curve_generation_config_process_parameter_values(
                std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve ir_curve_generation_config_process_parameter_values from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback(
        "ir_curve_generation_config_process_parameter_values",
        [&session, &pagination](std::ostream& out) {
            process_get_ir_curve_generation_config_process_parameter_values(
                out, session, pagination);
        });

    ir_curve_generation_config_process_parameter_values_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string s_config_id,
                   std::string s_parameter_definition_id,
                   std::string s_parameter_value,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_ir_curve_generation_config_process_parameter_value(
                std::ref(out),
                std::ref(session),
                std::move(s_config_id),
                std::move(s_parameter_definition_id),
                std::move(s_parameter_value),
                std::move(change_reason_code),
                std::move(change_commentary));
        },
        "Add a ir_curve_generation_config_process_parameter_value (<config_id> "
        "<parameter_definition_id> <parameter_value> <reason_code> \"commentary\")");

    ir_curve_generation_config_process_parameter_values_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string id) {
            process_delete_ir_curve_generation_config_process_parameter_value(
                std::ref(out), std::ref(session), std::move(id));
        },
        "Delete a ir_curve_generation_config_process_parameter_value by id");

    ir_curve_generation_config_process_parameter_values_menu->Insert(
        "history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_ir_curve_generation_config_process_parameter_value_history(
                std::ref(out), std::ref(session), args);
        },
        "Show a ir_curve_generation_config_process_parameter_value's version history (--diff for a "
        "unified diff, --version <n> to "
        "pick one)",
        {"id [--diff] [--version <n>]"});

    root_menu.Insert(std::move(ir_curve_generation_config_process_parameter_values_menu));
}

void ir_curve_generation_config_process_parameter_value_commands::
    process_get_ir_curve_generation_config_process_parameter_values(
        std::ostream& out, nats_client& session, pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug)
        << "Initiating get ir_curve_generation_config_process_parameter_values request.";

    auto& state = pagination.state_for("ir_curve_generation_config_process_parameter_values");

    synthetic::messaging::get_ir_curve_generation_config_process_parameter_values_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<
        synthetic::messaging::get_ir_curve_generation_config_process_parameter_values_response>(
        out, session, "synthetic.v1.ir_curve_generation_config_process_parameter_values.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("ir_curve_generation_config_process_parameter_values");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->process_parameter_values.size()
                              << " ir_curve_generation_config_process_parameter_values.";
    out << result->process_parameter_values << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " ("
        << result->process_parameter_values.size() << " of " << state.total_count << " total)"
        << std::endl;
}

void ir_curve_generation_config_process_parameter_value_commands::
    process_add_ir_curve_generation_config_process_parameter_value(
        std::ostream& out,
        nats_client& session,
        std::string s_config_id,
        std::string s_parameter_definition_id,
        std::string s_parameter_value,
        std::string change_reason_code,
        std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug)
        << "Initiating add ir_curve_generation_config_process_parameter_value request.";

    if (!session.is_logged_in()) {
        fail(out)
            << "You must be logged in to add a ir_curve_generation_config_process_parameter_value."
            << std::endl;
        return;
    }

    domain::ir_curve_generation_config_process_parameter_value v;
    v.id = boost::uuids::random_generator{}();

    // config_id (uuid).
    const auto u_config_id = parse_uuid(s_config_id);
    if (!u_config_id) {
        fail(out) << "Invalid config_id: " << s_config_id << std::endl;
        return;
    }
    v.config_id = *u_config_id;

    // parameter_definition_id (uuid).
    const auto u_parameter_definition_id = parse_uuid(s_parameter_definition_id);
    if (!u_parameter_definition_id) {
        fail(out) << "Invalid parameter_definition_id: " << s_parameter_definition_id << std::endl;
        return;
    }
    v.parameter_definition_id = *u_parameter_definition_id;

    // parameter_value (double).
    double n_parameter_value = 0;
    try {
        n_parameter_value = std::stod(s_parameter_value);
    } catch (const std::exception&) {
        fail(out) << "Invalid parameter_value: " << s_parameter_value << std::endl;
        return;
    }
    v.parameter_value = n_parameter_value;

    v.change_reason_code = std::move(change_reason_code);
    v.change_commentary = std::move(change_commentary);
    v.recorded_at = std::chrono::system_clock::now();

    auto req =
        synthetic::messaging::save_ir_curve_generation_config_process_parameter_value_request::from(
            std::move(v));

    auto result = do_auth_request<
        synthetic::messaging::save_ir_curve_generation_config_process_parameter_value_response>(
        out, session, "synthetic.v1.ir_curve_generation_config_process_parameter_values.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info)
            << "Successfully added ir_curve_generation_config_process_parameter_value.";
        out << "✓ Ir Curve Generation Config Process Parameter Value added successfully!"
            << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to add ir_curve_generation_config_process_parameter_value: " << msg;
        fail(out) << "Failed to add ir_curve_generation_config_process_parameter_value: " << msg
                  << std::endl;
    }
}

void ir_curve_generation_config_process_parameter_value_commands::
    process_delete_ir_curve_generation_config_process_parameter_value(std::ostream& out,
                                                                      nats_client& session,
                                                                      std::string id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Initiating delete ir_curve_generation_config_process_parameter_value request for: "
        << id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete a "
                     "ir_curve_generation_config_process_parameter_value."
                  << std::endl;
        return;
    }

    synthetic::messaging::delete_ir_curve_generation_config_process_parameter_value_request req;
    req.ids = {std::move(id)};

    auto result = do_auth_request<
        synthetic::messaging::delete_ir_curve_generation_config_process_parameter_value_response>(
        out,
        session,
        "synthetic.v1.ir_curve_generation_config_process_parameter_values.delete",
        req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info)
            << "Successfully deleted ir_curve_generation_config_process_parameter_value.";
        out << "✓ Ir Curve Generation Config Process Parameter Value deleted successfully!"
            << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to delete ir_curve_generation_config_process_parameter_value: "
            << result->message;
        fail(out) << "Failed to delete ir_curve_generation_config_process_parameter_value: "
                  << result->message << std::endl;
    }
}

void ir_curve_generation_config_process_parameter_value_commands::
    process_get_ir_curve_generation_config_process_parameter_value_history(
        std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "diff", .requires_value = false, .default_value = "false"},
                              {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: ir_curve_generation_config_process_parameter_values history id "
                     "[--diff] [--version <n>]"
                  << std::endl;
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
        render_history_diff(out,
                            session,
                            "ores.synthetic.ir_curve_generation_config_process_parameter_value",
                            std::move(key),
                            version);
        return;
    }

    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Initiating get ir_curve_generation_config_process_parameter_value history for: " << key;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get "
                     "ir_curve_generation_config_process_parameter_value history."
                  << std::endl;
        return;
    }

    synthetic::messaging::get_ir_curve_generation_config_process_parameter_value_history_request
        req;
    req.id = key;

    auto result = do_auth_request<
        synthetic::messaging::
            get_ir_curve_generation_config_process_parameter_value_history_response>(
        out,
        session,
        "synthetic.v1.ir_curve_generation_config_process_parameter_values.history",
        req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to get ir_curve_generation_config_process_parameter_value history: "
            << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this ir_curve_generation_config_process_parameter_value."
            << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
