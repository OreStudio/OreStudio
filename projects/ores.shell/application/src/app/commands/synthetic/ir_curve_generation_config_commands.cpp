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
#include "ores.shell/app/commands/synthetic/ir_curve_generation_config_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_table_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
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

void ir_curve_generation_config_commands::register_commands(cli::Menu& root_menu,
                                                            nats_client& session,
                                                            pagination_context& pagination) {
    auto ir_curve_generation_configs_menu =
        std::make_unique<cli::Menu>("ir_curve_generation_configs");

    ir_curve_generation_configs_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_ir_curve_generation_configs(
                std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve ir_curve_generation_configs from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback(
        "ir_curve_generation_configs", [&session, &pagination](std::ostream& out) {
            process_get_ir_curve_generation_configs(out, session, pagination);
        });

    ir_curve_generation_configs_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string s_config_id,
                   std::string s_currency_code,
                   std::string s_index_family,
                   std::string s_tenor,
                   std::string s_role,
                   std::string s_process_type,
                   std::string s_ticks_per_hour,
                   std::string s_enabled,
                   std::string s_auto_start,
                   std::string s_price_source,
                   std::string s_vintage_source,
                   std::string s_vintage_date,
                   std::string s_description,
                   std::string s_fixed_leg_payment_frequency_code,
                   std::string s_source_name,
                   std::string s_folder_id,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_ir_curve_generation_config(std::ref(out),
                                                   std::ref(session),
                                                   std::move(s_config_id),
                                                   std::move(s_currency_code),
                                                   std::move(s_index_family),
                                                   std::move(s_tenor),
                                                   std::move(s_role),
                                                   std::move(s_process_type),
                                                   std::move(s_ticks_per_hour),
                                                   std::move(s_enabled),
                                                   std::move(s_auto_start),
                                                   std::move(s_price_source),
                                                   std::move(s_vintage_source),
                                                   std::move(s_vintage_date),
                                                   std::move(s_description),
                                                   std::move(s_fixed_leg_payment_frequency_code),
                                                   std::move(s_source_name),
                                                   std::move(s_folder_id),
                                                   std::move(change_reason_code),
                                                   std::move(change_commentary));
        },
        "Add a ir_curve_generation_config (<config_id> <currency_code> <index_family> <tenor> "
        "<role> <process_type> <ticks_per_hour> <enabled> <auto_start> <price_source> "
        "<vintage_source> <vintage_date> <description> <fixed_leg_payment_frequency_code> "
        "<source_name> <folder_id> <reason_code> \"commentary\")");

    ir_curve_generation_configs_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string id) {
            process_delete_ir_curve_generation_config(
                std::ref(out), std::ref(session), std::move(id));
        },
        "Delete a ir_curve_generation_config by id");

    ir_curve_generation_configs_menu->Insert(
        "history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_ir_curve_generation_config_history(std::ref(out), std::ref(session), args);
        },
        "Show a ir_curve_generation_config's version history (--diff for a unified diff, --version "
        "<n> to "
        "pick one)",
        {"id [--diff] [--version <n>]"});

    root_menu.Insert(std::move(ir_curve_generation_configs_menu));
}

void ir_curve_generation_config_commands::process_get_ir_curve_generation_configs(
    std::ostream& out, nats_client& session, pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get ir_curve_generation_configs request.";

    auto& state = pagination.state_for("ir_curve_generation_configs");

    synthetic::messaging::get_ir_curve_generation_configs_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<synthetic::messaging::get_ir_curve_generation_configs_response>(
        out, session, "synthetic.v1.ir_curve_generation_configs.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("ir_curve_generation_configs");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->ir_curve_generation_configs.size()
                              << " ir_curve_generation_configs.";
    out << result->ir_curve_generation_configs << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " ("
        << result->ir_curve_generation_configs.size() << " of " << state.total_count << " total)"
        << std::endl;
}

void ir_curve_generation_config_commands::process_add_ir_curve_generation_config(
    std::ostream& out,
    nats_client& session,
    std::string s_config_id,
    std::string s_currency_code,
    std::string s_index_family,
    std::string s_tenor,
    std::string s_role,
    std::string s_process_type,
    std::string s_ticks_per_hour,
    std::string s_enabled,
    std::string s_auto_start,
    std::string s_price_source,
    std::string s_vintage_source,
    std::string s_vintage_date,
    std::string s_description,
    std::string s_fixed_leg_payment_frequency_code,
    std::string s_source_name,
    std::string s_folder_id,
    std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add ir_curve_generation_config request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add a ir_curve_generation_config." << std::endl;
        return;
    }

    domain::ir_curve_generation_config v;
    v.id = boost::uuids::random_generator{}();

    // config_id (uuid).
    const auto u_config_id = parse_uuid(s_config_id);
    if (!u_config_id) {
        fail(out) << "Invalid config_id: " << s_config_id << std::endl;
        return;
    }
    v.config_id = *u_config_id;

    // currency_code (str).
    v.currency_code = std::move(s_currency_code);

    // index_family (str).
    v.index_family = std::move(s_index_family);

    // tenor (str).
    v.tenor = std::move(s_tenor);

    // role (str).
    v.role = std::move(s_role);

    // process_type (str).
    v.process_type = std::move(s_process_type);

    // ticks_per_hour (int).
    const auto n_ticks_per_hour = parse_uint32(s_ticks_per_hour);
    if (!n_ticks_per_hour) {
        fail(out) << "Invalid ticks_per_hour: " << s_ticks_per_hour << std::endl;
        return;
    }
    v.ticks_per_hour = static_cast<int>(*n_ticks_per_hour);

    // enabled (bool).
    bool b_enabled = false;
    if (!parse_flag(s_enabled, b_enabled)) {
        fail(out) << "Invalid enabled: " << s_enabled << std::endl;
        return;
    }
    v.enabled = b_enabled;

    // auto_start (bool).
    bool b_auto_start = false;
    if (!parse_flag(s_auto_start, b_auto_start)) {
        fail(out) << "Invalid auto_start: " << s_auto_start << std::endl;
        return;
    }
    v.auto_start = b_auto_start;

    // price_source (str).
    v.price_source = std::move(s_price_source);

    // vintage_source (str).
    v.vintage_source = std::move(s_vintage_source);

    // vintage_date (str).
    v.vintage_date = std::move(s_vintage_date);

    // description (str).
    v.description = std::move(s_description);

    // fixed_leg_payment_frequency_code (str).
    v.fixed_leg_payment_frequency_code = std::move(s_fixed_leg_payment_frequency_code);

    // source_name (str).
    v.source_name = std::move(s_source_name);

    // folder_id (uuid_opt).
    if (!s_folder_id.empty()) {
        const auto u_folder_id = parse_uuid(s_folder_id);
        if (!u_folder_id) {
            fail(out) << "Invalid folder_id: " << s_folder_id << std::endl;
            return;
        }
        v.folder_id = *u_folder_id;
    }

    v.change_reason_code = std::move(change_reason_code);
    v.change_commentary = std::move(change_commentary);
    v.recorded_at = std::chrono::system_clock::now();

    auto req = synthetic::messaging::save_ir_curve_generation_config_request::from(std::move(v));

    auto result = do_auth_request<synthetic::messaging::save_ir_curve_generation_config_response>(
        out, session, "synthetic.v1.ir_curve_generation_configs.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added ir_curve_generation_config.";
        out << "✓ Ir Curve Generation Config added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add ir_curve_generation_config: " << msg;
        fail(out) << "Failed to add ir_curve_generation_config: " << msg << std::endl;
    }
}

void ir_curve_generation_config_commands::process_delete_ir_curve_generation_config(
    std::ostream& out, nats_client& session, std::string id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete ir_curve_generation_config request for: "
                               << id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete a ir_curve_generation_config." << std::endl;
        return;
    }

    synthetic::messaging::delete_ir_curve_generation_config_request req;
    req.ids = {std::move(id)};

    auto result = do_auth_request<synthetic::messaging::delete_ir_curve_generation_config_response>(
        out, session, "synthetic.v1.ir_curve_generation_configs.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted ir_curve_generation_config.";
        out << "✓ Ir Curve Generation Config deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete ir_curve_generation_config: "
                                  << result->message;
        fail(out) << "Failed to delete ir_curve_generation_config: " << result->message
                  << std::endl;
    }
}

void ir_curve_generation_config_commands::process_get_ir_curve_generation_config_history(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "diff", .requires_value = false, .default_value = "false"},
                              {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: ir_curve_generation_configs history id [--diff] [--version <n>]"
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
        render_history_diff(
            out, session, "ores.synthetic.ir_curve_generation_config", std::move(key), version);
        return;
    }

    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Initiating get ir_curve_generation_config history for: " << key;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get ir_curve_generation_config history."
                  << std::endl;
        return;
    }

    synthetic::messaging::get_ir_curve_generation_config_history_request req;
    req.id = key;

    auto result =
        do_auth_request<synthetic::messaging::get_ir_curve_generation_config_history_response>(
            out, session, "synthetic.v1.ir_curve_generation_configs.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get ir_curve_generation_config history: "
                                  << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this ir_curve_generation_config." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
