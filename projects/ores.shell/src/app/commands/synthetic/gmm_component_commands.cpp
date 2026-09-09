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
#include "ores.shell/app/commands/synthetic/gmm_component_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/domain/gmm_component_table_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
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

void gmm_component_commands::register_commands(cli::Menu& root_menu,
                                               nats_client& session,
                                               pagination_context& pagination) {
    auto gmm_components_menu = std::make_unique<cli::Menu>("gmm_components");

    gmm_components_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_gmm_components(std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve gmm_components from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("gmm_components", [&session, &pagination](std::ostream& out) {
        process_get_gmm_components(out, session, pagination);
    });

    gmm_components_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string s_fx_spot_config_id,
                   std::string s_component_index,
                   std::string s_description,
                   std::string s_mean,
                   std::string s_stdev,
                   std::string s_weight,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_gmm_component(std::ref(out),
                                      std::ref(session),
                                      std::move(s_fx_spot_config_id),
                                      std::move(s_component_index),
                                      std::move(s_description),
                                      std::move(s_mean),
                                      std::move(s_stdev),
                                      std::move(s_weight),
                                      std::move(change_reason_code),
                                      std::move(change_commentary));
        },
        "Add a gmm_component (<fx_spot_config_id> <component_index> <description> <mean> <stdev> "
        "<weight> <reason_code> \"commentary\")");

    gmm_components_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string id) {
            process_delete_gmm_component(std::ref(out), std::ref(session), std::move(id));
        },
        "Delete a gmm_component by id");

    gmm_components_menu->Insert(
        "history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_gmm_component_history(std::ref(out), std::ref(session), args);
        },
        "Show a gmm_component's version history (--diff for a unified diff, --version <n> to "
        "pick one)",
        {"id [--diff] [--version <n>]"});

    root_menu.Insert(std::move(gmm_components_menu));
}

void gmm_component_commands::process_get_gmm_components(std::ostream& out,
                                                        nats_client& session,
                                                        pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get gmm_components request.";

    auto& state = pagination.state_for("gmm_components");

    synthetic::messaging::get_gmm_components_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<synthetic::messaging::get_gmm_components_response>(
        out, session, "synthetic.v1.gmm_components.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("gmm_components");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->gmm_components.size()
                              << " gmm_components.";
    out << result->gmm_components << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->gmm_components.size()
        << " of " << state.total_count << " total)" << std::endl;
}

void gmm_component_commands::process_add_gmm_component(std::ostream& out,
                                                       nats_client& session,
                                                       std::string s_fx_spot_config_id,
                                                       std::string s_component_index,
                                                       std::string s_description,
                                                       std::string s_mean,
                                                       std::string s_stdev,
                                                       std::string s_weight,
                                                       std::string change_reason_code,
                                                       std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add gmm_component request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add a gmm_component." << std::endl;
        return;
    }

    domain::gmm_component v;
    v.id = boost::uuids::random_generator{}();

    // fx_spot_config_id (uuid).
    const auto u_fx_spot_config_id = parse_uuid(s_fx_spot_config_id);
    if (!u_fx_spot_config_id) {
        fail(out) << "Invalid fx_spot_config_id: " << s_fx_spot_config_id << std::endl;
        return;
    }
    v.fx_spot_config_id = *u_fx_spot_config_id;

    // component_index (int).
    const auto n_component_index = parse_uint32(s_component_index);
    if (!n_component_index) {
        fail(out) << "Invalid component_index: " << s_component_index << std::endl;
        return;
    }
    v.component_index = static_cast<int>(*n_component_index);

    // description (str).
    v.description = std::move(s_description);

    // mean (double).
    double n_mean = 0;
    try {
        n_mean = std::stod(s_mean);
    } catch (const std::exception&) {
        fail(out) << "Invalid mean: " << s_mean << std::endl;
        return;
    }
    v.mean = n_mean;

    // stdev (double).
    double n_stdev = 0;
    try {
        n_stdev = std::stod(s_stdev);
    } catch (const std::exception&) {
        fail(out) << "Invalid stdev: " << s_stdev << std::endl;
        return;
    }
    v.stdev = n_stdev;

    // weight (double).
    double n_weight = 0;
    try {
        n_weight = std::stod(s_weight);
    } catch (const std::exception&) {
        fail(out) << "Invalid weight: " << s_weight << std::endl;
        return;
    }
    v.weight = n_weight;

    v.change_reason_code = std::move(change_reason_code);
    v.change_commentary = std::move(change_commentary);
    v.recorded_at = std::chrono::system_clock::now();

    auto req = synthetic::messaging::save_gmm_component_request::from(std::move(v));

    auto result = do_auth_request<synthetic::messaging::save_gmm_component_response>(
        out, session, "synthetic.v1.gmm_components.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added gmm_component.";
        out << "✓ Gmm Component added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add gmm_component: " << msg;
        fail(out) << "Failed to add gmm_component: " << msg << std::endl;
    }
}

void gmm_component_commands::process_delete_gmm_component(std::ostream& out,
                                                          nats_client& session,
                                                          std::string id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete gmm_component request for: " << id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete a gmm_component." << std::endl;
        return;
    }

    synthetic::messaging::delete_gmm_component_request req;
    req.ids = {std::move(id)};

    auto result = do_auth_request<synthetic::messaging::delete_gmm_component_response>(
        out, session, "synthetic.v1.gmm_components.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted gmm_component.";
        out << "✓ Gmm Component deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete gmm_component: " << result->message;
        fail(out) << "Failed to delete gmm_component: " << result->message << std::endl;
    }
}

void gmm_component_commands::process_get_gmm_component_history(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "diff", .requires_value = false, .default_value = "false"},
                              {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: gmm_components history id [--diff] [--version <n>]" << std::endl;
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
        render_history_diff(out, session, "ores.synthetic.gmm_component", std::move(key), version);
        return;
    }

    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Initiating get gmm_component history for: " << key;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get gmm_component history." << std::endl;
        return;
    }

    synthetic::messaging::get_gmm_component_history_request req;
    req.id = key;

    auto result = do_auth_request<synthetic::messaging::get_gmm_component_history_response>(
        out, session, "synthetic.v1.gmm_components.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get gmm_component history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this gmm_component." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
