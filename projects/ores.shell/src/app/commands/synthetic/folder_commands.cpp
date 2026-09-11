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
#include "ores.shell/app/commands/synthetic/folder_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.synthetic.api/domain/folder.hpp"
#include "ores.synthetic.api/domain/folder_table_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.api/messaging/folder_protocol.hpp"
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

std::optional<boost::uuids::uuid> parse_uuid(const std::string& value) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(value);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

} // namespace

void folder_commands::register_commands(cli::Menu& root_menu,
                                        nats_client& session,
                                        pagination_context& pagination) {
    auto folders_menu = std::make_unique<cli::Menu>("folders");

    folders_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_folders(std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve folders from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("folders", [&session, &pagination](std::ostream& out) {
        process_get_folders(out, session, pagination);
    });

    folders_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string s_name,
                   std::string s_kind,
                   std::string s_parent_id,
                   std::string s_collection_id,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_folder(std::ref(out),
                               std::ref(session),
                               std::move(s_name),
                               std::move(s_kind),
                               std::move(s_parent_id),
                               std::move(s_collection_id),
                               std::move(change_reason_code),
                               std::move(change_commentary));
        },
        "Add a folder (<name> <kind> <parent_id> <collection_id> <reason_code> \"commentary\")");

    folders_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string id) {
            process_delete_folder(std::ref(out), std::ref(session), std::move(id));
        },
        "Delete a folder by id");

    folders_menu->Insert(
        "history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_folder_history(std::ref(out), std::ref(session), args);
        },
        "Show a folder's version history (--diff for a unified diff, --version <n> to "
        "pick one)",
        {"id [--diff] [--version <n>]"});

    root_menu.Insert(std::move(folders_menu));
}

void folder_commands::process_get_folders(std::ostream& out,
                                          nats_client& session,
                                          pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get folders request.";

    auto& state = pagination.state_for("folders");

    synthetic::messaging::get_folders_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<synthetic::messaging::get_folders_response>(
        out, session, "synthetic.v1.folders.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("folders");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->folders.size() << " folders.";
    out << result->folders << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->folders.size() << " of "
        << state.total_count << " total)" << std::endl;
}

void folder_commands::process_add_folder(std::ostream& out,
                                         nats_client& session,
                                         std::string s_name,
                                         std::string s_kind,
                                         std::string s_parent_id,
                                         std::string s_collection_id,
                                         std::string change_reason_code,
                                         std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add folder request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add a folder." << std::endl;
        return;
    }

    domain::folder v;
    v.id = boost::uuids::random_generator{}();

    // name (str).
    v.name = std::move(s_name);

    // kind (str).
    v.kind = std::move(s_kind);

    // parent_id (uuid_opt).
    if (!s_parent_id.empty()) {
        const auto u_parent_id = parse_uuid(s_parent_id);
        if (!u_parent_id) {
            fail(out) << "Invalid parent_id: " << s_parent_id << std::endl;
            return;
        }
        v.parent_id = *u_parent_id;
    }

    // collection_id (uuid_opt).
    if (!s_collection_id.empty()) {
        const auto u_collection_id = parse_uuid(s_collection_id);
        if (!u_collection_id) {
            fail(out) << "Invalid collection_id: " << s_collection_id << std::endl;
            return;
        }
        v.collection_id = *u_collection_id;
    }

    v.change_reason_code = std::move(change_reason_code);
    v.change_commentary = std::move(change_commentary);
    v.recorded_at = std::chrono::system_clock::now();

    auto req = synthetic::messaging::save_folder_request::from(std::move(v));

    auto result = do_auth_request<synthetic::messaging::save_folder_response>(
        out, session, "synthetic.v1.folders.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added folder.";
        out << "✓ Folder added successfully!" << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add folder: " << msg;
        fail(out) << "Failed to add folder: " << msg << std::endl;
    }
}

void folder_commands::process_delete_folder(std::ostream& out,
                                            nats_client& session,
                                            std::string id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete folder request for: " << id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete a folder." << std::endl;
        return;
    }

    synthetic::messaging::delete_folder_request req;
    req.ids = {std::move(id)};

    auto result = do_auth_request<synthetic::messaging::delete_folder_response>(
        out, session, "synthetic.v1.folders.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted folder.";
        out << "✓ Folder deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete folder: " << result->message;
        fail(out) << "Failed to delete folder: " << result->message << std::endl;
    }
}

void folder_commands::process_get_folder_history(std::ostream& out,
                                                 nats_client& session,
                                                 const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "diff", .requires_value = false, .default_value = "false"},
                              {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: folders history id [--diff] [--version <n>]" << std::endl;
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
        render_history_diff(out, session, "ores.synthetic.folder", std::move(key), version);
        return;
    }

    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Initiating get folder history for: " << key;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get folder history." << std::endl;
        return;
    }

    synthetic::messaging::get_folder_history_request req;
    req.id = key;

    auto result = do_auth_request<synthetic::messaging::get_folder_history_response>(
        out, session, "synthetic.v1.folders.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get folder history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this folder." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
