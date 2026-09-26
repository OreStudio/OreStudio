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
#include "ores.shell/app/commands/history_commands.hpp"
#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include <cli/cli.h>
#include <optional>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

void history_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto history_menu = std::make_unique<cli::Menu>("history");

    history_menu->Insert(
        "get",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get(std::ref(out), std::ref(session), args);
        },
        "Show any entity's version history (--diff for a unified diff, --version <n> "
        "to pick one)",
        {"<entity_type> <entity_id> [--diff] [--version <n>]"});

    root_menu.Insert(std::move(history_menu));
}

void history_commands::process_get(std::ostream& out,
                                   nats_client& session,
                                   const std::vector<std::string>& args) {
    using namespace ores::history::messaging;

    auto parsed = parse_args(args,
                             {{.name = "diff", .requires_value = false, .default_value = "false"},
                              {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 2) {
        fail(out) << "Usage: history get <entity_type> <entity_id> [--diff] "
                     "[--version <n>]"
                  << std::endl;
        fail(out) << "For example: history get ores.workflow.workflow_instance <uuid>"
                  << std::endl;
        return;
    }

    const auto& entity_type = parsed->positionals[0];
    const auto& entity_id = parsed->positionals[1];

    std::optional<int> version;
    if (const auto& raw_version = parsed->flag("version"); !raw_version.empty()) {
        const auto parsed_version = parse_uint32(raw_version);
        if (!parsed_version) {
            fail(out) << "Invalid --version value: " << raw_version << std::endl;
            return;
        }
        version = static_cast<int>(*parsed_version);
    }

    // The diff path is already shared by the per-entity history commands, so the
    // one thing this unit adds is the listing beside it.
    if (parsed->flag_set("diff")) {
        render_history_diff(out, session, entity_type, entity_id, version);
        return;
    }
    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Initiating history read for " << entity_type << " "
                               << entity_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to read history." << std::endl;
        return;
    }

    get_entity_history_request req;
    req.entity_type = entity_type;
    req.entity_id = entity_id;

    auto result = do_auth_request<get_entity_history_response>(
        out, session, history_subject_for(req.entity_type), req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to read history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    const auto& versions = result->versions;
    if (versions.empty()) {
        out << "No history recorded for " << entity_id << "." << std::endl;
        return;
    }

    out << "History of " << entity_id << " (" << versions.size() << " version(s)):" << std::endl;
    for (const auto& v : versions) {
        out << "  v" << v.version << "  "
            << ores::platform::time::datetime::to_iso8601_utc(v.recorded_at) << "  "
            << v.modified_by << "  " << v.fields.size() << " field(s)" << std::endl;
    }
    out << "Run with --diff to see what changed, and --version <n> to pick the version "
           "diffed against its predecessor."
        << std::endl;
}

}
