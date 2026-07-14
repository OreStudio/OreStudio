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
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include <algorithm>
#include <rfl/json.hpp>

namespace ores::shell::app::commands {

namespace {

inline auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.shell.app.commands.history_diff_renderer");
    return instance;
}

std::string version_header(const ores::history::messaging::entity_history_version& v) {
    return "v" + std::to_string(v.version) + " (" + v.modified_by + ")";
}

} // namespace

void render_history_diff(std::ostream& out,
                         ores::nats::service::nats_client& session,
                         std::string_view entity_type,
                         std::string entity_id,
                         std::optional<int> version) {
    using namespace ores::logging;
    using ores::history::messaging::get_entity_history_request;
    using ores::history::messaging::get_entity_history_response;

    BOOST_LOG_SEV(lg(), debug) << "Initiating history diff for " << entity_type << " " << entity_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get history." << std::endl;
        return;
    }

    get_entity_history_request req;
    req.entity_type = std::string(entity_type);
    req.entity_id = std::move(entity_id);

    auto reply = session.authenticated_request(get_entity_history_request::nats_subject,
                                               rfl::json::write(req));
    auto data_str =
        std::string(reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
    auto result = rfl::json::read<get_entity_history_response>(data_str);
    if (!result) {
        fail(out) << "Failed to parse response" << std::endl;
        return;
    }

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    const auto& versions = result->versions;
    if (versions.empty()) {
        out << "No history found for this entity." << std::endl;
        return;
    }

    // Versions arrive newest first; default to the latest.
    const auto* target = &versions.front();
    if (version) {
        const auto it = std::find_if(
            versions.begin(), versions.end(), [&](const auto& v) { return v.version == *version; });
        if (it == versions.end()) {
            fail(out) << "Version " << *version << " not found for " << req.entity_id << std::endl;
            return;
        }
        target = &*it;
    }

    const auto prev_it = std::find_if(versions.begin(), versions.end(), [&](const auto& v) {
        return v.version == target->version - 1;
    });
    const bool has_predecessor = prev_it != versions.end();

    if (has_predecessor)
        out << "--- " << req.entity_id << " " << version_header(*prev_it) << "\n";
    else
        out << "--- /dev/null\n";
    out << "+++ " << req.entity_id << " " << version_header(*target) << "\n";

    if (!has_predecessor) {
        // Initial version: every field is an addition.
        for (const auto& f : target->fields)
            out << "+" << f.name << ": " << f.value << "\n";
    } else if (target->changes.entries.empty()) {
        out << "(no field changes recorded for this version)\n";
    } else {
        // Unchanged fields as context, changed fields as -/+ pairs, in the
        // current version's field order; fields the current version no
        // longer has (removed) are appended last, in changes order.
        for (const auto& f : target->fields) {
            const auto entry = std::find_if(target->changes.entries.begin(),
                                            target->changes.entries.end(),
                                            [&](const auto& e) { return e.field_name == f.name; });
            if (entry == target->changes.entries.end()) {
                out << " " << f.name << ": " << f.value << "\n";
            } else {
                const auto in_predecessor =
                    std::any_of(prev_it->fields.begin(),
                                prev_it->fields.end(),
                                [&](const auto& pf) { return pf.name == f.name; });
                if (in_predecessor)
                    out << "-" << entry->field_name << ": " << entry->old_value << "\n";
                out << "+" << entry->field_name << ": " << entry->new_value << "\n";
            }
        }
        for (const auto& e : target->changes.entries) {
            const auto in_fields =
                std::any_of(target->fields.begin(), target->fields.end(), [&](const auto& f) {
                    return f.name == e.field_name;
                });
            if (!in_fields)
                out << "-" << e.field_name << ": " << e.old_value << "\n";
        }
    }
    out << std::flush;
}

}
