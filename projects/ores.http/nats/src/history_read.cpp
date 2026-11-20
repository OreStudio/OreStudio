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
#include "ores.http.nats/history_read.hpp"
#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.http.nats/forwarding.hpp"
#include <algorithm>

namespace ores::http::net::history {

using ores::nats::service::nats_client;

answer read(const ores::http::domain::http_request& req,
            nats_client& session,
            std::string_view entity_type,
            std::string_view entity_id) {
    ores::history::messaging::get_entity_history_request msg;
    msg.entity_type = std::string(entity_type);
    msg.entity_id = std::string(entity_id);
    const auto reply = forwarding::forward_on(
        req, session, ores::history::messaging::history_subject_for(msg.entity_type), msg);

    answer out;
    out.answered = reply.success;
    out.message = reply.message;
    out.versions.reserve(reply.versions.size());
    for (const auto& answered : reply.versions) {
        version row;
        row.version = answered.version;
        row.modified_by = answered.modified_by;
        row.recorded_at = answered.recorded_at;
        for (const auto& stamped : answered.fields)
            row.fields.push_back({stamped.name, stamped.value});
        for (const auto& entry : answered.changes.entries)
            row.changes.push_back({entry.field_name, entry.old_value, entry.new_value});
        out.versions.push_back(std::move(row));
    }
    std::ranges::sort(out.versions, std::ranges::greater{}, &version::version);
    return out;
}

}
