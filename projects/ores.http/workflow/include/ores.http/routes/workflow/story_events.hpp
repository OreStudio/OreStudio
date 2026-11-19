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
#ifndef ORES_HTTP_ROUTES_WORKFLOW_STORY_EVENTS_HPP
#define ORES_HTTP_ROUTES_WORKFLOW_STORY_EVENTS_HPP

#include "ores.http.nats/history_read.hpp"
#include <chrono>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

/**
 * @brief The entry a story is made of, and the two rules every story shares.
 *
 * A story is one subject's whole record in one stream: every row any component
 * wrote for it, newest first, whether the row was a field change or an act that
 * changed no field. The shape is not a request's and it is not a person's: the
 * subjects differ in the producer that fills the stream, not in the stream, so
 * the entry and its ordering live here once rather than beside each subject.
 */
namespace ores::http::routes::workflow {

/// One thing that happened to a subject, before it is ordered or written.
struct story_event final {
    std::string entity_type;
    std::string entity_id;
    std::string kind;
    std::chrono::system_clock::time_point at;
    std::string actor;
    int version = 0;
    std::string reason_code;
    std::string commentary;
    std::vector<std::pair<std::string, std::string>> fields;
};

/**
 * @brief How late in a second an event of each kind lands.
 *
 * The timestamps on the wire carry whole seconds, so ties are the common case
 * rather than the exception, and the order among them reads as the order the
 * acts happened in: a request is raised, its roles are recorded, notices go out,
 * an answer is given, the role is applied; a link is opened before it is closed.
 */
[[nodiscard]] int story_kind_rank(std::string_view kind);

/// Newest first, and within one second, the later act first.
void order_story(std::vector<story_event>& events);

/// One event, as the stream reads it.
[[nodiscard]] std::string story_event_json(const story_event& event);

/// One field's value, or the empty string when the event does not carry it.
[[nodiscard]] std::string story_field(const story_event& event, std::string_view name);

/**
 * @brief The entries one versioned record's history makes.
 *
 * Every record counts its versions from its own first one, and the history
 * numbers them newest first, so the version number goes on the entry as it
 * arrived. The reason and the commentary are stamped on the version as fields,
 * so they are read back out of them here and shown as the entry's own facts.
 */
[[nodiscard]] std::vector<story_event>
story_record_events(std::string_view entity_type,
                    std::string entity_id,
                    const std::vector<ores::http::net::history::version>& versions);

/**
 * @brief The entries a link table's history makes.
 *
 * A link has two keys, so the gateway reads its history by the parent and
 * answers every version of every link of that parent, each carrying the pair as
 * its =ID= field. The versions are grouped by that pair, and each link is one row
 * of the stream: its opening, each change, and its closing. A link is closed by
 * ending its validity rather than by writing a version, so the closing is read
 * off the fields the server states: when the link ended and who ended it. Those
 * two belong to the closing and not to the version that carries them, so they
 * are left out of the other entries.
 */
[[nodiscard]] std::vector<story_event>
story_link_events(std::string_view entity_type,
                  const std::vector<ores::http::net::history::version>& versions);

}

#endif
