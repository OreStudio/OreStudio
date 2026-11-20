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
#ifndef ORES_HTTP_ROUTES_WORKFLOW_TIMELINE_SOURCES_HPP
#define ORES_HTTP_ROUTES_WORKFLOW_TIMELINE_SOURCES_HPP

#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http/routes/workflow/timeline.hpp"
#include <functional>
#include <map>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * @brief What every subject of the timeline reads its sources through.
 *
 * Two rules are shared by the subjects rather than stated by each.
 *
 * A refusal the transport carries is a gap when it says the caller may not make
 * the read, and a fault otherwise: a service that is down is never reported as a
 * permission the person lacks. A read the service declines in a reply it did send
 * is a gap too, and the gap states that the service did not answer rather than
 * why, because the reply carries one flag for three different pieces of news.
 *
 * And a row that one entry names by id is named by its own name where the stream
 * can read it, and by its id where it cannot, with a gap stating that the entry
 * shows an id.
 */
namespace ores::http::routes::workflow {

/**
 * @brief Runs one source of the stream, recording a gap when this caller may
 * not read it.
 *
 * Only a refusal becomes a gap, so a service that is down is never reported as a
 * permission the person lacks. A source that answers with its own words rather
 * than a refusal is the caller's to inspect, and versions_or_gap states that
 * case for the history read.
 */
template <typename Source>
auto or_gap(std::vector<timeline_gap>& gaps,
            std::string_view entity,
            std::string_view reason,
            Source&& source) -> std::invoke_result_t<Source> {
    try {
        return std::invoke(std::forward<Source>(source));
    } catch (const ores::http::net::record_routes::refusal& refusal) {
        if (refusal.status != ores::http::domain::http_status::forbidden &&
            refusal.status != ores::http::domain::http_status::unauthorized)
            throw;
        gaps.push_back({std::string(entity), std::string(reason)});
        return {};
    }
}

/// The versions one record's history holds, or a gap when the service holds no
/// read for it or refuses this caller the one it holds.
[[nodiscard]] std::vector<ores::http::net::history::version>
versions_or_gap(std::vector<timeline_gap>& gaps,
                std::string_view entity,
                std::string_view reason,
                const ores::http::domain::http_request& req,
                ores::nats::service::nats_client& session,
                std::string_view entity_type,
                std::string_view entity_id);

/// The parties a set of ids names. A read that does not answer leaves every
/// entry with its id and states a gap, so a stream of ids is never silent.
[[nodiscard]] std::map<std::string, std::string>
party_names(const ores::http::domain::http_request& req,
            ores::nats::service::nats_client& session,
            const std::set<std::string>& ids,
            std::vector<timeline_gap>& gaps);

/// The counterparties a set of ids names.
[[nodiscard]] std::map<std::string, std::string>
counterparty_names(const ores::http::domain::http_request& req,
                   ores::nats::service::nats_client& session,
                   const std::set<std::string>& ids,
                   std::vector<timeline_gap>& gaps);

/// The events one field names by id, with each id its name where the stream read
/// one and the id where it did not.
[[nodiscard]] std::vector<story_event> with_names(const std::vector<story_event>& events,
                                                  std::string_view field,
                                                  const std::map<std::string, std::string>& names);

/// The ids one field of these events holds, without repeats and without blanks.
[[nodiscard]] std::set<std::string> ids_in(const std::vector<story_event>& events,
                                           std::string_view field);

}

#endif
