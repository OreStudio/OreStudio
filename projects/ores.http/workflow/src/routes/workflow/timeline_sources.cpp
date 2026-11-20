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
#include "ores.http/routes/workflow/timeline_sources.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/logger.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <exception>
#include <string>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;

using ores::http::domain::http_request;
using ores::nats::service::nats_client;

/// The entities a stream names by their own names, so a gap can say which.
constexpr std::string_view named_party_entity = "ores.refdata.party";
constexpr std::string_view named_counterparty_entity = "ores.refdata.counterparty";

/// States a gap once, with the entity the read was for and why it is a gap.
void note_gap(std::vector<timeline_gap>& gaps, std::string_view entity, std::string_view reason) {
    gaps.push_back({std::string(entity), std::string(reason)});
}

/// The trail a source leaves when it cannot name the row an entry points at.
auto& timeline_lg() {
    static auto instance = ores::logging::make_logger("ores.http.workflow.timeline_sources");
    return instance;
}

/// The uuids a set of ids names, skipping the ones that are not uuids at all.
std::vector<boost::uuids::uuid> uuids_of(const std::set<std::string>& ids) {
    std::vector<boost::uuids::uuid> out;
    out.reserve(ids.size());
    for (const auto& id : ids) {
        try {
            out.push_back(boost::uuids::string_generator()(id));
        } catch (const std::exception& e) {
            ORES_LOG_SEV(timeline_lg(), ores::logging::warn)
                << "A row this stream points at is not named by a uuid, so it keeps its id: "
                << e.what();
        }
    }
    return out;
}

}

std::vector<ores::http::net::history::version> versions_or_gap(std::vector<timeline_gap>& gaps,
                                                               std::string_view entity,
                                                               std::string_view reason,
                                                               const http_request& req,
                                                               nats_client& session,
                                                               std::string_view entity_type,
                                                               std::string_view entity_id) {
    return or_gap(gaps, entity, reason, [&]() {
        const auto answer = ores::http::net::history::read(req, session, entity_type, entity_id);
        if (!answer.answered) {
            ORES_LOG_SEV(timeline_lg(), ores::logging::info)
                << "The history of " << entity_type << " does not answer this "
                << "caller, so the stream carries a gap: " << answer.message;
            gaps.push_back({std::string(entity), std::string(reason)});
            return std::vector<ores::http::net::history::version>{};
        }
        return answer.versions;
    });
}

std::map<std::string, std::string> party_names(const http_request& req,
                                               nats_client& session,
                                               const std::set<std::string>& ids,
                                               std::vector<timeline_gap>& gaps) {
    std::map<std::string, std::string> names;
    if (ids.empty())
        return names;
    ores::refdata::messaging::list_parties_request msg;
    msg.limit = static_cast<std::uint32_t>(ids.size());
    msg.filter = ores::refdata::messaging::parties_filter{uuids_of(ids), std::nullopt};
    const auto unanswered = [&] {
        note_gap(gaps,
                 named_party_entity,
                 "The parties this stream points at could not be named, so each is named by its "
                 "id.");
    };
    try {
        const auto reply = forwarding::forward(req, session, msg);
        if (reply.result.outcome != ores::utility::domain::outcome::ok) {
            ORES_LOG_SEV(timeline_lg(), ores::logging::info)
                << "The parties a timeline points at could not be named: " << reply.result.message;
            unanswered();
            return names;
        }
        for (const auto& party : reply.parties)
            names[rr::as_text(party.id)] =
                party.full_name.empty() ? party.short_code : party.full_name;
    } catch (const rr::refusal& refusal) {
        ORES_LOG_SEV(timeline_lg(), ores::logging::info)
            << "The parties a timeline points at were refused: " << refusal.message;
        unanswered();
    } catch (const std::exception& e) {
        ORES_LOG_SEV(timeline_lg(), ores::logging::warn)
            << "The parties a timeline points at could not be read, so their ids stand: "
            << e.what();
        unanswered();
    }
    return names;
}

std::map<std::string, std::string> counterparty_names(const http_request& req,
                                                      nats_client& session,
                                                      const std::set<std::string>& ids,
                                                      std::vector<timeline_gap>& gaps) {
    std::map<std::string, std::string> names;
    if (ids.empty())
        return names;
    ores::refdata::messaging::list_counterparties_request msg;
    msg.limit = static_cast<std::uint32_t>(ids.size());
    msg.filter = ores::refdata::messaging::counterparties_filter{uuids_of(ids)};
    const auto unanswered = [&] {
        note_gap(gaps,
                 named_counterparty_entity,
                 "The counterparties this stream points at could not be named, so each is named "
                 "by its id.");
    };
    try {
        const auto reply = forwarding::forward(req, session, msg);
        if (reply.result.outcome != ores::utility::domain::outcome::ok) {
            ORES_LOG_SEV(timeline_lg(), ores::logging::info)
                << "The counterparties a timeline points at could not be named: "
                << reply.result.message;
            unanswered();
            return names;
        }
        for (const auto& counterparty : reply.counterparties)
            names[rr::as_text(counterparty.id)] =
                counterparty.full_name.empty() ? counterparty.short_code : counterparty.full_name;
    } catch (const rr::refusal& refusal) {
        ORES_LOG_SEV(timeline_lg(), ores::logging::info)
            << "The counterparties a timeline points at were refused: " << refusal.message;
        unanswered();
    } catch (const std::exception& e) {
        ORES_LOG_SEV(timeline_lg(), ores::logging::warn)
            << "The counterparties a timeline points at could not be read, so their ids stand: "
            << e.what();
        unanswered();
    }
    return names;
}

std::vector<story_event> with_names(const std::vector<story_event>& events,
                                    std::string_view field,
                                    const std::map<std::string, std::string>& names) {
    if (names.empty())
        return events;
    std::vector<story_event> named = events;
    for (auto& event : named) {
        for (auto& [name, value] : event.fields) {
            if (name != field)
                continue;
            const auto found = names.find(value);
            if (found != names.end())
                value = found->second;
        }
    }
    return named;
}

std::set<std::string> ids_in(const std::vector<story_event>& events, std::string_view field) {
    std::set<std::string> ids;
    for (const auto& event : events) {
        for (const auto& [name, value] : event.fields)
            if (name == field && !value.empty())
                ids.insert(value);
    }
    return ids;
}

}
