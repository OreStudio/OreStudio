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
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/workflow/request_story.hpp"
#include "ores.http/routes/workflow/timeline.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <array>
#include <chrono>
#include <exception>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The sentence a timeline states when its address names no subject and id.
constexpr std::string_view timeline_refused = "Name a timeline by its subject and its id.";

/// The subjects a timeline can be read for, as the reads name them.
constexpr std::array<std::string_view, 3> subjects{"person", "party", "request"};

/// How long a timeline stays fresh: its entries arrive over the hour, not the second.
constexpr std::chrono::seconds timeline_stale_after{30};

/// One gap, as the stream reads it.
std::string gap_json(const timeline_gap& gap) {
    return "{\"entity\":" + json::write(gap.entity) + ",\"reason\":" + json::write(gap.reason) +
           "}";
}

/// The answer one timeline states, each failure stated as the browser reads it.
http_response timeline_of(const http_request& req, nats_client& session) {
    try {
        rr::refuse_unknown_params(req, {"subject", "id"});
        const auto subject = req.get_query_param("subject");
        const auto id = req.get_query_param("id");
        if (subject.empty() || subject.size() > 32 || id.empty() || id.size() > 200)
            rr::invalid(std::string(timeline_refused));

        if (subject == "person")
            return timeline_routes::view_answer(req,
                                                timeline_json(person_timeline(req, session, id)));
        if (subject == "party")
            return timeline_routes::view_answer(req,
                                                timeline_json(party_timeline(req, session, id)));
        if (subject == "request") {
            if (!rr::is_uuid(id))
                rr::invalid(std::string(timeline_refused));
            timeline_answer answer;
            answer.subject = "request";
            answer.id = id;
            answer.events = request_story_events(req, session, id);
            return timeline_routes::view_answer(req, timeline_json(answer));
        }
        rr::invalid("A timeline is read for one of person, party, request, not for " + subject +
                    ".");
    } catch (const rr::refusal& refusal) {
        return rr::answer(refusal);
    } catch (const std::exception& e) {
        return ores::http::forwarding::unanswered("the timeline", e);
    }
}

}

std::string timeline_json(const timeline_answer& answer) {
    std::vector<std::string> events;
    events.reserve(answer.events.size());
    for (const auto& event : answer.events)
        events.push_back(story_event_json(event));
    std::vector<std::string> gaps;
    gaps.reserve(answer.gaps.size());
    for (const auto& gap : answer.gaps)
        gaps.push_back(gap_json(gap));
    return "{\"subject\":" + json::write(answer.subject) + ",\"id\":" + json::write(answer.id) +
           ",\"events\":" + rr::array_of(events) + ",\"gaps\":" + rr::array_of(gaps) + "}";
}

http_response timeline_routes::view_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(timeline_stale_after));
}

void timeline_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto built =
        router->get("/api/timeline")
            .summary("One subject's story, newest first")
            .description("A composed view owned by ores.workflow: every row any component wrote "
                         "for one subject, in one stream. The browser names a subject, never an "
                         "entity type: a request's story is the stream the inbox already answers, "
                         "while a person's is the account, its contact record, the roles it was "
                         "given, the parties it works in and its sign-ins, and a party's is its "
                         "own versions, those of the parts it holds and the links it keeps. Each "
                         "source is gated by the permission the owning component puts on it, and "
                         "a source that refuses becomes a gap on the answer rather than a hole a "
                         "screen cannot explain. A subject nothing holds answers with an empty "
                         "stream. The answer carries an ETag and stays fresh for 30 seconds.")
            .tags({"workflow"})
            .auth_required()
            .query_param("subject", "string", "", true, "The subject: person, party or request")
            .query_param("id", "string", "", true, "The subject's key")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return timeline_of(req, session);
            })
            .build();
    router->add_route(built);
    registry->register_route(built);
}

}
