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
#include "ores.http/routes/workflow/request_story_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/workflow/request_story.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <chrono>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace inbox_messaging = ores::inbox::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The sentence the story states when its address names no request.
constexpr std::string_view story_id_refused = "A request is named by its identifier.";

/// How long a story stays fresh: a decision told about it arrives within the minute.
constexpr std::chrono::seconds story_stale_after{15};

}

http_response request_story_routes::story_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(story_stale_after));
}

void request_story_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"workflow"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/requests/{id}/story")
            .summary("One request's whole story, newest first")
            .description("A composed view owned by ores.workflow: every row any component wrote "
                         "for the request, in one stream. The request's own versions, the answers "
                         "given on it, the roles it asked for and what became of them, and the "
                         "notices this reader was given, each a read of its own. The parts are "
                         "not equally visible: the request and its roles are the asker's, and "
                         "the decisions answer only to a reader who may read decisions, so a "
                         "member's story has a hole exactly where the answer was given. An "
                         "address whose request this caller may not read answers with no events, "
                         "which is what opening it answers. The answer carries an ETag and stays "
                         "fresh for 15 seconds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::get_approval_history_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid(std::string(story_id_refused));

                    const auto events = request_story_events(req, session, raw);
                    std::vector<std::string> written;
                    written.reserve(events.size());
                    for (const auto& event : events)
                        written.push_back(story_event_json(event));
                    return story_answer(req,
                                        "{\"requestId\":" + json::write(raw) +
                                            ",\"events\":" + rr::array_of(written) + "}");
                });
            }));
}
}
