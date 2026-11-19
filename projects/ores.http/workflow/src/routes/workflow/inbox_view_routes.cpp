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
#include "ores.http/routes/workflow/inbox_view_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/workflow/page_query.hpp"
#include "ores.http/routes/workflow/request_joins.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <map>
#include <set>
#include <string>
#include <string_view>
#include <utility>
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

/// The most rows one page of the inbox may ask for. A page costs a join per row.
constexpr std::uint32_t inbox_page_max = 100;

/// The page the inbox answers with when the caller states none.
constexpr std::uint32_t inbox_page_default = 50;

/// The sentence a page states when it is not one.
constexpr std::string_view inbox_page_refused = "A page names an offset and a limit of 1 to 100.";

/// How long the inbox stays fresh: short, because a decision empties a row.
constexpr std::chrono::seconds inbox_stale_after{15};

/**
 * One page of requests, each drawn with its joins.
 *
 * The page draws no parts: where each part of a request stands is read for the
 * one request a person opened, not for a page, so a list answers an empty list
 * of them rather than paying a read per row for what it does not draw.
 */
std::string page_json(const std::vector<ores::inbox::domain::approval_request>& rows,
                      const http_request& req,
                      nats_client& session) {
    std::vector<boost::uuids::uuid> ids;
    ids.reserve(rows.size());
    for (const auto& row : rows)
        ids.push_back(row.id);

    const auto requesters = joins::requesters_of(req, session, ids);
    const auto subjects = joins::subjects_of_page(req, session, ids);
    const auto decisions = joins::decisions_of(req, session, ids);
    std::set<std::string> accounts;
    for (const auto& row : rows) {
        const auto found = requesters.find(rr::as_text(row.id));
        accounts.insert(found == requesters.end() ? rr::as_text(row.requested_by) : found->second);
    }
    for (const auto& [id, decision] : decisions)
        accounts.insert(rr::as_text(decision.decided_by));
    const auto usernames = joins::usernames_of(req, session, accounts);

    std::vector<std::string> written;
    written.reserve(rows.size());
    for (const auto& row : rows) {
        const auto found = subjects.find(rr::as_text(row.id));
        std::vector<std::string> written_subjects;
        if (found != subjects.end()) {
            written_subjects.reserve(found->second.size());
            for (const auto& link : found->second)
                written_subjects.push_back(joins::subject_json(link));
        }
        written.push_back(joins::request_json(row,
                                              joins::roles_of(req, session, rr::as_text(row.id)),
                                              requesters,
                                              usernames,
                                              decisions,
                                              "[]",
                                              rr::array_of(written_subjects)));
    }
    return rr::array_of(written);
}

}

http_response inbox_view_routes::inbox_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(inbox_stale_after));
}

void inbox_view_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"workflow"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/requests")
            .summary("The requests this person may decide, and those answered and still in view")
            .description("A composed view owned by ores.workflow: the queue is the inbox "
                         "service's own read, which picks what this person may decide from the "
                         "permission each request's kind names and leaves out what they raised "
                         "themselves. Each row carries the roles it asks for, the latest thing "
                         "said about it, and the trades it is about, each a read of its own "
                         "beside the page. The answered tail travels beside the open queue, "
                         "because a notice that links back to a decision opens a request the "
                         "queue no longer holds. The answer carries an ETag and stays fresh for "
                         "15 seconds.")
            .query_param("offset", "integer", "int32", false, "How many requests to skip")
            .query_param("limit", "integer", "int32", false, "How many requests to return")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::list_approval_queue_request>([&] {
                    const auto asked =
                        page_of(req, inbox_page_default, inbox_page_max, inbox_page_refused);

                    inbox_messaging::list_approval_queue_request msg;
                    msg.offset = static_cast<int>(asked.offset);
                    msg.limit = static_cast<int>(asked.limit);
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    return inbox_answer(
                        req,
                        "{\"items\":" + page_json(reply.requests, req, session) +
                            ",\"total\":" + std::to_string(reply.total) +
                            ",\"answered\":" + page_json(reply.answered, req, session) + "}");
                });
            }));
}

}
