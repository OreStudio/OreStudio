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
#include "ores.http/routes/workflow/request_joins.hpp"
#include "ores.inbox.api/messaging/approval_decision_protocol.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
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

/// The trail the story leaves when a source is deliberately absent.
auto& story_lg() {
    static auto instance = ores::logging::make_logger("ores.http.workflow.request_story_routes");
    return instance;
}

/// The sentence the story states when its address names no request.
constexpr std::string_view story_id_refused = "A request is named by its identifier.";

/// How long a story stays fresh: a decision told about it arrives within the minute.
constexpr std::chrono::seconds story_stale_after{15};

/// The tables the story draws from, named as the models name them.
constexpr std::string_view request_entity = "ores.inbox.approval_request";
constexpr std::string_view decision_entity = "ores.inbox.approval_decision";
constexpr std::string_view notice_entity = "ores.inbox.notification";
constexpr std::string_view request_role_entity = "ores.iam.role_grant_request_role";

/// One thing that happened to the request, before it is ordered or written.
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
 * How late in a second an event of each kind lands.
 *
 * The timestamps on the wire carry whole seconds, so ties are the common case
 * rather than the exception, and the order among them reads as the order the
 * acts happened in: a request is raised, its roles are recorded, notices go out,
 * an answer is given, the role is applied.
 */
int kind_rank(std::string_view kind) {
    if (kind == "raised")
        return 10;
    if (kind == "changed")
        return 20;
    if (kind == "asked")
        return 30;
    if (kind == "told")
        return 40;
    if (kind == "decided")
        return 50;
    if (kind == "granted")
        return 60;
    return 0;
}

/// Newest first, and within one second, the later act first.
void order_story(std::vector<story_event>& events) {
    std::ranges::stable_sort(events, [](const story_event& left, const story_event& right) {
        if (left.at != right.at)
            return left.at > right.at;
        const auto rank = kind_rank(right.kind) - kind_rank(left.kind);
        if (rank != 0)
            return rank < 0;
        if (left.entity_id != right.entity_id)
            return left.entity_id < right.entity_id;
        return right.version < left.version;
    });
}

/// One field of an event, as the screen reads it.
std::string field_value_json(const std::pair<std::string, std::string>& field) {
    return "{\"name\":" + json::write(field.first) + ",\"value\":" + json::write(field.second) +
           "}";
}

/// One event, as the stream reads it.
std::string event_json(const story_event& event) {
    std::vector<std::string> written;
    written.reserve(event.fields.size());
    for (const auto& field : event.fields)
        written.push_back(field_value_json(field));
    return "{\"entityType\":" + json::write(event.entity_type) +
           ",\"entityId\":" + json::write(event.entity_id) +
           ",\"kind\":" + json::write(event.kind) + ",\"at\":" + json::write(event.at) +
           ",\"actor\":" + json::write(event.actor) +
           ",\"version\":" + std::to_string(event.version) +
           ",\"reasonCode\":" + json::write(event.reason_code) +
           ",\"commentary\":" + json::write(event.commentary) +
           ",\"fields\":" + rr::array_of(written) + "}";
}

/// The request's own versions, or none when the caller may not read them.
std::vector<story_event>
request_versions(const http_request& req, nats_client& session, const std::string& request_id) {
    std::vector<story_event> events;
    inbox_messaging::get_approval_history_request msg;
    msg.request_id = request_id;
    const auto reply = forwarding::forward(req, session, msg);
    if (reply.result.outcome == ores::utility::domain::outcome::missing ||
        reply.result.outcome == ores::utility::domain::outcome::denied)
        return events;
    rr::require_ok(reply.result);

    events.reserve(reply.versions.size());
    for (const auto& version : reply.versions) {
        story_event event;
        event.entity_type = std::string(request_entity);
        event.entity_id = request_id;
        event.kind = version.version == 1 ? "raised" : "changed";
        event.at = version.recorded_at;
        event.actor = version.modified_by;
        event.version = version.version;
        event.reason_code = version.change_reason_code;
        event.commentary = version.change_commentary;
        for (const auto& field : version.fields)
            event.fields.emplace_back(field.name, field.value);
        events.push_back(std::move(event));
    }
    return events;
}

/**
 * What became of the roles one request asked for.
 *
 * Asking and applying are two events, because they are two acts by two people and
 * the second is the one the person is waiting for.
 */
std::vector<story_event>
role_events(const http_request& req, nats_client& session, const std::string& request_id) {
    std::vector<story_event> events;
    for (const auto& held : joins::roles_of(req, session, request_id)) {
        story_event asked;
        asked.entity_type = std::string(request_role_entity);
        asked.entity_id = rr::as_text(held.role.id);
        asked.kind = "asked";
        asked.at = held.asked_at;
        asked.version = 1;
        asked.fields.emplace_back("Role", held.role.name);
        asked.fields.emplace_back("Description", held.role.description);
        events.push_back(std::move(asked));

        if (!held.applied_at)
            continue;
        story_event granted;
        granted.entity_type = std::string(request_role_entity);
        granted.entity_id = rr::as_text(held.role.id);
        granted.kind = "granted";
        granted.at = *held.applied_at;
        granted.actor = held.applied_by;
        granted.version = 2;
        granted.fields.emplace_back("Role", held.role.name);
        events.push_back(std::move(granted));
    }
    return events;
}

/**
 * The answers given on one request, when the reader may read them.
 *
 * A request that needs several parts has an answer from each, so every answer is
 * its own event and names the part it answered for. The decider is named by
 * account, so the ids are resolved rather than shown.
 */
std::vector<story_event>
decision_events(const http_request& req, nats_client& session, const std::string& request_id) {
    inbox_messaging::list_approval_decisions_request msg;
    msg.limit = joins::join_page;
    msg.filter.emplace();
    msg.filter->request_id = boost::uuids::string_generator()(request_id);
    const auto reply = forwarding::forward(req, session, msg);
    if (reply.result.outcome == ores::utility::domain::outcome::missing ||
        reply.result.outcome == ores::utility::domain::outcome::denied)
        return {};
    rr::require_ok(reply.result);
    const auto& decisions = reply.decisions;

    std::set<std::string> accounts;
    for (const auto& decision : decisions)
        accounts.insert(rr::as_text(decision.decided_by));
    const auto usernames = joins::usernames_of(req, session, accounts);

    std::vector<story_event> events;
    events.reserve(decisions.size());
    for (const auto& decision : decisions) {
        story_event event;
        event.entity_type = std::string(decision_entity);
        event.entity_id = request_id;
        event.kind = "decided";
        event.at = decision.decided_at;
        const auto named = usernames.find(rr::as_text(decision.decided_by));
        event.actor = named == usernames.end() ? rr::as_text(decision.decided_by) : named->second;
        event.version = 1;
        event.fields.emplace_back("Decision", decision.decision_code);
        if (decision.part_code && !decision.part_code->empty())
            event.fields.emplace_back("Part", *decision.part_code);
        event.fields.emplace_back("Comment", decision.comment);
        events.push_back(std::move(event));
    }
    return events;
}

/// One notice, added when it names the request.
void add_notice(const ores::inbox::messaging::inbox_notification& notice,
                const std::string& request_id,
                std::vector<story_event>& events) {
    if (notice.link_id != request_id)
        return;
    story_event event;
    event.entity_type = std::string(notice_entity);
    event.entity_id = notice.id;
    event.kind = "told";
    event.at = notice.raised_at;
    event.actor = notice.raised_by;
    event.version = 1;
    event.fields.emplace_back("Message Key", notice.message_key);
    for (const auto& argument : notice.arguments)
        event.fields.emplace_back(argument.name, argument.value);
    // The field states the instant in the wire's own text, which is what the JSON
    // codec writes for every instant: one formatter, reached the same way.
    if (notice.read_at)
        event.fields.emplace_back(
            "Read At",
            ores::utility::codec::text_form<std::chrono::system_clock::time_point>::to_text(
                *notice.read_at));
    events.push_back(std::move(event));
}

/**
 * The notices this reader was given about one request.
 *
 * A notice is a person's own copy, so this reads the caller's own list and keeps
 * the ones that name the request: a story is a view of what happened to you and of
 * what you did, not of what was done to somebody else.
 *
 * The read answers a page of the caller's own notices and offers no filter by the
 * thing a notice links to, so a story reads every page it needs. The bound is that
 * page raised by the most pages a story will read: a person with more notices than
 * that is told the story is short rather than shown a stream that looks complete.
 */
std::vector<story_event>
notice_events(const http_request& req, nats_client& session, const std::string& request_id) {
    std::vector<story_event> events;
    constexpr std::uint32_t notice_pages = 50;
    const auto page = static_cast<std::size_t>(joins::join_page);
    for (std::uint32_t read = 0; read < notice_pages; ++read) {
        inbox_messaging::list_my_notifications_request msg;
        msg.unread_only = false;
        msg.offset = static_cast<int>(static_cast<std::size_t>(read) * page);
        msg.limit = static_cast<int>(page);
        const auto reply = forwarding::forward(req, session, msg);
        if (reply.result.outcome != ores::utility::domain::outcome::ok)
            return events;
        for (const auto& notice : reply.notifications)
            add_notice(notice, request_id, events);
        if (reply.notifications.size() < page)
            return events;
    }
    BOOST_LOG_SEV(story_lg(), ores::logging::warn)
        << "The notices about request " << request_id
        << " were read to the page limit; older ones are not in the story.";
    return events;
}

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

                    std::vector<story_event> events = request_versions(req, session, raw);
                    for (auto& event : role_events(req, session, raw))
                        events.push_back(std::move(event));
                    for (auto& event : decision_events(req, session, raw))
                        events.push_back(std::move(event));
                    for (auto& event : notice_events(req, session, raw))
                        events.push_back(std::move(event));
                    order_story(events);

                    std::vector<std::string> written;
                    written.reserve(events.size());
                    for (const auto& event : events)
                        written.push_back(event_json(event));
                    return story_answer(req,
                                        "{\"requestId\":" + json::write(raw) +
                                            ",\"events\":" + rr::array_of(written) + "}");
                });
            }));
}
}
