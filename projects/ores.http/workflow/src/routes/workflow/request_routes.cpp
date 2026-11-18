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
#include "ores.http/routes/workflow/request_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/workflow/page_query.hpp"
#include "ores.http/routes/workflow/request_joins.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/role_grant_request_protocol.hpp"
#include "ores.iam.api/messaging/role_request_operations_protocol.hpp"
#include "ores.inbox.api/messaging/approval_decision_protocol.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.inbox.api/messaging/approval_request_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/messaging/trade_authorisation_request_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <charconv>
#include <chrono>
#include <cstdint>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace inbox_domain = ores::inbox::domain;
namespace inbox_messaging = ores::inbox::messaging;
namespace iam_domain = ores::iam::domain;
namespace iam_messaging = ores::iam::messaging;
namespace trading_messaging = ores::trading::messaging;
namespace joins = ores::http::routes::workflow::joins;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The trail a read leaves when a join of a page is deliberately absent.
auto& request_lg() {
    static auto instance = ores::logging::make_logger("ores.http.workflow.request_routes");
    return instance;
}

/// The most rows one page may ask for. A page costs the join one read per row.
constexpr std::uint32_t page_max = 100;

/// The page a read answers with when the caller states none.
constexpr std::uint32_t page_default = 50;

/// The sentence a page states when it is not one.
constexpr std::string_view page_refused = "A page names an offset and a limit of 1 to 100.";

/// The sentence an ask states when its body is not the shape it takes.
constexpr std::string_view ask_refused = "Asking for roles names at least one role, and why.";

/// The sentence a withdrawal states when its body is not the shape it takes.
constexpr std::string_view withdraw_refused =
    "Withdrawing a request names the version read and why.";

/// The sentence a route states when its address names no identifier.
constexpr std::string_view id_refused = "A request is named by its identifier.";

/// An ask as the screen states it: the roles wanted and why.
struct ask_body final {
    std::vector<std::string> role_ids;
    std::string reason;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<ask_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"roleIds", &ask_body::role_ids},
                                            field{"reason", &ask_body::reason}};
}

/// A withdrawal as the screen states it: the version read and why.
struct intent_body final {
    int version{};
    std::string comment;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<intent_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"version", &intent_body::version},
                                            field{"comment", &intent_body::comment}};
}

/// A decision as the screen states it: what it decides, and why.
struct decide_body final {
    int version{};
    std::string comment;
    std::string decision_code;
    std::optional<std::string> part_code;
    std::optional<std::string> hold_reason_code;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<decide_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"version", &decide_body::version},
        field{"comment", &decide_body::comment},
        field{"decisionCode", &decide_body::decision_code},
        field{"partCode", &decide_body::part_code},
        field{"holdReasonCode", &decide_body::hold_reason_code}};
}

/// The sentence a decision states when its body is not the shape it takes.
constexpr std::string_view decision_refused =
    "A decision is approve, refuse, hold or resume, against the version read.";

/// Whether a code is lower snake case over the characters @p extra allows.
bool is_code(const std::string& code, std::string_view extra) {
    if (code.empty() || code.size() > 100)
        return false;
    if (code.front() < 'a' || code.front() > 'z')
        return false;
    for (const char c : code) {
        const auto lower = c >= 'a' && c <= 'z';
        const auto digit = c >= '0' && c <= '9';
        if (!lower && !digit && extra.find(c) == std::string_view::npos)
            return false;
    }
    return true;
}

/// The decision the screen states, refused when a member is not the shape it takes.
decide_body decision_of(const http_request& req) {
    const auto body = rr::body_of<decide_body>(
        req,
        {"version", "comment", "decisionCode", "partCode", "holdReasonCode"},
        std::string(decision_refused));
    const auto part = body.part_code.value_or(std::string());
    const auto reason = body.hold_reason_code.value_or(std::string());
    const auto known = body.decision_code == "approve" || body.decision_code == "refuse" ||
                       body.decision_code == "hold" || body.decision_code == "resume";
    if (body.version < 0 || !known || rr::characters_of(body.comment) > 500 ||
        (!part.empty() && !is_code(part, "_")) || (!reason.empty() && !is_code(reason, "_.")))
        rr::invalid(std::string(decision_refused));
    return body;
}

/// The identifier a request is named by.
std::string request_id_of(const http_request& req) {
    const auto raw = req.get_path_param("id");
    if (!rr::is_uuid(raw))
        rr::invalid(std::string(id_refused));
    return raw;
}


}

void request_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"workflow"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/me/requests")
            .summary("The requests the caller raised, newest first")
            .description("Each row names the roles it asks for and the latest thing said about "
                         "it. The names are a join: the account that raised a request, and the "
                         "people who decided, are read beside the page, and a read that fails "
                         "leaves that name out rather than failing the page.")
            .query_param("offset", "integer", "int32", false, "How many requests to skip")
            .query_param("limit", "integer", "int32", false, "How many requests to return")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::list_my_approval_requests_request>([&] {
                    const auto asked = page_of(req, page_default, page_max, page_refused);
                    inbox_messaging::list_my_approval_requests_request msg;
                    msg.offset = static_cast<int>(asked.offset);
                    msg.limit = static_cast<int>(asked.limit);
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    std::vector<boost::uuids::uuid> ids;
                    ids.reserve(reply.requests.size());
                    for (const auto& request : reply.requests)
                        ids.push_back(request.id);

                    const auto requesters = joins::requesters_of(req, session, ids);
                    std::set<std::string> accounts;
                    for (const auto& request : reply.requests) {
                        const auto found = requesters.find(rr::as_text(request.id));
                        accounts.insert(found == requesters.end() ?
                                            rr::as_text(request.requested_by) :
                                            found->second);
                    }
                    const auto decisions = joins::decisions_of(req, session, ids);
                    for (const auto& [request_id, decision] : decisions)
                        accounts.insert(rr::as_text(decision.decided_by));
                    const auto usernames = joins::usernames_of(req, session, accounts);

                    std::vector<std::string> written;
                    written.reserve(reply.requests.size());
                    for (const auto& request : reply.requests)
                        written.push_back(joins::request_json(
                            request,
                            joins::roles_of(req, session, rr::as_text(request.id)),
                            requesters,
                            usernames,
                            decisions,
                            "[]",
                            "[]"));
                    return http_response::json("{\"items\":" + rr::array_of(written) +
                                               ",\"total\":" + std::to_string(reply.total) + "}");
                });
            }));

    add(router->get("/api/requests/{id}")
            .summary("One request, with what it asks for, what was decided and what it is about")
            .description("A notice carries the request it is about, and is read after the request "
                         "stopped waiting, so this read does not depend on a queue. A request the "
                         "caller may not open answers as one that does not exist. requestedBy and "
                         "a decision's decidedBy hold a username where the join could read the "
                         "account and the account's identifier where it could not.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::get_approval_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto id = request_id_of(req);
                    inbox_messaging::get_approval_request msg;
                    msg.request_id = id;
                    const auto reply = forwarding::forward(req, session, msg);
                    if (reply.result.outcome == ores::utility::domain::outcome::missing)
                        rr::refuse(http_status::not_found, "not-found", "No such request.");
                    if (!reply.request) {
                        BOOST_LOG_SEV(request_lg(), ores::logging::warn)
                            << "The inbox answered request " << id << " without a row.";
                        rr::refuse(http_status::not_found, "not-found", "No such request.");
                    }
                    rr::require_ok(reply.result);

                    const auto request = *reply.request;
                    const std::vector<boost::uuids::uuid> ids{request.id};
                    const auto requesters = joins::requesters_of(req, session, ids);
                    const auto decisions = joins::decisions_of(req, session, ids);
                    const auto parts = joins::parts_of(req, session, id);

                    std::set<std::string> accounts{rr::as_text(request.requested_by)};
                    for (const auto& [request_id, decision] : decisions)
                        accounts.insert(rr::as_text(decision.decided_by));
                    for (const auto& part : parts)
                        if (!part.decided_by.empty())
                            accounts.insert(part.decided_by);
                    const auto usernames = joins::usernames_of(req, session, accounts);

                    std::vector<std::string> written_parts;
                    written_parts.reserve(parts.size());
                    for (const auto& part : parts)
                        written_parts.push_back(joins::part_json(part, usernames));

                    const auto links = joins::subjects_of(req, session, id);
                    std::vector<std::string> written_subjects;
                    written_subjects.reserve(links.size());
                    for (const auto& link : links)
                        written_subjects.push_back(joins::subject_json(link));

                    return http_response::json(
                        joins::request_json(request,
                                            joins::roles_of(req, session, id),
                                            requesters,
                                            usernames,
                                            decisions,
                                            rr::array_of(written_parts),
                                            rr::array_of(written_subjects)));
                });
            }));

    add(router->post("/api/me/requests")
            .summary("Ask for roles, and answer the request raised")
            .description("The service refuses a role that is unknown, already held, or already "
                         "asked for in a request that still waits.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<iam_messaging::ask_for_roles_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto body =
                        rr::body_of<ask_body>(req, {"roleIds", "reason"}, std::string(ask_refused));
                    if (body.role_ids.empty() || body.role_ids.size() > page_max ||
                        rr::characters_of(body.reason) > 500)
                        rr::invalid(std::string(ask_refused));
                    for (const auto& role : body.role_ids)
                        if (!rr::is_uuid(role))
                            rr::invalid(std::string(ask_refused));

                    iam_messaging::ask_for_roles_request msg;
                    msg.role_ids = body.role_ids;
                    msg.reason = body.reason;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    return http_response::json("{\"requestId\":" + json::write(reply.request_id) +
                                               "}");
                });
            }));

    add(router->post("/api/requests/{id}/decision")
            .summary("Approve, refuse, hold or resume one request")
            .description("The decider needs the permission the request's kind names and is not "
                         "the person who asked; the service decides both, so the route does not "
                         "guess at either.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::decide_approval_request_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto id = request_id_of(req);
                    const auto body = decision_of(req);

                    inbox_messaging::decide_approval_request_request msg;
                    msg.request_id = id;
                    msg.version = body.version;
                    msg.decision_code = body.decision_code;
                    msg.comment = body.comment;
                    msg.part_code = body.part_code.value_or(std::string());
                    msg.hold_reason_code = body.hold_reason_code.value_or(std::string());
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    return rr::no_content();
                });
            }));

    add(router->delete_("/api/me/requests/{id}")
            .summary("Take back a request, against the version the caller read")
            .description("Only an open request can be withdrawn, and only by the person who "
                         "asked. A request that moved on is refused rather than discarded.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::withdraw_approval_request_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto id = request_id_of(req);
                    const auto body = rr::body_of<intent_body>(
                        req, {"version", "comment"}, std::string(withdraw_refused));
                    if (body.version < 0 || rr::characters_of(body.comment) > 500)
                        rr::invalid(std::string(withdraw_refused));

                    inbox_messaging::withdraw_approval_request_request msg;
                    msg.request_id = id;
                    msg.version = body.version;
                    msg.comment = body.comment;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    return rr::no_content();
                });
            }));
}

}
