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
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/role_grant_request_protocol.hpp"
#include "ores.iam.api/messaging/role_request_operations_protocol.hpp"
#include "ores.inbox.api/messaging/approval_decision_protocol.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.inbox.api/messaging/approval_request_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
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

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The most rows one page may ask for. A page costs the join one read per row.
constexpr std::uint32_t page_max = 100;

/// The page a read answers with when the caller states none.
constexpr std::uint32_t page_default = 50;

/// The most rows one join asks for; the services refuse a longer filter.
constexpr std::uint32_t join_page = 1000;

/// The sentence a page states when it is not one.
constexpr std::string_view page_refused = "A page names an offset and a limit of 1 to 100.";

/// The sentence an ask states when its body is not the shape it takes.
constexpr std::string_view ask_refused = "Asking for roles names at least one role, and why.";

/// The sentence a withdrawal states when its body is not the shape it takes.
constexpr std::string_view withdraw_refused =
    "Withdrawing a request names the version read and why.";

/// The sentence a route states when its address names no identifier.
constexpr std::string_view id_refused = "A request is named by its identifier.";

/// A page of a list: how many rows to skip and how many to answer with.
struct page final {
    std::uint32_t offset = 0;
    std::uint32_t limit = page_default;
};

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

page page_of(const http_request& req) {
    rr::refuse_unknown_params(req, {"offset", "limit"});
    page out;
    const auto whole = [](const std::string& raw) -> std::optional<std::uint32_t> {
        if (raw.empty())
            return std::nullopt;
        std::uint32_t value = 0;
        for (const char c : raw) {
            if (c < '0' || c > '9' || value > 400'000'000)
                rr::invalid(std::string(page_refused));
            value = value * 10 + static_cast<std::uint32_t>(c - '0');
        }
        return value;
    };
    if (const auto stated = whole(req.get_query_param("offset")))
        out.offset = *stated;
    if (const auto stated = whole(req.get_query_param("limit"))) {
        if (*stated < 1 || *stated > page_max)
            rr::invalid(std::string(page_refused));
        out.limit = *stated;
    }
    return out;
}

/// The identifier a request is named by.
std::string request_id_of(const http_request& req) {
    const auto raw = req.get_path_param("id");
    if (!rr::is_uuid(raw))
        rr::invalid(std::string(id_refused));
    return raw;
}

/**
 * @brief The roles one request asks for, or none when the caller may not read them.
 *
 * The store serves no batched read of a request's roles, so a page costs one
 * read per row; a refusal costs that row's roles and not the page.
 */
std::vector<iam_messaging::requested_role>
roles_of(const http_request& req, nats_client& session, const std::string& request_id) {
    try {
        iam_messaging::get_request_roles_request msg;
        msg.request_id = request_id;
        const auto reply = forwarding::forward(req, session, msg);
        if (reply.result.outcome != ores::utility::domain::outcome::ok)
            return {};
        return reply.roles;
    } catch (const std::exception&) {
        return {};
    }
}

/// The account that raised each request, by the request's id.
std::map<std::string, std::string>
requesters_of(const http_request& req,
              nats_client& session,
              const std::vector<boost::uuids::uuid>& request_ids) {
    std::map<std::string, std::string> requesters;
    if (request_ids.empty())
        return requesters;
    try {
        iam_messaging::list_role_grant_requests_request msg;
        msg.limit = join_page;
        msg.filter.emplace();
        msg.filter->request_id_one_of = request_ids;
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        for (const auto& grant : reply.role_grant_requests)
            requesters.emplace(rr::as_text(grant.request_id), rr::as_text(grant.account_id));
    } catch (const std::exception&) {
        return requesters;
    }
    return requesters;
}

/// The username of each account named, for the accounts this caller may read.
std::map<std::string, std::string> usernames_of(const http_request& req,
                                                nats_client& session,
                                                const std::set<std::string>& account_ids) {
    std::map<std::string, std::string> usernames;
    if (account_ids.empty())
        return usernames;

    std::vector<boost::uuids::uuid> wanted;
    wanted.reserve(account_ids.size());
    for (const auto& id : account_ids) {
        if (rr::is_uuid(id))
            wanted.push_back(boost::uuids::string_generator()(id));
    }
    if (wanted.empty())
        return usernames;

    try {
        iam_messaging::list_accounts_request msg;
        msg.limit = join_page;
        msg.filter.emplace();
        msg.filter->id_one_of = std::move(wanted);
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        for (const auto& account : reply.accounts)
            usernames.emplace(rr::as_text(account.id), account.username);
    } catch (const std::exception&) {
        return usernames;
    }
    return usernames;
}

/// The latest decision on each request, by the request's id.
std::map<std::string, inbox_domain::approval_decision>
decisions_of(const http_request& req,
             nats_client& session,
             const std::vector<boost::uuids::uuid>& request_ids) {
    std::map<std::string, inbox_domain::approval_decision> decisions;
    if (request_ids.empty())
        return decisions;
    try {
        inbox_messaging::list_approval_decisions_request msg;
        msg.limit = join_page;
        msg.filter.emplace();
        msg.filter->request_id_one_of = request_ids;
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        for (const auto& decision : reply.decisions) {
            const auto key = rr::as_text(decision.request_id);
            const auto found = decisions.find(key);
            if (found != decisions.end() && found->second.decided_at >= decision.decided_at)
                continue;
            decisions.insert_or_assign(key, decision);
        }
    } catch (const std::exception&) {
        return decisions;
    }
    return decisions;
}

/// One role a request asks for, as the screen draws it.
std::string role_json(const iam_domain::role& role) {
    return "{\"roleId\":" + json::write(rr::as_text(role.id)) +
           ",\"name\":" + json::write(role.name) +
           ",\"description\":" + json::write(role.description) + "}";
}

/// What was decided about a request, with the decider named where they can be.
std::string decision_json(const inbox_domain::approval_decision& decision,
                          const std::map<std::string, std::string>& usernames) {
    const auto decider = rr::as_text(decision.decided_by);
    const auto named = usernames.find(decider);
    return "{\"decisionCode\":" + json::write(decision.decision_code) +
           ",\"decidedBy\":" + json::write(named == usernames.end() ? decider : named->second) +
           ",\"decidedAt\":" + json::write(decision.decided_at) +
           ",\"comment\":" + json::write(decision.comment) + "}";
}

/// One request as the screen reads it.
std::string request_json(const inbox_domain::approval_request& request,
                         const std::vector<iam_messaging::requested_role>& roles,
                         const std::map<std::string, std::string>& requesters,
                         const std::map<std::string, std::string>& usernames,
                         const std::map<std::string, inbox_domain::approval_decision>& decisions) {
    const auto id = rr::as_text(request.id);
    const auto raised_by = rr::as_text(request.requested_by);
    const auto account = requesters.find(id);
    const auto requester = account == requesters.end() ? raised_by : account->second;
    const auto named = usernames.find(requester);

    std::vector<std::string> written;
    written.reserve(roles.size());
    for (const auto& held : roles)
        written.push_back(role_json(held.role));

    const auto decided = decisions.find(id);
    const auto decision = decided == decisions.end() ? std::string("null") :
                                                       decision_json(decided->second, usernames);

    return "{\"id\":" + json::write(id) + ",\"version\":" + json::write(request.version) +
           ",\"kindCode\":" + json::write(request.kind_code) +
           ",\"stateCode\":" + json::write(request.state_code) +
           ",\"autoApprovalCode\":" + json::write(request.auto_approval_code) +
           ",\"requestedBy\":" + json::write(named == usernames.end() ? requester : named->second) +
           ",\"requestedAt\":" + json::write(request.requested_at) +
           ",\"reason\":" + json::write(request.reason) +
           ",\"expiresAt\":" + json::write(request.expires_at) +
           ",\"roles\":" + rr::array_of(written) + ",\"decision\":" + decision +
           ",\"parts\":[],\"subjects\":[]}";
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
                    const auto asked = page_of(req);
                    inbox_messaging::list_my_approval_requests_request msg;
                    msg.offset = static_cast<int>(asked.offset);
                    msg.limit = static_cast<int>(asked.limit);
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    std::vector<boost::uuids::uuid> ids;
                    ids.reserve(reply.requests.size());
                    for (const auto& request : reply.requests)
                        ids.push_back(request.id);

                    const auto requesters = requesters_of(req, session, ids);
                    std::set<std::string> accounts;
                    for (const auto& request : reply.requests) {
                        const auto found = requesters.find(rr::as_text(request.id));
                        accounts.insert(found == requesters.end() ?
                                            rr::as_text(request.requested_by) :
                                            found->second);
                    }
                    const auto decisions = decisions_of(req, session, ids);
                    for (const auto& [request_id, decision] : decisions)
                        accounts.insert(rr::as_text(decision.decided_by));
                    const auto usernames = usernames_of(req, session, accounts);

                    std::vector<std::string> written;
                    written.reserve(reply.requests.size());
                    for (const auto& request : reply.requests)
                        written.push_back(
                            request_json(request,
                                         roles_of(req, session, rr::as_text(request.id)),
                                         requesters,
                                         usernames,
                                         decisions));
                    return http_response::json("{\"items\":" + rr::array_of(written) +
                                               ",\"total\":" + std::to_string(reply.total) + "}");
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
