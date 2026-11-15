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
#include "ores.http/routes/iam/access_read_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/access_screen_shapes.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <charconv>
#include <chrono>
#include <exception>
#include <functional>
#include <limits>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::iam::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

/// A query number, or its default when the query leaves it out.
std::optional<int>
number_of(const http_request& req, const std::string& name, int fallback, int least, int most) {
    const auto text = req.get_query_param(name);
    if (text.empty())
        return fallback;
    int value = 0;
    const auto [end, failed] = std::from_chars(text.data(), text.data() + text.size(), value);
    if (failed != std::errc{} || end != text.data() + text.size() || value < least || value > most)
        return std::nullopt;
    return value;
}

/// A query flag the browser states as true or false, or false when it is left out.
std::optional<bool> flag_of(const http_request& req, const std::string& name) {
    const auto text = req.get_query_param(name);
    if (text.empty() || text == "false")
        return false;
    if (text == "true")
        return true;
    return std::nullopt;
}

/// A query text, or nothing when it is longer than the BFF allowed. The limit
/// counts characters, as the BFF did, so a byte of UTF-8 that continues a
/// character is not counted.
std::optional<std::string>
text_of(const http_request& req, const std::string& name, std::size_t most) {
    auto text = req.get_query_param(name);
    if (ores::http::net::record_routes::characters_of(text) > most)
        return std::nullopt;
    return text;
}

http_response not_a_page() {
    return rr::answer(
        rr::refusal{http_status::bad_request, "invalid-request", "The page asked for is not one."});
}

access_view access_of(const messaging::get_account_roles_response& reply) {
    access_view view;
    for (const auto& held : reply.roles)
        view.roles.push_back({boost::uuids::to_string(held.role.id),
                              held.role.name,
                              held.role.description,
                              held.permission_codes,
                              held.assigned_by,
                              held.assigned_at,
                              held.change_reason_code,
                              held.change_commentary});
    return view;
}

permission_page_view page_of(const messaging::permission_page_response& reply) {
    return {
        reply.area, reply.rows, reply.total_count, reply.areas, reply.area_whole, reply.everything};
}

/// Forward one read, check its outcome, and answer its view as JSON.
template <typename Request, typename Shape>
http_response
answer(const http_request& req, nats_client& session, const Request& msg, Shape shape) {
    try {
        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        return http_response::json(shape(reply));
    } catch (const rr::refusal& r) {
        return rr::answer(r);
    } catch (const std::exception& e) {
        return ores::http::forwarding::unanswered(Request::nats_subject, e);
    }
}

}

/// The holders page, written by hand because a holder with no picture states
/// imageId as null, which the browser requires and a codec member would leave out.
std::string access_read_routes::holders_json(const messaging::role_holders_response& reply) {
    std::string out = "{\"holders\":[";
    for (std::size_t i = 0; i < reply.holders.size(); ++i) {
        const auto& h = reply.holders[i];
        out += (i == 0 ? "" : ",");
        out += "{\"accountId\":" + json::write(h.account_id);
        out += ",\"username\":" + json::write(h.username);
        out += ",\"fullName\":" + json::write(h.full_name);
        out +=
            ",\"imageId\":" + (h.image_id.empty() ? std::string("null") : json::write(h.image_id));
        out += ",\"assignedBy\":" + json::write(h.assigned_by) + "}";
    }
    return out + "],\"totalCount\":" + std::to_string(reply.total_count) + "}";
}

std::string access_read_routes::access_json(const messaging::get_account_roles_response& reply) {
    return json::write(access_of(reply));
}

std::string access_read_routes::page_json(const messaging::permission_page_response& reply) {
    return json::write(page_of(reply));
}

std::string access_read_routes::roles_json(const messaging::role_page_response& reply) {
    role_page_view view;
    for (const auto& r : reply.roles)
        view.roles.push_back({r.id,
                              r.version,
                              r.name,
                              r.description,
                              r.service,
                              r.registration_default,
                              r.requestable,
                              r.permission_count,
                              r.everything});
    view.totalCount = reply.total_count;
    view.serviceHidden = reply.service_hidden;
    view.areas = reply.areas;
    return json::write(view);
}

std::optional<access_read_routes::permission_query>
access_read_routes::read_permission_query(const http_request& req) {
    const auto area = text_of(req, "area", 100);
    const auto search = text_of(req, "search", 200);
    const auto offset = number_of(req, "offset", 0, 0, std::numeric_limits<int>::max());
    const auto limit = number_of(req, "limit", 15, 1, 500);
    if (!area || !search || !offset || !limit)
        return std::nullopt;
    return permission_query{*area, *search, *offset, *limit};
}

std::optional<access_read_routes::roles_query>
access_read_routes::read_roles_query(const http_request& req) {
    const auto role_id = req.get_query_param("roleId");
    const auto search = text_of(req, "search", 200);
    const auto area = text_of(req, "area", 100);
    const auto include_service = flag_of(req, "includeService");
    const auto offset = number_of(req, "offset", 0, 0, std::numeric_limits<int>::max());
    const auto limit = number_of(req, "limit", 15, 1, 500);
    if ((!role_id.empty() && !rr::is_uuid(role_id)) || !search || !area || !include_service ||
        !offset || !limit)
        return std::nullopt;
    return roles_query{role_id, *search, *area, *include_service, *offset, *limit};
}

void access_read_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder,
                         std::string summary,
                         std::function<http_response(const http_request&)> handle) {
        const auto built =
            builder.summary(std::move(summary))
                .tags({"access"})
                .auth_required()
                .handler([handle = std::move(handle)](const http_request& req)
                             -> boost::asio::awaitable<http_response> { co_return handle(req); })
                .build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/me/access"),
        "Read the roles the caller holds",
        [&session](const http_request& req) {
            return answer(req, session, messaging::get_my_roles_request{}, access_json);
        });
    add(router->get("/api/accounts/{accountId}/access"),
        "Read the roles one account holds",
        [&session](const http_request& req) {
            const auto account = req.get_path_param("accountId");
            if (!rr::is_uuid(account))
                return rr::answer(rr::refusal{
                    http_status::not_found, "not-found", "No account has this identifier."});
            return answer(req, session, messaging::get_account_roles_request{account}, access_json);
        });
    add(router->get("/api/me/permissions"),
        "Read a page of what the caller's roles let them do",
        [&session](const http_request& req) {
            const auto query = read_permission_query(req);
            if (!query)
                return not_a_page();
            return answer(req,
                          session,
                          messaging::list_my_permissions_request{
                              query->area, query->search, query->offset, query->limit},
                          page_json);
        });
    add(router->get("/api/accounts/{accountId}/permissions"),
        "Read a page of what one account's roles let it do",
        [&session](const http_request& req) {
            const auto account = req.get_path_param("accountId");
            const auto query = read_permission_query(req);
            if (!rr::is_uuid(account) || !query)
                return not_a_page();
            return answer(req,
                          session,
                          messaging::list_account_permissions_request{
                              account, query->area, query->search, query->offset, query->limit},
                          page_json);
        });
    add(router->get("/api/roles/page"),
        "Read a page of the tenant's roles",
        [&session](const http_request& req) {
            const auto query = read_roles_query(req);
            if (!query)
                return not_a_page();
            return answer(req,
                          session,
                          messaging::list_roles_page_request{query->role_id,
                                                             query->search,
                                                             query->area,
                                                             query->include_service,
                                                             query->offset,
                                                             query->limit},
                          roles_json);
        });
    add(router->get("/api/roles/{roleId}/permissions"),
        "Read a page of the catalogue against what one role grants",
        [&session](const http_request& req) {
            const auto role = req.get_path_param("roleId");
            const auto query = read_permission_query(req);
            const auto unheld = flag_of(req, "includeUnheld");
            if (!rr::is_uuid(role) || !query || !unheld)
                return not_a_page();
            return answer(
                req,
                session,
                messaging::list_role_permissions_page_request{
                    role, query->area, query->search, *unheld, query->offset, query->limit},
                page_json);
        });
    add(router->get("/api/roles/{roleId}/holders"),
        "Read a page of the people who hold one role",
        [&session](const http_request& req) {
            const auto role = req.get_path_param("roleId");
            const auto offset = number_of(req, "offset", 0, 0, std::numeric_limits<int>::max());
            const auto limit = number_of(req, "limit", 15, 1, 500);
            if (!rr::is_uuid(role) || !offset || !limit)
                return not_a_page();
            return answer(req,
                          session,
                          messaging::list_role_holders_request{role, *offset, *limit},
                          holders_json);
        });
}
}
