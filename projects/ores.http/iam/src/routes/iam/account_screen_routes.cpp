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
#include "ores.http/routes/iam/account_screen_routes.hpp"
#include "ores.assets.api/messaging/image_protocol.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/image_answer.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/access_screen_shapes.hpp"
#include "ores.http/routes/iam/account_view.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <charconv>
#include <cstdint>
#include <exception>
#include <functional>
#include <limits>
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
namespace assets = ores::assets::messaging;
namespace rr = ores::http::net::record_routes;
using ores::http::net::record_routes::characters_of;
using ores::http::forwarding::guarded;
using ores::http::net::record_routes::refused;

namespace {

std::optional<std::uint32_t> count_of(const http_request& req,
                                      const std::string& name,
                                      std::uint32_t fallback,
                                      std::uint32_t least,
                                      std::uint32_t most) {
    const auto text = req.get_query_param(name);
    if (text.empty())
        return fallback;
    std::uint32_t value = 0;
    const auto [end, failed] = std::from_chars(text.data(), text.data() + text.size(), value);
    if (failed != std::errc{} || end != text.data() + text.size() || value < least || value > most)
        return std::nullopt;
    return value;
}


/// Whether the path names the caller's own account, which the self read answers.
bool is_own_account(const http_request& req, const std::string& username) {
    const auto& claims = req.authenticated_user;
    return claims && claims->username && *claims->username == username;
}

/// One account by username: the caller's own by the self read, which needs no
/// account permission, any other by the account read. A failed self read is
/// refused as the BFF refused a failed operation.
std::optional<ores::iam::domain::account>
account_or_own(const http_request& req, nats_client& session, const std::string& username) {
    if (is_own_account(req, username)) {
        const auto reply =
            ores::http::forwarding::forward(req, session, messaging::get_my_account_request{});
        if (reply.result.outcome != ores::utility::domain::outcome::ok)
            throw rr::refusal{http_status::conflict, "invalid-request", reply.result.message};
        return reply.account;
    }
    return ores::http::forwarding::forward(
               req, session, messaging::get_account_request{messaging::account_key{username}})
        .account;
}

}

std::optional<messaging::list_accounts_request>
account_screen_routes::read_page_query(const http_request& req) {
    const auto offset = count_of(req, "offset", 0, 0, std::numeric_limits<std::uint32_t>::max());
    const auto limit = count_of(req, "limit", 100, 1, 1000);
    const auto search = rr::trimmed(req.get_query_param("search"));
    const auto sort = req.get_query_param("sort");
    const auto descending = req.get_query_param("descending");
    if (!offset || !limit || characters_of(search) > 256 ||
        (sort != "" && sort != "username" && sort != "full_name") ||
        (descending != "" && descending != "true" && descending != "false"))
        return std::nullopt;
    messaging::list_accounts_request out;
    out.offset = *offset;
    out.limit = *limit;
    out.order.field = sort;
    out.order.descending = descending == "true";
    if (!search.empty())
        out.filter = messaging::accounts_filter{std::nullopt, search};
    return out;
}

std::string account_screen_routes::tree_json(const messaging::get_reporting_tree_response& reply) {
    const auto or_null = [](const std::string& id) {
        return id.empty() ? std::string("null") : json::write(id);
    };
    std::string out = "{\"unrooted\":" + std::to_string(reply.unrooted) + ",\"nodes\":[";
    for (std::size_t i = 0; i < reply.nodes.size(); ++i) {
        const auto& n = reply.nodes[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"accountId\":" + json::write(n.account_id);
        out += ",\"username\":" + json::write(n.username);
        out += ",\"fullName\":" + json::write(n.full_name);
        out += ",\"jobTitle\":" + json::write(n.job_title);
        out += ",\"accountType\":" + json::write(n.account_type.empty() ? "user" : n.account_type);
        out += ",\"imageId\":" + or_null(n.image_id);
        out += ",\"reportsToAccountId\":" + or_null(n.reports_to_account_id);
        out +=
            ",\"reportsOutsideScope\":" + std::string(n.reports_outside_scope ? "true" : "false");
        out += ",\"depth\":" + std::to_string(n.depth);
        out += ",\"directReports\":" + std::to_string(n.direct_reports);
        out += ",\"partyIds\":" + json::write(n.party_ids) + "}";
    }
    out += "],\"parties\":[";
    for (std::size_t i = 0; i < reply.parties.size(); ++i) {
        const auto& p = reply.parties[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"partyId\":" + json::write(p.party_id);
        out += ",\"name\":" + json::write(p.name);
        out += ",\"shortCode\":" + json::write(p.short_code);
        out += ",\"parentPartyId\":" + or_null(p.parent_party_id) + "}";
    }
    return out + "]}";
}

http_response account_screen_routes::picture_of(const ores::assets::domain::image& image) {
    return ores::http::net::image_answer(image.mime_type,
                                         std::string(image.data.begin(), image.data.end()));
}

std::string account_screen_routes::page_json(const messaging::list_accounts_response& reply) {
    std::string out = "{\"accounts\":[";
    for (std::size_t i = 0; i < reply.accounts.size(); ++i)
        out += (i == 0 ? "" : ",") + account_view::json_of(reply.accounts[i]);
    return out + "],\"totalCount\":" + std::to_string(reply.total) + "}";
}

void account_screen_routes::register_routes(
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

    add(router->get("/api/accounts"),
        "Read a page of the tenant's accounts",
        [&session](const http_request& req) {
            const auto page = read_page_query(req);
            if (!page)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "A page of people names an offset, a limit of 1 to 1000, a search, "
                               "and an order by username or name.");
            return guarded<messaging::list_accounts_request>([&] {
                return http_response::json(
                    page_json(ores::http::forwarding::forward(req, session, *page)));
            });
        });

    add(router->get("/api/accounts/{username}"),
        "Read one account by username",
        [&session](const http_request& req) {
            const auto username = req.get_path_param("username");
            if (username.empty())
                return refused(
                    http_status::bad_request, "invalid-request", "A username is required.");
            return guarded<messaging::get_account_request>([&] {
                return http_response::json(
                    "{\"account\":" +
                    account_view::json_of(account_or_own(req, session, username)) + "}");
            });
        });

    add(router->post("/api/me/default-party"),
        "Set or clear the party quick sign-in uses",
        [&session](const http_request& req) {
            const auto body = json::read_expected<default_party_body>(req.body);
            if (!body)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "A partyId is required. Send an empty string to clear the default.");
            const messaging::set_my_default_party_request msg{body->partyId};
            return guarded<messaging::set_my_default_party_request>([&] {
                const auto reply = ores::http::forwarding::forward(req, session, msg);
                if (!reply.success)
                    return refused(http_status::conflict, "invalid-request", reply.message);
                return http_response::json("{\"defaultPartyId\":" + json::write(body->partyId) +
                                           "}");
            });
        });

    add(router->get("/api/accounts/{username}/picture"),
        "Read the picture of one account",
        [&session](const http_request& req) {
            const auto username = req.get_path_param("username");
            return guarded<assets::list_images_request>([&] {
                const auto account = account_or_own(req, session, username);
                if (!account || !account->image_id || account->image_id->is_nil())
                    return refused(
                        http_status::not_found, "not-found", "This account has no picture.");
                assets::list_images_request list;
                list.limit = 1;
                list.filter = assets::images_filter{
                    std::vector<boost::uuids::uuid>{*account->image_id}, std::nullopt};
                const auto reply = ores::http::forwarding::forward(req, session, list);
                if (reply.result.outcome != ores::utility::domain::outcome::ok)
                    return refused(http_status::conflict, "invalid-request", reply.result.message);
                if (reply.images.empty())
                    return refused(
                        http_status::not_found, "not-found", "This account has no picture.");
                return picture_of(reply.images.front());
            });
        });

    add(router->put("/api/accounts/{accountId}/reporting-line"),
        "Set or clear who one account reports to",
        [&session](const http_request& req) {
            const auto body = json::read_expected<reporting_line_body>(req.body);
            if (!body)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "A reportsToAccountId is required. Send an empty string to clear "
                               "the line.");
            return guarded<messaging::set_reporting_line_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req,
                    session,
                    messaging::set_reporting_line_request{req.get_path_param("accountId"),
                                                          body->reportsToAccountId,
                                                          body->expectedVersion.value_or(""),
                                                          body->reasonCode.value_or(""),
                                                          body->commentary.value_or("")});
                if (reply.result.outcome != ores::utility::domain::outcome::ok)
                    return refused(http_status::conflict, "invalid-request", reply.result.message);
                if (!reply.account)
                    return refused(http_status::conflict,
                                   "invalid-request",
                                   "The write answered without the account it wrote.");
                return http_response::json(account_view::json_of(*reply.account));
            });
        });

    add(router->get("/api/reporting-tree"),
        "Read the tenant's reporting shape, or one account's branch of it",
        [&session](const http_request& req) {
            return guarded<messaging::get_reporting_tree_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req,
                    session,
                    messaging::get_reporting_tree_request{req.get_query_param("root")});
                if (reply.result.outcome != ores::utility::domain::outcome::ok)
                    return refused(http_status::conflict, "invalid-request", reply.result.message);
                return http_response::json(tree_json(reply));
            });
        });
}

}
