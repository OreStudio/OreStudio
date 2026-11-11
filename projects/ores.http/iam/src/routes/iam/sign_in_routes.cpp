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
#include "ores.http/routes/iam/sign_in_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/access_read_routes.hpp"
#include "ores.http/routes/iam/account_view.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/login_info_protocol.hpp"
#include "ores.iam.api/messaging/session_operations_protocol.hpp"
#include "ores.iam.api/messaging/session_protocol.hpp"
#include <boost/algorithm/string/predicate.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <charconv>
#include <cstdint>
#include <functional>
#include <limits>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace messaging = ores::iam::messaging;
using ores::http::forwarding::guarded;
using ores::http::net::record_routes::refused;

namespace {

std::optional<std::uint32_t> whole_number(const http_request& req,
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

/// An offset of zero or more and a limit of one to @p most, @p fallback when
/// the query leaves it out.
std::optional<std::pair<std::uint32_t, std::uint32_t>>
page_of(const http_request& req, std::uint32_t fallback, std::uint32_t most) {
    const auto offset =
        whole_number(req, "offset", 0, 0, std::numeric_limits<std::uint32_t>::max());
    const auto limit = whole_number(req, "limit", fallback, 1, most);
    if (!offset || !limit)
        return std::nullopt;
    return std::pair{*offset, *limit};
}

http_response not_whole_numbers() {
    return refused(http_status::bad_request,
                   "invalid-request",
                   "The page offset and limit must be whole numbers.");
}

std::string sessions_page(const std::vector<ores::iam::domain::session>& sessions,
                          std::uint64_t total) {
    return "{\"sessions\":" + account_view::json_of(sessions) +
           ",\"totalCount\":" + std::to_string(total) + "}";
}


std::string login_info_answer(const std::optional<ores::iam::domain::login_info>& info) {
    return "{\"loginInfo\":" + account_view::json_of(info) + "}";
}
}

std::optional<std::pair<std::uint32_t, std::uint32_t>>
sign_in_routes::read_sign_ins_page(const http_request& req) {
    return page_of(req, 20, 200);
}

std::optional<std::pair<std::uint32_t, std::uint32_t>>
sign_in_routes::read_page(const http_request& req) {
    return page_of(req, 100, 1000);
}

std::optional<std::string>
sign_in_routes::sign_ins_json(const http_request& req,
                              nats_client& session,
                              const std::string& username,
                              std::pair<std::uint32_t, std::uint32_t> page) {
    const auto read = ores::http::forwarding::forward(
        req, session, messaging::get_account_request{messaging::account_key{username}});
    if (!read.account)
        return std::nullopt;
    const auto& account = *read.account;
    const auto info = ores::http::forwarding::forward(
        req, session, messaging::get_login_info_request{messaging::login_info_key{account.id}});
    messaging::list_sessions_request list;
    list.offset = page.first;
    list.limit = page.second;
    list.order.field = "start_time";
    list.order.descending = true;
    list.filter = messaging::sessions_filter{account.id, std::nullopt};
    const auto sessions = ores::http::forwarding::forward(req, session, list);
    return "{\"account\":" + account_view::json_of(account) +
           ",\"loginInfo\":" + account_view::json_of(info.login_info) +
           ",\"sessions\":" + account_view::json_of(sessions.sessions) +
           ",\"totalCount\":" + std::to_string(sessions.total) + "}";
}

void sign_in_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder,
                         std::string summary,
                         std::function<http_response(const http_request&)> handle) {
        const auto built =
            builder.summary(std::move(summary))
                .tags({"auth"})
                .auth_required()
                .handler([handle = std::move(handle)](const http_request& req)
                             -> boost::asio::awaitable<http_response> { co_return handle(req); })
                .build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/login-info"),
        "Read a page of the tenant's sign-in records",
        [&session](const http_request& req) {
            const auto page = read_page(req);
            if (!page)
                return not_whole_numbers();
            return guarded<messaging::list_login_info_request>([&] {
                messaging::list_login_info_request list;
                list.offset = page->first;
                list.limit = page->second;
                const auto reply = ores::http::forwarding::forward(req, session, list);
                std::string out = "{\"loginInfo\":[";
                for (std::size_t i = 0; i < reply.login_info.size(); ++i)
                    out += (i == 0 ? "" : ",") + account_view::json_of(reply.login_info[i]);
                return http_response::json(out + "],\"totalCount\":" + std::to_string(reply.total) +
                                           "}");
            });
        });

    add(router->get("/api/login-info/{accountId}"),
        "Read one account's sign-in record",
        [&session](const http_request& req) {
            const auto account = req.get_path_param("accountId");
            if (!access_read_routes::is_uuid(account))
                return refused(
                    http_status::bad_request, "invalid-request", "An account id is required.");
            const auto own =
                req.authenticated_user && boost::iequals(req.authenticated_user->subject, account);
            if (own)
                return guarded<messaging::get_my_login_info_request>([&] {
                    const auto reply = ores::http::forwarding::forward(
                        req, session, messaging::get_my_login_info_request{});
                    return http_response::json(login_info_answer(reply.login_info));
                });
            return guarded<messaging::get_login_info_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req,
                    session,
                    messaging::get_login_info_request{
                        messaging::login_info_key{boost::uuids::string_generator()(account)}});
                return http_response::json(login_info_answer(reply.login_info));
            });
        });

    add(router->get("/api/sessions"),
        "Read a page of the tenant's sessions",
        [&session](const http_request& req) {
            const auto page = read_page(req);
            if (!page)
                return not_whole_numbers();
            return guarded<messaging::list_sessions_request>([&] {
                messaging::list_sessions_request list;
                list.offset = page->first;
                list.limit = page->second;
                const auto reply = ores::http::forwarding::forward(req, session, list);
                return http_response::json(sessions_page(reply.sessions, reply.total));
            });
        });

    add(router->get("/api/sessions/active"),
        "Read the tenant's open sessions",
        [&session](const http_request& req) {
            return guarded<messaging::get_active_sessions_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req, session, messaging::get_active_sessions_request{});
                return http_response::json(
                    "{\"sessions\":" + account_view::json_of(reply.sessions) + "}");
            });
        });

    add(router->get("/api/me/sessions"),
        "Read the caller's own open sessions",
        [&session](const http_request& req) {
            return guarded<messaging::get_my_sessions_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req, session, messaging::get_my_sessions_request{});
                return http_response::json(
                    "{\"sessions\":" + account_view::json_of(reply.sessions) + "}");
            });
        });

    add(router->get("/api/accounts/{username}/sign-ins"),
        "Read one account with its sign-in state and its sessions, newest first",
        [&session](const http_request& req) {
            const auto page = read_sign_ins_page(req);
            if (!page)
                return not_whole_numbers();
            const auto username = req.get_path_param("username");
            return guarded<messaging::list_sessions_request>([&] {
                const auto answer = sign_ins_json(req, session, username, *page);
                if (!answer)
                    return refused(
                        http_status::not_found, "not-found", "No account has this username.");
                return http_response::json(*answer);
            });
        });
}

}
