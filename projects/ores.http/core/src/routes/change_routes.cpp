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
#include "ores.http.core/routes/change_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/stream/change_pump.hpp"
#include <regex>
#include <yyjson.h>

namespace ores::http::server::routes {

using namespace ores::logging;

change_routes::change_routes(std::shared_ptr<stream::change_hub> hub,
                             std::chrono::milliseconds keep_alive)
    : hub_(std::move(hub))
    , keep_alive_(keep_alive) {
    BOOST_LOG_SEV(lg(), debug) << "Change routes initialized";
}

void change_routes::register_routes(std::shared_ptr<http::net::router> router,
                                    std::shared_ptr<http::openapi::endpoint_registry> registry) {

    BOOST_LOG_SEV(lg(), info) << "Registering the change stream";

    auto changes = router->get("/api/v1/changes")
                       .summary("Follow the changes")
                       .description("A stream of change frames, written as they happen")
                       .tags({"changes"})
                       .auth_required()
                       .handler([this](const domain::http_request& req) {
                           return handle_changes(req, false);
                       });
    auto changes_built = changes.build();
    router->add_route(changes_built);
    registry->register_route(changes_built);

    auto events =
        router->get("/api/events")
            .summary("Follow the changes the session watches")
            .description("The change stream, carrying only the collections the session watches")
            .tags({"changes"})
            .auth_required()
            .handler([this](const domain::http_request& req) { return handle_changes(req, true); });
    auto events_built = events.build();
    router->add_route(events_built);
    registry->register_route(events_built);

    auto watch = router->post("/api/events/watch")
                     .summary("State what the session watches")
                     .description("Replaces the collections the session's change stream carries")
                     .tags({"changes"})
                     .auth_required()
                     .handler([this](const domain::http_request& req)
                                  -> boost::asio::awaitable<domain::http_response> {
                         co_return handle_watch(req);
                     });
    auto watch_built = watch.build();
    router->add_route(watch_built);
    registry->register_route(watch_built);

    BOOST_LOG_SEV(lg(), info) << "Change routes registered: 3 endpoints";
}

std::optional<std::unordered_set<std::string>>
change_routes::read_watches(const std::string& body) {
    static const std::regex name_form("^[a-z][a-z0-9_]*$");
    std::unordered_set<std::string> out;
    if (body.find_first_not_of(" \t\r\n") == std::string::npos)
        return out;
    std::unique_ptr<yyjson_doc, decltype(&yyjson_doc_free)> doc(
        yyjson_read(body.data(), body.size(), 0), &yyjson_doc_free);
    if (!doc || !yyjson_is_obj(yyjson_doc_get_root(doc.get())))
        return std::nullopt;
    auto* watches = yyjson_obj_get(yyjson_doc_get_root(doc.get()), "watches");
    if (watches == nullptr)
        return out;
    if (!yyjson_is_arr(watches) || yyjson_arr_size(watches) > 50)
        return std::nullopt;
    std::size_t index = 0, count = 0;
    yyjson_val* item = nullptr;
    yyjson_arr_foreach(watches, index, count, item) {
        const auto name = [&](const char* key) -> std::optional<std::string> {
            auto* v = yyjson_obj_get(item, key);
            if (!yyjson_is_str(v))
                return std::nullopt;
            std::string text(yyjson_get_str(v), yyjson_get_len(v));
            if (!std::regex_match(text, name_form))
                return std::nullopt;
            return text;
        };
        const auto component = yyjson_is_obj(item) ? name("component") : std::nullopt;
        const auto entity = yyjson_is_obj(item) ? name("entity") : std::nullopt;
        if (!component || !entity)
            return std::nullopt;
        out.insert(*component + '.' + *entity);
    }
    return out;
}

namespace {

std::string session_of(const domain::http_request& req) {
    if (!req.authenticated_user)
        return {};
    return req.authenticated_user->session_id.value_or(req.authenticated_user->subject);
}

}

domain::http_response change_routes::handle_watch(const domain::http_request& req) {
    auto collections = read_watches(req.body);
    if (!collections)
        return http::net::record_routes::answer({domain::http_status::bad_request,
                                                 "invalid-request",
                                                 "The watch set is not one the browser sends."});
    if (!watches_.watch(session_of(req), std::move(*collections)))
        BOOST_LOG_SEV(lg(), debug) << "A watch set arrived for a session with no stream open";
    return domain::http_response::json(R"({"ok":true})");
}

boost::asio::awaitable<domain::http_response>
change_routes::handle_changes(const domain::http_request& req, bool watched) {
    BOOST_LOG_SEV(lg(), debug) << "Opening a change stream";

    domain::http_response resp;
    resp.status = domain::http_status::ok;
    resp.content_type = "text/event-stream";
    /*
     * A proxy that buffers would hold every frame until the stream ended, and a
     * cache would answer a later caller with frames from an earlier session.
     */
    resp.headers["Cache-Control"] = "no-cache";
    resp.headers["X-Accel-Buffering"] = "no";
    /*
     * A stream hears the caller's own tenant and the system tenant, and nothing
     * else. The frame states which tenant it belongs to, so the hub can decide
     * before it queues anything: a session must not be able to read another
     * tenant's changes, whatever the stream is called.
     */
    const auto tenant = req.authenticated_user ?
                            req.authenticated_user->tenant_id.value_or(std::string{}) :
                            std::string{};

    /*
     * A reconnecting browser states where it stopped. Inside the hub's short
     * retention it is given what it missed; outside it, or naming an id another
     * process issued or no frame at all, it is told to resync rather than left with
     * a silent gap it cannot see.
     */
    std::optional<stream::change_hub::resume_point> resumed;
    if (const auto seen = req.get_header("last-event-id"); !seen.empty()) {
        resumed = stream::change_hub::read_id(seen);
        if (!resumed) {
            BOOST_LOG_SEV(lg(), warn) << "Last-Event-ID is not a frame id: " << seen;
            // An id no frame of this process could have carried is answered as a gap.
            resumed = stream::change_hub::resume_point{};
        }
    }

    /*
     * A member hears their own party and the tenant-wide changes; an administrator
     * hears every party of the tenant. The party travels in the token, so the
     * gateway filters with what the caller already proved rather than asking IAM.
     */
    std::string party;
    auto sees_all_parties = false;
    if (req.authenticated_user) {
        party = req.authenticated_user->party_id.value_or(std::string{});
        for (const auto& role : req.authenticated_user->roles) {
            if (role == "TenantAdmin" || role == "SuperAdmin")
                sees_all_parties = true;
        }
    }

    const stream::stream_scope scope{.tenant = tenant,
                                     .party = party,
                                     .sees_all_parties = sees_all_parties,
                                     .watching =
                                         watched ? watches_.open(session_of(req)) : nullptr};
    const auto connected = "event: connected\ndata: {\"at\":\"" +
                           stream::instant_text(std::chrono::system_clock::now()) + "\"}\n\n";
    resp.body_awaitable = std::make_shared<stream::change_stream_source>(
        hub_->subscribe(scope, resumed), keep_alive_, connected);
    co_return resp;
}

}
