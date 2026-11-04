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

    auto changes =
        router->get("/api/v1/changes")
            .summary("Follow the changes")
            .description("A stream of change frames, written as they happen")
            .tags({"changes"})
            .auth_required()
            .handler([this](const domain::http_request& req) { return handle_changes(req); });
    auto changes_built = changes.build();
    router->add_route(changes_built);
    registry->register_route(changes_built);

    BOOST_LOG_SEV(lg(), info) << "Change routes registered: 1 endpoint";
}

boost::asio::awaitable<domain::http_response>
change_routes::handle_changes(const domain::http_request& req) {
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
     * retention it is given what it missed; outside it, or naming an id this
     * process never issued, it is told to resync rather than left with a silent
     * gap it cannot see.
     */
    std::optional<stream::change_hub::resume_point> resumed;
    if (const auto seen = req.get_header("last-event-id"); !seen.empty()) {
        try {
            resumed = stream::change_hub::resume_point{
                .last_seen = static_cast<std::uint64_t>(std::stoull(seen))};
        } catch (const std::exception&) {
            BOOST_LOG_SEV(lg(), warn) << "Last-Event-ID is not a frame id: " << seen;
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

    const stream::stream_scope scope{
        .tenant = tenant, .party = party, .sees_all_parties = sees_all_parties};
    resp.body_awaitable = std::make_shared<stream::change_stream_source>(
        hub_->subscribe(scope, resumed), keep_alive_);
    co_return resp;
}

}
